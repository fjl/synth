-module(syn_sink).
-export([start/4, stop/1, play/2]).

%% @doc
%% Start a sink process using the specified backend and sample rate.
%% At the moment, the only backend is `aplay'.
%% @spec start(Backend::atom()) -> {ok, pid()} | {error, unknown_backend} | {error, Msg}
start(aplay, SampleRate, Gen, PrefLen) ->
  aplay_sink(SampleRate, Gen, PrefLen);
start(_, _SR, _Gen, _PrefLen) ->
  {error, unknown_backend}.

%% @doc
%% Schedules a packet for playing in a running sink.
%% @spec play(Sink::pid(), Packet) -> ok
%%       Packet = syn_pack:packet()
play(Sink, Packet) ->
  Sink ! {play, Packet},
  ok.

%% @doc
%% Stop a running sink.
%% @spec stop(Sink::pid()) -> ok
stop(Sink) ->
  Sink ! stop,
  ok.

%% -- APLAY --------------------------------------------------------------------------
-record(apState, {aplay, silence, sampleRate, preflen, gen, timer, next}).

%% @private
%% a sink that uses aplay (from alsautils)
%% for sound output
aplay_sink(SampleRate, Gen, Preflen) ->
  Prog = os:find_executable("aplay"),
  Port = open_port({spawn_executable, Prog},
    [{args, ["-f", "FLOAT_LE", "-r", integer_to_list(SampleRate)]},
      binary,stream,stderr_to_stdout]),
  Silence = syn_pack:silence_sec(SampleRate, 0.1),
  Initial = syn_gen:get_packet(Gen, SampleRate, Preflen),
  Sink = spawn(fun () ->
        link(Gen),
        {ok, Timer} = timer:send_interval(Preflen * 1000, send_packet_now),
        aplaylp(#apState{aplay = Port,
                         silence = Silence,
                         next = Initial,
                         sampleRate = SampleRate,
                         preflen = Preflen,
                         gen = Gen,
                         timer = Timer})
                 end),
  Sink ! send_packet_now,
  port_connect(Port, Sink),
  unlink(Port), 
  {ok, Sink}.

%% @private
%% aplay sink main loop
aplaylp(ST = #apState{aplay = Aplay, sampleRate = SampleRate, preflen = Preflen,
                      gen = Gen, next = Next, timer = Timer}) ->
  receive
    {Gen, packet, Pack} ->
      aplaylp(ST#apState{next = Pack});
    send_packet_now ->
      syn_gen:send_packet(Gen, SampleRate, Preflen), % request a new one
      case Next of
        undefined ->
          io:format("warning: packet is delayed.~n"),
          timer:cancel(Timer),
          aplaylp_wait_for_packet(ST);
        Next ->
          port_command(Aplay, syn_pack:pcm(Next)),
          aplaylp(ST#apState{next = undefined})
      end;
    {Aplay, {data, _}} -> aplaylp(ST); %% ignore output
    stop ->
      syn_gen:stop(Gen),
      port_close(Aplay)
  end.

%% @private
%% this function is entered when a packet is delayed.
%% it will feed silence to the port until a packet arrives. 
aplaylp_wait_for_packet(ST = #apState{aplay = Aplay, gen = Gen, 
                                      preflen = Preflen, silence = Silence, 
                                      sampleRate = SampleRate}) ->
  port_command(Aplay, syn_pack:pcm(Silence)),
  receive
    {Gen, packet, Pack} ->
      self() ! send_packet_now,
      {ok, NewTimer} = 
        timer:send_interval(round((Preflen / SampleRate) * 1000), send_packet_now),
      aplaylp(ST#apState{next = Pack,
          timer = NewTimer})
  after
    syn_pack:length_ms(Silence, SampleRate) ->
      aplaylp_wait_for_packet(ST)
  end.
