-module(syn_gen).
-export([send_packet/3, send_packet/4, get_packet/3, stop/1]).
-export([start/1, set_instr/2, render_loop/2]).

get_packet(Generator, SampleRate, Len) ->
  send_packet(Generator, SampleRate, Len),
  receive
    {Generator, packet, Pack} -> Pack
  end.

send_packet(Generator, SampleRate, Len) ->
  send_packet(Generator, self(), SampleRate, Len).
send_packet(Generator, To, SampleRate, Len) ->
  Generator ! {To, send_packet, SampleRate, Len},
  ok.

start(Instr) ->
  spawn(?MODULE, render_loop, [Instr, 0.0]).

stop(Generator) when is_pid(Generator) ->
  Generator ! stop,
  ok.

%% renderer
set_instr(Renderer, Fun) ->
  Renderer ! {set_instr, Fun},
  ok.

render_loop(Instr, Time) ->
  receive
    {Slave, send_packet, SampleRate, Len} when is_pid(Slave) ->
      Pack = syn_pack:gen(Instr, Time, Len, SampleRate),
      NewTime = Time + Len,
      Slave ! {self(), packet, Pack},
      render_loop(Instr, NewTime);
    {set_instr, F} ->
      ?MODULE:render_loop(F, Time);
    stop ->
      ok
  end.
