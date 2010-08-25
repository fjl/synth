-module(syn_pack).
-export([gen/3, gen/4, silence/1, silence_sec/2]).
-export([length/1, length_ms/2, pcm/1]). 
-export([map/2]).

%% @type packet() = {SampleRate, PCM}
%%       SampleRate = integer()
%%       PCM = binary()

%% @doc Get length of packet in samples.
length({L, _}) -> L.
%% @doc Get PCM data of packet.
pcm({_, B}) -> B.

%% @doc Get length of a packet in milliseconds relative to sample rate.
length_ms({Len, _Pcm}, SampleRate) ->
  round((Len / SampleRate) * 1000).

%% @doc Generate a one second sample of silence.
silence(SampleRate) ->
  silence_sec(SampleRate, 1).

silence_sec(SampleRate, Len) ->
  Bitlen = round(SampleRate * 32 * Len),
  {SampleRate * Len, <<0:Bitlen/integer>>}.

%% @doc
%% Generate a packet of specified length (in samples)
%% using the supplied function.
%% @equiv gen(F, 0, Length, SampleRate)
-compile({inline, gen/3}).
gen(F, Len, SampleRate) -> gen(F, 0.0, Len, SampleRate).

%% Generate a packet of specified length (in samples)
%% using the supplied function. Starting at Count.
%% @spec gen(Function::function(), Count::integer(), Length::integer()) -> packet()
-compile({inline, gen/4}).
gen(Fun, Count, Len, SampleRate) ->
  Delta = 1 / SampleRate,
  {Len, gen_pcm(Fun, Count, Count + Len, Delta)}.

%% @doc
%% Map a function over the pcm values of a packet.
%% Produces a new packet of the same length.
map(Fun, {Len, Pcm}) ->
  {Len, << <<(Fun(V)):32/float-little>> || <<V:32/float-little>> <= Pcm >>}.

%% ------------------------------------------------------------
%% Helper Functions
%% @private
-compile({inline, gen_pcm/4}).
gen_pcm(Fun, From, To, Delta) ->
  gen_pcm(Fun, From, To, Delta, <<>>). 

%% @private
-compile({inline, gen_pcm/5}).
gen_pcm(_Fun, From, To, _Delta, Res) when From >= To ->
  Res;
gen_pcm(Fun, From, To, Delta, Res) ->
  gen_pcm(Fun, From + Delta, To, Delta, 
    <<Res/binary, (Fun(From)):32/float-little>>).

