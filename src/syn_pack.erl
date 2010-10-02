-module(syn_pack).
-export([gen/3, gen/4, silence/1, silence_sec/2]).
-export([length/1, length_ms/2, pcm/1]).
-export([map/2]).

length({L, _}) -> L.
pcm({_, B}) -> B.

length_ms({Len, _Pcm}, SampleRate) ->
  round((Len / SampleRate) * 1000).

silence(SampleRate) ->
  silence_sec(SampleRate, 1).

silence_sec(SampleRate, Len) ->
  Bitlen = round(SampleRate * 32 * Len),
  {SampleRate * Len, <<0:Bitlen/integer>>}.

-compile({inline, gen/3}).
gen(Instr, Len, SampleRate) -> gen(Instr, 0.0, Len, SampleRate).

-compile({inline, gen/4}).
gen(Instr, Count, Len, SampleRate) ->
  Delta = 1 / SampleRate,
  {Len, gen_pcm(Instr, Count, Count + Len, Delta)}.

map(Fun, {Len, Pcm}) ->
  {Len, << <<(Fun(V)):32/float-little>> || <<V:32/float-little>> <= Pcm >>}.

%% ------------------------------------------------------------
%% Helper Functions
-compile({inline, gen_pcm/4}).
gen_pcm(Instr, From, To, Delta) ->
  gen_pcm(Instr, From, To, Delta, <<>>).

-compile({inline, gen_pcm/5}).
gen_pcm(_Instr, From, To, _Delta, Res) when From >= To ->
  Res;
gen_pcm(Instr, From, To, Delta, Res) ->
  gen_pcm(Instr, From + Delta, To, Delta,
    <<Res/binary, (syn_instr:run_instr(From, Instr)):32/float-little>>).
