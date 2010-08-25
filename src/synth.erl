-module(synth).
-export([start/0, start/1, start/2, stop/1, set_instr/2]).

-record(synth_engine, {renderer, sink}).

-define(DEFAULT_ST, aplay).
-define(DEFAULT_SR, 44000).
-define(DEFAULT_PL, 1).

start() ->
  start(?DEFAULT_ST).
start(SinkType) ->
  start(SinkType, ?DEFAULT_SR). 
start(SinkType, SampleRate) ->
  Renderer = syn_gen:start(syn_instr:null()),
  {ok, Sink} = syn_sink:start(SinkType, SampleRate, Renderer, ?DEFAULT_PL),
  #synth_engine{renderer = Renderer, sink = Sink}.

stop(#synth_engine{renderer = Ren, sink = Sink}) ->
  syn_sink:stop(Sink),
  syn_gen:stop(Ren).

set_instr(#synth_engine{renderer = Ren}, Instr) ->  
  syn_gen:set_instr(Ren, Instr).
