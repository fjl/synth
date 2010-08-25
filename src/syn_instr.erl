-module(syn_instr).
-export([null/0, sin/1, square/1, noise/0]). 

-define(PI, 3.14159265).

null() ->
  fun (_T) -> 0.0 end.

noise() ->
  fun (_T) -> random:uniform() end.

sin(Freq) when is_float(Freq) ->
  fun (T) when is_float(T) -> 
      math:sin(Freq * T * 2 * ?PI) 
  end.

square(Freq) when is_float(Freq) ->
  fun (T) when is_float(T) ->
      sign(math:sin(Freq * T * 2 * ?PI))
  end.
    
-compile({inline, sign/1}).
sign(X) when is_float(X), X >  0.0  -> 1.0;
sign(X) when is_float(X), X == 0.0  -> 0.0;
sign(X) when is_float(X)            -> -1.0.
