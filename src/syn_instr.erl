-module(syn_instr).
-export([run_instr/2]).

-include("syn_records.hrl").

-define(PI, 3.14159265).

%% what it should look like
%% #sin{freq = #sin{freq = 440.0, amp = 1.0}, amp = #pulse{freq = 200.0}}

run_instr(_T, N) when is_float(N) ->
  N;
run_instr(T, #sin{freq = Freq, amp = A}) ->
  run_instr(T, A) * math:sin(run_instr(T, Freq) * T * 2 * ?PI);
run_instr(T, #square{freq = Freq, amp = A}) ->
  run_instr(T, A) * sign(math:sin(run_instr(T, Freq) * T * 2 * ?PI));
run_instr(T, #noise{amp = A}) ->
  run_instr(T, A) * random:uniform().

-compile({inline, sign/1}).
sign(X) when is_float(X), X >  0.0  -> 1.0;
sign(X) when is_float(X), X == 0.0  -> 0.0;
sign(X) when is_float(X)            -> -1.0.
