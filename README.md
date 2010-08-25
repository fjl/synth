          .                         .                         .
       .     .                   .     .                   .     .
     .    |    .               .         .               .    |    .
    .     |     .             .           .             .     |     .
     -----|----- . --------- . ----------- . --------- . -----|----- .
          |       .         .               .         .       |
          |         .     .                   .     .         |
                       .                         .

A pure-erlang software synthesizer (or so).
Uses aplay for output, so it only works on Linux.

Doesn't that sound convincing?

# Pitchfork
    
    $ make shell
    <build output, eshell booting...>
    1> Syn = synth:start().
    2> synth:set_instr(Syn, syn_instr:sin(440.0)).
