          .                         .                         .
       .     .                   .     .                   .     .
     .    |    .               .         .               .    |    .
    .     |     .             .           .             .     |     .
     -----|----- . --------- . ----------- . --------- . -----|----- .
          |       .         .               .         .       |
          |         .     .                   .     .         |
                       .                         .

A pure-erlang software synthesizer (or so). Incomplete. Uses aplay for output. Only works on Linux.

Doesn't that sound convincing?

## Pitchfork
    
    $ make shell
    <build output, eshell booting...>
    1> Syn = synth:start().
    2> rr("include/*").
    <blah blah>
    2> synth:set_instr(Syn, #sin{freq = 440.0}).
