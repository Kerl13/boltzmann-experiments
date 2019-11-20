(** Random numbers generator. *)

(** This module is a wrapper around OCaml's {!Random} module. The generation of
  random bits being an important bottleneck of Boltzmann generation, we
  reimplement the boolean generator to use all the random bits produced by
  {!Random.bits}, unlike OCaml's {!Random.bool} which generates 30 random bits
  at each call but only uses one of them. *)

(* A integer to store up to 30 bits. *)
let buffer = ref 0

(* The number of random bits left in the buffer. *)
let pos = ref 0

(** Like {!Random.bool} but save some random bits. *)
let bool () =
  if !pos = 0 then begin
    pos := 30;
    buffer := Random.bits ()
  end;
  let b = (!buffer land 1) = 1 in
  buffer := !buffer lsr 1;
  decr pos;
  b

(** Return the state of the RNG. *)
let get_state () =
  Random.get_state (), !buffer, !pos

(** Set the state of the RNG. *)
let set_state state =
  let random_state, buf, p = state in
  Random.set_state random_state;
  buffer := buf;
  pos := p

(** Initialise the generator, using the argument as a seed. *)
let init = Random.init
