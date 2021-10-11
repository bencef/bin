module Ffmpeg: sig

  type 'a m

  type pix_fmt =
    | Yuv420p

  type color = {r: int; g: int; b: int}

  module Source: sig
    type handle
    (** abstract type representing a source for use as parameter
        to builder functions *)

    val file: string -> handle m
    (** [file path] creates a new source object representing an input file *)

    val v4l2: ?frame_rate:int -> ?pix_fmt:pix_fmt -> string -> handle m
    (** [v4l2 ?frame_rate ?pix_fmt device] creates a new source representing a
        v4l2 device.  Example value for the device is [/dev/video0] *)
  end

  module Filter: sig
    val color: color -> Source.handle m
    (** [color builder color_value] creates a source representing a picture of
        that color. *)
  end

  val pure: 'a -> 'a m

  val bind: 'a m -> ('a -> 'b m) -> 'b m

  val ignore: 'a m -> unit m

  val to_string_array: unit m -> string array
  (** [to_string_array builder] returns an array of parameters that can be
      passed to the ffmpeg command. *)

  module Syntax: sig
    val ( let* ): 'a m -> ('a -> 'b m) -> 'b m
  end
end =
struct
  type t = { inputs: string array list
           ; pipeline: string array
           ; outputs: string array }

  type 'a m = t -> 'a * t

  type pix_fmt =
    | Yuv420p

  type color = {r: int; g: int; b: int}

  module Source =
  struct
    type handle = string

    let add_source builder params =
      let handle = builder.inputs |> List.length |> string_of_int in
      let inputs = params :: builder.inputs in
      (handle, { builder with inputs })

    let string_of_pix_fmt = function
      | Yuv420p -> "yuv420p"

    let file path =
      let params = [| "-i"; path |] in
      fun builder -> add_source builder params

    let v4l2 ?frame_rate ?pix_fmt path =
      let add_opt value transform template_fun =
        value
        |> Option.map transform
        |> Option.map template_fun
        |> Option.value ~default:[||] in
      let params =
        Array.concat [
          [| "-f"; "v4l2"|];
          add_opt frame_rate string_of_int (fun rate -> [| "-framerate"; rate |]);
          add_opt pix_fmt string_of_pix_fmt (fun pix_fmt ->
              [| "-input_format"; "rawvideo"; "-pix_fmt"; pix_fmt |]);
          [|"-i"; path |]
        ] in
      fun builder -> add_source builder params
  end

  module Filter =
  struct
    let color color =
      (* TODO: implement me *)
      fun builder -> "f1", builder
  end

  let to_string_array steps =
    let empty_builder =
      let inputs = [] in
      let pipeline = [||] in
      let outputs = [||] in
      { inputs; pipeline; outputs } in
    let action = fun builder ->
      [ builder.inputs |> List.rev |> Array.concat
      ; builder.pipeline
      ; builder.outputs ]
      |> Array.concat in
    let (_, builder) = steps empty_builder in
    action builder

  let pure v = fun builder -> v, builder

  let bind ma mf = fun builder ->
    let (a, builder') = ma builder in
    mf a builder'

  let map f ma = fun builder ->
    let (a, builder') = ma builder in
    (f a, builder')

  let ignore ma = map ignore ma

  module Syntax =
  struct
    let ( let* ) = bind
  end

end
