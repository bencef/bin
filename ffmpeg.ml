module Ffmpeg: sig
  type t
  (** Abstract type for a builder of an ffmpeg command. *)

  type pix_fmt =
    | Yuv420p

  type color = {r: int; g: int; b: int}

  module Source: sig
    type handle
    (** abstract type representing a source for use as parameter
        to builder functions *)

    val file: t -> string -> handle * t
    (** [file builder path] creates a new source object representing an input file *)

    val v4l2: ?frame_rate:int -> ?pix_fmt:pix_fmt -> t -> string -> handle * t
    (** [v4l2 ?frame_rate ?pix_fmt builder device] creates a new source representing a
        v4l2 device.  Example value for the device is [/dev/video0] *)
  end

  module Filter: sig
    val color: t -> color -> Source.handle * t
    (** [color builder color_value] creates a source representing a picture of
        that color. *)
  end

  val builder: unit -> t
  (** [builder ()] returns a new builder *)

  val to_string_array: t -> string array
  (** [to_string_array builder] returns an array of parameters that can be
      passed to the ffmpeg command. *)
end =
struct
  type t = { inputs: string array list
           ; pipeline: string array
           ; outputs: string array }

  type pix_fmt =
    | Yuv420p

  type color = {r: int; g: int; b: int}

  let string_of_pix_fmt = function
    | Yuv420p -> "yuv420p"

  let add_source builder params =
    let handle = builder.inputs |> List.length |> string_of_int in
    let inputs = params :: builder.inputs in
    (handle, { builder with inputs })

  module Source =
  struct
    type handle = string

    let file builder path =
      let params = [| "-i"; path |] in
      add_source builder params

    let v4l2 ?frame_rate ?pix_fmt builder path =
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
      add_source builder params
  end

  module Filter =
  struct
    let color builder color =
      (* TODO: implement me *)
      "f1", builder
  end

  let builder () =
    let inputs = [] in
    let pipeline = [||] in
    let outputs = [||] in
    { inputs; pipeline; outputs }

  let to_string_array builder =
    [ builder.inputs |> List.rev |> Array.concat
    ; builder.pipeline
    ; builder.outputs ]
    |> Array.concat
end
