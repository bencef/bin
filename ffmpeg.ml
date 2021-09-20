module Ffmpeg: sig
  type t
  (** Abstract type for a builder of an ffmpeg command. *)

  type pix_fmt =
    | Yuv420p

  module Source: sig
    type t
    (** abstract type representing a source *)

    type handle
    (** abstract type representing a source for use as parameter
        to builder functions *)

    val file: string -> t
    (** [file path] creates a new source object representing an input file *)

    val v4l2: ?frame_rate:int -> ?pix_fmt:pix_fmt -> string -> t
    (** [v4l2 ~frame_rate ~pix_fmt device] creates a new source representing a
        v4l2 device.  Example value for the device is [/dev/video0] *)
  end

  val builder: unit -> t
  (** [builder ()] returns a new builder *)

  val to_string_array: t -> string array
  (** [to_string_array builder] returns an array of parameters that can be
      passed to the ffmpeg command. *)
  val source: t -> Source.t -> Source.handle * t
  (** [source builder input] adds a source to the builder and returns
      an opaque handle to the source and the new builder. *)
end =
  struct
    type t = { inputs: string array list
             ; pipeline: string array
             ; outputs: string array }

    type pix_fmt =
      | Yuv420p

    let string_of_pix_fmt = function
      | Yuv420p -> "yuv420p"

    module Source =
      struct
        type t =
          | FilePath of string
          | V4l2 of string * int option * pix_fmt option

        type handle = int

        let file path = FilePath path

        let v4l2 ?frame_rate ?pix_fmt path = V4l2 (path, frame_rate, pix_fmt)
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

    let source builder input =
      let handle = builder.inputs |> List.length in
      let add_opt value transform template_fun =
        value
        |> Option.map transform
        |> Option.map template_fun
        |> Option.value ~default:[||] in
      let input =
        let open Source in
        match input with
        | FilePath path -> [| "-i"; path |]
        | V4l2 (path, frame_rate, pix_fmt) ->
           Array.concat [
               [| "-f"; "v4l2"|];
               add_opt frame_rate string_of_int (fun rate -> [| "-framerate"; rate |]);
               add_opt pix_fmt string_of_pix_fmt (fun pix_fmt ->
                   [| "-input_format"; "rawvideo"; "-pix_fmt"; pix_fmt |]);
               [|"-i"; path |]
             ]
      in
      let inputs = input :: builder.inputs in
      (handle, { builder with inputs })
  end
