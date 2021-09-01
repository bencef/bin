module Ffmpeg: sig
  type t
  (** Abstract type for a builder of an ffmpeg command. *)

  module Source: sig
    type t
    (** abstract type representing a source *)

    type handle
    (** abstract type representing a source for use as parameter
        to builder functions *)

    val file: string -> t
    (** [file path] creates a new source object representing an input file *)
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

    module Source =
      struct
        type t =
          | FilePath of string
        type handle = int

        let file path = FilePath path
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
      let input =
        let open Source in
        match input with
        | FilePath path -> [| "-i"; path |]
      in
      let inputs = input :: builder.inputs in
      (handle, { builder with inputs })
  end
