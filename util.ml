module Util :
sig
  val arg : int -> string option
  (** Get the command-line argument with index. The element at index 0
      is always present and is the name of the executable. *)

  val index_of_opt : 'a -> 'a list -> int option
  (** Get the index of the given element.  Uses a list to make it finite. *)
end =
struct
  let arg index =
    if Array.length Sys.argv > index then Some Sys.argv.(index) else None

  let index_of_opt element list =
    let indices = list
                  |> List.mapi (fun idx v -> (idx, v))
                  |> List.filter_map (fun (idx, v) ->
                      if v = element
                      then Some idx
                      else None) in
    let first_match = List.nth_opt indices 0 in
    first_match
end

module NonEmpty :
sig
  type 'a t
  (** A List type that always has at least one element *)

  val make : 'a list -> ('a t) option
  (** Wrap a list into the non-empty type.

      @return None
      if the list is empty.
  *)

  val to_array : 'a t -> 'a array
  (** Return the contents as an array.  The returned array is never empty.  *)
end =
struct
  type 'a t = NonEmpty of 'a * 'a list

  let make = function
    | head :: tail -> Some (NonEmpty (head, tail))
    | _            -> None

  let to_array = function
    | NonEmpty (head, tail) -> head::tail |> Array.of_list
end
