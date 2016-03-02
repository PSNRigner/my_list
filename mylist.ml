type 'a my_list = 
  | Item of ('a * 'a my_list)
  | Empty


(* length : return the length of the list *)
let rec length = function
  | Item(head, rest) -> (length rest) + 1
  | Empty -> 0


(* hd : return the head of the list *)
let rec hd = function
  | Item(head, rest) -> head
  | Empty -> raise(Failure "empty list (hd)")


(* tl : return the tail of the list *)
let rec tl = function
  | Item(head, rest) -> rest
  | Empty -> raise(Failure "empty list (tl)")


(* nth : return the nth element of the list *)
let rec nth my_list n =
  if n < 0
  then
     raise(Invalid_argument "negative index (nth)")
  else
    match my_list with
      | Item(head, rest) ->
        if n = 0
        then
          head
        else
          nth rest (n - 1)
      | Empty -> raise(Failure "invalid index (nth)")
