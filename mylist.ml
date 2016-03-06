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

(* append : append two lists *)
let rec append l1 l2 = match l1 with
  | Item(head, rest) -> Item(head, (append rest l2))
  | Empty -> l2

(* rev_append : reverse first list and appends it to 2nd list *)
let rec rev_append l1 l2 = match l1 with
  | Item(head, rest) -> rev_append rest (Item(head, l2))
  | Empty -> l2

(* rev : reverse list *)
let rec rev l = rev_append l Empty

(* flatten : list of lists -> one list *)
let rec flatten = function
  | Item(head, rest) -> append head (flatten rest)
  | Empty -> Empty

(* iter : apply function to all elements *)
let rec iter f = function
  | Item(head, rest) ->
    begin
      f head;
      iter f rest;
    end
  | Empty -> ()

(* map : apply function to all elements and creates new list with return values *)
let rec map f = function
  | Item(head, rest) -> let tmp = f head in Item(tmp, map f rest)
  | Empty -> Empty

