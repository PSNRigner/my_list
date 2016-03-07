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

(* fold_left : Cannot really explain *)
let rec fold_left f l = function
  | Item(head, rest) -> fold_left f (f l head) rest
  | Empty -> l

(* fold_right : Same as fold_left, but in the other sense *)
let rec fold_right f l o = match l with
  | Item(head, rest) -> f head (fold_right f rest o)
  | Empty -> o

(* for_all : Apply func to elements, returning true if all is true *)
let rec for_all f = function
  | Item(head, rest) -> f head && for_all f rest
  | Empty -> true

(* exists : Apply func to elements, returning true if one is true *)
let rec exists f = function
  | Item(head, rest) -> f head || exists f rest
  | Empty -> false

(* mem : Return true if argument equals a value in the list *)
let rec mem o = function
  | Item(head, rest) -> head = o || mem o rest
  | Empty -> false

(* memq : Return true if argument equals a value in the list (physical equality) *)
let rec memq o = function
  | Item(head, rest) -> head == o || memq o rest
  | Empty -> false

(* filter : Return elements that validate condition *)
let rec __filter__ f o = function
  | Item(head, rest) ->
    if f head
    then
      __filter__ f (Item(head, o)) rest
    else
      __filter__ f o rest
  | Empty -> rev o

let filter f l = __filter__ f Empty l

(* mem_assoc : Return true if argument is found as a key in the pairs list *)
let rec mem_assoc o = function
  | Item((key, value), rest) ->
    if o = key
    then
      true
    else
      mem_assoc o rest
  | Empty -> false

(* assoc : Return value associated with argument in the pairs list *)
let rec assoc o = function
  | Item((key, value), rest) ->
    if o = key
    then
      value
    else
      assoc o rest
  | Empty -> raise(Not_found "key not found")
