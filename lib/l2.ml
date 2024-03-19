(* lesson 2 notes/examples *)

(* functions *)

(* here is a regular function *)
let f = print_endline @@ "Hello" ^ "World!"

(* here is a recursive function *)
(* also, the comment below is called a specification comment.
   you can use those to add docs to functions and such*)

(** [fact n] is [n]!.
    Requires: [n >= 0]. *)
let rec fact n = if n = 0 then 1 else n * fact (n - 1)

(* integers are kind of special. They are 31 or 63 bits
   because the ocaml GC needs to distinguish between ints and pointers *)

(** [pow x y] is [x] to the power of [y].
    Requires: [y >= 0].*)
let rec pow x y = if y = 0 then 1 else x * pow x (y - 1)

(* mutually recursive functions *)
let rec even n = 
  n = 0 || odd (n - 1)

and odd n = 
  n <> 0 && even (n - 1)

(* anonymous functions *)
(* anonymous functions are also called lambda expressions *)
let _ = (fun x -> x + 1) 3110
let _ = (fun x y -> x +. y) 46. 50.
let mult = fun x y -> x *. y

(* evaluation of e0 e1 ... en:
    1. Evaluate subexpressions: 
        e0 => v0, e1 => v1, ..., en => vn
       v0 must be a function 
 
    2. Substitute vi for xi in e yielding new expression e'.
       Evaluate e' => v. *)

(* Application operators *)
(* Application:
    let (@@) f x = f x 
   Reverse application:
    le (|>) x f = f x *)

let _ = succ @@ 2 * 10

let square x = x * x

let _ = 5 |> succ |> square |> succ


