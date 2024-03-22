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

(* polymorphic functions *)
(* poly = many, morph = form *)
(* write functions that work for many arguments
   regardless of their type *)

(* identity function is the function that returns its input *)
let id x = x

(* 'a is a type variable. Typically called alpha *)

(* we can make it more strict with manual type annotations *)
let int_id (x : int) : int = x

(* another way... *) 
let id_int' : int -> int = id
(* we limit the ability of id_int to integers with the
   polymorphic function id *)

(* let first x y = x *)
(* let first_int : int -> 'b -> int = first *)
(* let bad_first : int -> 'b -> string = first *)


(* labeled and optional arguments *)
let f ~name1:arg1 ~name2:arg2 = arg1 + arg2
let res = f ~name2:3 ~name1:4

(* labels for args are often the same name. OCaml allows a shorthand *)
let fn ~name1 ~name2 = name1 +. name2
let res2 = fn ~name1:3. ~name2:2.

(* optional args *)
let fn2 ?name:(arg1=8) arg2 = arg1 + arg2

(* Partial application *)
(* you could define an addition function like this *)
let add x y = x + y

(* you could also do it like this *)
let addx x = fun y -> x + y

let add5 = addx 5
let res = add5 2

(* you could also do it with the regular add *)
let add5 = add 5
let res = add5 2

(* this is called partial application: we partially applied the
   function add to one argument. This works because the following 
   functions are syntactically different but semantically equivalent.
   That is, they are different ways of expressing the same computation.*)

(* Function associativity *)
(* The TRUTH... every OCaml function takes exactly one argument *)

(* Functions take a single argument and returns a new function
   that expects the remaining arguments *)

(* function types are right associative and function application
   is left associative *)

(* function types *)
(* t1 -> (t2 -> (t3 -> t4)) *)

(* function application *)
(* ((e1 e2) e3) e4 *)

(* operators as functions *)
(* addition operator + has type int -> int -> int 
   normall infix 3 + 4; but putting paren can make it
   prefix *)
let res = ( + ) 3 4

let add3 = ( + ) 3

(* you can even define our own infix operators *)
let ( ^^ ) x y = max x y
let res = 2 ^^ 3

(* tail recursion *)
(** [count n] is [n], computed by adding 1 to itself [n] times. That is,
    this function coun up from 1 to [n]. *)
let rec count n =
  if n = 0 then 0 else 1 + count (n - 1)

let res = count 10

let rec count_aux n acc = 
  if n = 0 then acc else count_aux (n - 1) (acc + 1)

let count_tr n = count_aux n 0 

(* why does tail recursion matter: a recursive call in tail position does not need
   a new stack frame. It can just reuese the existing stack frame because there is
   nothing left of use in the existing stack frame. The compiler just recycles the
   space. *)
(* This is called tail-call optimization. It reduces space complexity from O(n) to O(1) *)

(* the recipe for tail recurion is as follows:
    1. Change the function into a helper function. Add an extra argument: the accumulator,
       often named acc.
    2. Write a new "main" version of the function that calls the helper. It passes the 
       original base case's return value as the initial value of the accumulator.
    3. Change the helper function to return the accumulator in the base case.
    4. Change the helper function's recursive case. *)

let rec fact_aux n acc =
  if n = 0 then acc else fact_aux (n - 1) (n * acc)

let fact_tr n = fact_aux n 1
