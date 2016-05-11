(* QUESTION 1.1 *)
exception Found of (int * string) list;

datatype 'a tree = Empty | Node of (int * 'a) * 'a tree * 'a tree;

fun collect ( p : int -> bool) ( t : string tree) : (int * string) list =
case t of
Empty => []
| Node((k,v),L,R) => (if p k then (raise Found ([(k,v)]))
                    else (collect p L @ collect p R))

 handle Found x => (collect p L @ x @ collect p R);


(* QUESTION 1.2 *)

fun gather (p : int -> bool) (t : string tree) (k : (int * string) list -> 'a) : 'a =
case t of
 Empty => k []
| Node((x,v), L, R) =>
    if (p x)
    then
        gather p L (fn a => gather p R (fn b => k (a @ ( (x,v):: b) ) ) )
    else
        gather p L (fn a => gather p R (fn b => k (b @ a)));

fun isFour (x : int) : bool = x = 4;


fun first [] = NONE
| first  (x::_) = SOME x;

fun count x = List.length x;


val tinyTree = Node ((6, "I'm"), Node ((4, "so"), Empty, Empty),Node ((40, "little"), Empty, Empty));

gather isFour tinyTree count;
gather isFour tinyTree first;
gather isFour tinyTree (fn x => x);

(*QUESTION 3.1*)

type 'a church = ('a -> 'a) * 'a -> 'a;


fun create ( n : int ) : 'a church =
case n of
0 => (fn (f,x) => x)
| n =>
    let
    val (c) = (create (n-1))
    in
    (fn(f,x) => f(c(f,x)))
end;



(*Q 3.2*)

fun churchToInt ( n : int church) : int = n (fn x=> x+1, 0);

(*Q 3.3*)

fun SUCC (n : 'a church) : 'a church =  (fn (f,x) => f (n (f,x)));

val church_0 : int church = create 0;


