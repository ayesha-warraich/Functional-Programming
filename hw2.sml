(*QUESTION1*)

fun remove x nil =  nil
| remove x (y::ys) =
if (x = y)
then (remove x ys)
else y::(remove x ys);


fun remDuplicates ( x : ''a list ) : ( ''a list ) =
case(x) of
nil => nil
|(x::xs) => x::(remDuplicates(remove x xs));

(*QUESTION 2*)

exception Diverge
 
fun derivative f h x  = (f (x + h) - f (x - h)) / (2.0 * h);

fun newton(f : (real -> real) , x : real, e : real) : real =
let
fun newton_a(f, x, e ,count) =
 if count = 1000
 then raise Diverge
 else
    if  abs(f x) < e then x
        else newton_a( f, x - (( f x)/(derivative f 0.00000001 x)), e, count+1)
in newton_a(f,x,e,0)
end;

(*QUESTION 4.1*)
 
 datatype Mathexp =
 Num of int
 | Var of string
 | Neg of Mathexp
 | Add of Mathexp * Mathexp
 | Mul of Mathexp * Mathexp
 
 val zero = Num 0;
 val one =  Num 1;


fun diff( e: Mathexp , v: string) : Mathexp =
case (e,v) of
(Num(x),v) => zero
|(Var(z),v) =>
if z=v
then one
else zero
|(Neg(e1),v) => Neg(diff (e1,v))
|(Add(e1,e2),v) => Add(diff(e1,v),diff(e2,v))
|(Mul(e1,e2),v) =>
 let
    val t1 = Mul(diff(e1,v),e2)
    val t2 = Mul(e1,diff(e2,v))
 in
 Add(t1,t2)
 end;
 
 
 (*diff( Mul( Add( Var("x"),Var("x") ), Add ( Var("x"),Var("y") ) ) , "x"  );*)
 
 
(*QUESTION 4.2*)

fun simplify(e:Mathexp) : Mathexp =
case e of
Num(x) => Num(x)
|Var(x) => Var(x)
|Neg(e1) => Neg(simplify(e1))
|Add(e1,Num(0))=> e1
|Add(Num(0),e1)=> e1
|Mul(e1,Num(1)) => e1
|Mul(Num(1),e1) => e1
|Mul(e1,Num(0)) => zero
|Mul(Num(0),e1) => zero
|Add(e1,e2) => Add( simplify(e1), simplify(e2))
|Mul(e1, e2) => Mul( simplify(e1),simplify(e2));


(*simplify(Mul( (Add ( Var("e"),Var("y") )),  Num(0)));*)
