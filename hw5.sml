exception NoMoreNumbers;



datatype exp =
Nat of int
| Bool of bool
| Plus of exp * exp
| Mult of exp * exp
| If of exp * exp * exp
| And of exp * exp
| Not of exp
| Eq of exp * exp
| Var of string
| Let of exp * (string * exp)
| Fun of string * string * exp
| Apply of exp * exp


fun check ( i:string , l : string list) : bool =
case (i,l) of
(i,[]) => false
| (i, x::xs) =>  if (i=x) then true
                else check(i,xs)

fun free_list (e : exp):string list =
let
    fun free_list_e ( e:exp, l :string list) =
        case (e,l) of
              (Nat(x),l) => []
            | (Bool(x),l) => []
            | (Plus(x,y),l) => free_list_e(x,l) @ free_list_e(y,l)
            | (Mult(x,y),l) => free_list_e(x,l) @ free_list_e(y,l)
            | (If(x,y,z),l) => free_list_e(x,l) @ free_list_e(y,l) @ free_list_e(z,l)
            | (And(x,y),l) => free_list_e(x,l) @ free_list_e(y,l)
            | (Not(x),l) => free_list_e(x,l)
            | (Eq(x,y),l) => free_list_e(x,l) @ free_list_e(y,l)
            | (Var(x),l) => if(check(x,l) = true) then []
                            else [x]
            | (Let(x,(y,z)),l) => ( case x of
                                        Var(a) => ( if (check(a,l) = true ) then free_list_e( z, a::y::l )
                                        else a::free_list_e( z, y::l))
                                      | a => (free_list_e(z,y::l)) )
            | (Fun(x,y,z),l) => (free_list_e(z, x::y:: l))
            | (Apply(x,y),l) => (free_list_e(x,l) @ free_list_e(y,l))
in
free_list_e ( e ,[])
end






(*Q2.1*)

fun create (n) = 0::List.tabulate(n-1, fn x => x+1);


fun create_catalan ( n: int) : int =
if n >= 1 then
    let val list = create (n)
    in
    List.foldl (fn(x,y) => ( ((2*((2*x)+1))*y) div (x+2))) 1 list
    end
else
1;

(*Q2.2*)

datatype realSeq = Cons of real * (unit -> realSeq);


fun  helperSeq ( a: int) : realSeq =
Cons(((2.0*((2.0*(Real.fromInt(a)))+1.0))/ (Real.fromInt(a)+2.0))  , (fn()=> helperSeq ( a+1 ) )  )

fun take n s = case (n , s) of
(0, Cons (x, f)) => []
| (n, Cons (x, f)) => x :: (take (n-1) (f ()))

fun catalanSeq() =
let
fun calcSeq (i:real,x:realSeq) : realSeq =
    case (i,x) of
    ( _ , (Cons (x,f))) => Cons(x*i,(fn()=>calcSeq(i*x, f () )))
    in
    calcSeq (1.0 ,(helperSeq 0))
end



(*Q3*)


datatype 'a instr = Put of 'a | Get | Restore;


fun makeCell (x : 'a) =
let val list_vals = [x]
    val ref_list = ref list_vals

in
    fn (operation : 'a instr ) =>
     case operation of
        Put(a) => ((ref_list := a::(!ref_list) ); a)
        |Get => (case (!ref_list) of
                ( x::xs ) => (x))
        |Restore => ( case (!ref_list) of
                    ([]) => (raise NoMoreNumbers)
                    | (x::xs)  => ((ref_list := xs ); x))
end










