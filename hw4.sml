
(*Question 1*)
Control.Print.printDepth := 100;


datatype 'a rlist = Empty | RCons of 'a * (('a rlist) ref)

fun insert ( f : ('a*'a->bool) ,  x : 'a ,  l : ('a rlist ref)  ) =
case !l of
Empty => l:= RCons (x, ref Empty)
| RCons(h, t) => if f (x,h) = true then (l := RCons ( x, ref(!l)))
else insert (f, x, t)

(* tests
fun great ( x : int , h: int) = if x > h then true else false

val li = ref (RCons(5,ref (RCons (2,ref(RCons(1,ref Empty))))))

insert (great, 4, li)*)


(*Question 2*)

datatype transactions =
Withdraw of int
| Deposit of int
| Check_balance

fun make_account (opening_balance: int, password: string)  =
let val balance = ref opening_balance
    val pass = ref password
in
    fn (trans: transactions , given_pass : string) =>
        if (given_pass = !pass) then
            case trans of
          Withdraw(a) => ((balance := !balance-a); !balance)
        | Deposit(a) => ((balance := !balance+a); !balance)
        | Check_balance => (!balance)
    else
(print ("Wrong password.\n"); 0)
end;
