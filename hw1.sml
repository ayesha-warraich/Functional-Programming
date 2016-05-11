(*QUESTION 1*)

exception InvalidDate;

(*Question 1.1*)
fun last_day (y:int, m:int , d:int)  : int*int*int =
      if m >= 1 andalso m<=12 andalso d >= 1 andalso d<=31 then
        if m>=1 andalso m<=7 then
           if m = 2 then
               if ( y mod 4=0 ) then (y,m,29)
               else
               (y,m,28)
            else
              if (m mod 2 = 0) then (y,m,30)
              else (y,m,31)
        else
             if (m mod 2 = 0) then (y,m,31)
              else (y,m,30)
else raise InvalidDate;



(*Question 1.2*)

fun valid_date (y:int, m:int , d:int) : bool =
    if y>=1 andalso y<=9999
    andalso m >= 1 andalso m<=12
    andalso d >= 1 andalso d<=31
    then
        if m=2 then
            if d=29 andalso y mod 4 = 0
            then true
            else false
        else
            if m >= 1 andalso m<=7 then
                if m mod 2=0 andalso d>30
                then false
                else true
            else
                if m mod 2>0 andalso d>30
                then false
                else true
    else false;

(*Question 1.3*)

fun next_day (y:int, m:int , d:int)  : int*int*int =
if d=31 then
   if m=12
   then (y+1,1,1)
   else (y,m+1,1)
else
   if d=30 then
        if m>=1 andalso m<=7 then
             if m mod 2=0 then (y,m+1,1)
             else (y,m,d+1)
        else
            if m mod 2=0 then (y,m,d+1)
            else (y,m+1,1)
   else
      if m=2 andalso d=28  then
            if y mod 4 = 0 then (y,m,d+1)
            else (y,m+1,1)
      else (y,m,d+1);


(*Question 1.4*)
fun precedes ((y1:int, m1:int , d1:int) , (y2:int, m2:int,d2:int)) : bool =
    if y1<=y2 then
        if y1<y2 then true
        else
            if m1<=m2 then
                if m1<m2 then true
                else
                  if d1<d2 then true
                  else false
            else false
    
    else false;


precedes((2014,2,4),(2014,2,4));
precedes((2014,2,3),(2014,3,4));
precedes((2015,2,3),(2014,3,4));
precedes((2015,2,3),(2014,3,4));


(*Question 1.5*)
fun earliest (date: (int*int*int) list) : int*int*int =
case date of
[] => (0,0,0)
| x::x2::[] => if precedes(x,x2) = true then x else x2
| x::x2::xs =>
    if precedes(x,x2) = true
    then earliest(x::xs)
    else
    earliest(x2::xs);

val d = [ (2015,2,3),(2012,3,5),(2014,5,4),(2013,4,5),(2010,5,4) ];
val e = [ (2012,2,5),(2014,5,4),(2013,4,5),(2015,5,4) ];
earliest(d);
earliest(e);


(*QUESTION 2*)

(*Question 2.1*)

fun zip(x: int list ,y:string list ): (int*string) list =
case (x,y) of
 ([],[]) => []
 |(x::t,[]) => []
 |([],x::t) => []
 |( (hi::ti), (hs::ts) ) => (hi,hs)::(zip (ti, ts));

zip ([1,2,3,4],["alice","bob","carol"]);


(*Question 2.2*)

fun unzip (zipped: (int * string) list) : ( int list * string list) =
case (zipped) of
[]=>([],[])
| (i,s)::tl =>
let val (xi,yi) =
unzip tl
in (i::xi,s::yi) end;


(*QUESTION 3*)

fun incr (lst : int list, amt : int) : int list =
case lst of
[] => []
| x :: xs => (x + amt) :: incr(xs, amt);

fun prefixSum_x( lst: int list ,lst_a: int list  ) : int list =
case (lst,lst_a) of
([],[]) => []
|  ([], x::xs) => []
| ( x::xs , [] )  => x::prefixSum_x(xs, incr(xs,x))
| ( (x::xs), (y::yx) ) => y::prefixSum_x( xs, incr(yx ,x)) ;

(*Question 3.1*)

fun prefixSum(lst: int list) : int list = prefixSum_x(lst,[]);


(*Question 3.3*)

fun prefixSumFast_helper(lst:int list , amt : int) : int list =
case (lst,amt) of
([],0) => []
|([],i) => []
|((x::xs),0) => x::prefixSumFast_helper(xs,x)
|((x::xs),i) => x+i::prefixSumFast_helper(xs,x+i);

fun prefixSumFast( lst: int list) : int list = prefixSumFast_helper(lst, 0);






















