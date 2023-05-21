functor Rational_CalcLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Rational_Calc_TOKENS
   end
 =
struct
structure ParserData=
struct
structure Header =
struct



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in
val table=let val actionRows =
"\
\\001\000\001\000\011\000\003\000\010\000\006\000\036\000\007\000\036\000\
\\008\000\009\000\010\000\008\000\011\000\007\000\014\000\006\000\000\000\
\\001\000\001\000\011\000\003\000\010\000\008\000\009\000\014\000\021\000\000\000\
\\001\000\001\000\023\000\014\000\022\000\000\000\
\\001\000\002\000\041\000\003\000\041\000\004\000\013\000\005\000\012\000\
\\006\000\041\000\007\000\041\000\009\000\041\000\000\000\
\\001\000\002\000\042\000\003\000\042\000\004\000\013\000\005\000\012\000\
\\006\000\042\000\007\000\042\000\009\000\042\000\000\000\
\\001\000\002\000\043\000\003\000\043\000\004\000\043\000\005\000\043\000\
\\006\000\043\000\007\000\043\000\009\000\043\000\000\000\
\\001\000\002\000\044\000\003\000\044\000\004\000\044\000\005\000\044\000\
\\006\000\044\000\007\000\044\000\009\000\044\000\000\000\
\\001\000\002\000\045\000\003\000\045\000\004\000\045\000\005\000\045\000\
\\006\000\045\000\007\000\045\000\009\000\045\000\000\000\
\\001\000\002\000\046\000\003\000\046\000\004\000\046\000\005\000\046\000\
\\006\000\046\000\007\000\046\000\009\000\046\000\000\000\
\\001\000\002\000\047\000\003\000\047\000\004\000\047\000\005\000\047\000\
\\006\000\047\000\007\000\047\000\009\000\047\000\000\000\
\\001\000\002\000\047\000\003\000\047\000\004\000\047\000\005\000\047\000\
\\006\000\047\000\007\000\047\000\012\000\017\000\013\000\016\000\000\000\
\\001\000\002\000\048\000\003\000\048\000\004\000\048\000\005\000\048\000\
\\006\000\048\000\007\000\048\000\009\000\048\000\000\000\
\\001\000\002\000\049\000\003\000\049\000\004\000\049\000\005\000\049\000\
\\006\000\049\000\007\000\049\000\009\000\049\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\005\000\012\000\
\\006\000\033\000\007\000\033\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\005\000\012\000\
\\006\000\039\000\007\000\039\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\005\000\012\000\
\\009\000\031\000\000\000\
\\001\000\006\000\000\000\007\000\000\000\000\000\
\\001\000\006\000\034\000\007\000\034\000\000\000\
\\001\000\006\000\035\000\007\000\035\000\000\000\
\\001\000\006\000\037\000\007\000\037\000\000\000\
\\001\000\006\000\038\000\007\000\038\000\000\000\
\\001\000\006\000\040\000\007\000\040\000\000\000\
\\001\000\012\000\017\000\013\000\016\000\000\000\
\\001\000\014\000\018\000\000\000\
\\001\000\014\000\019\000\000\000\
\\001\000\014\000\029\000\000\000\
\"
val actionRowNumbers =
"\000\000\018\000\017\000\013\000\
\\010\000\023\000\024\000\001\000\
\\002\000\008\000\001\000\001\000\
\\001\000\001\000\025\000\001\000\
\\020\000\019\000\015\000\009\000\
\\012\000\011\000\006\000\005\000\
\\004\000\003\000\021\000\022\000\
\\014\000\007\000\016\000"
val gotoT =
"\
\\001\000\030\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\018\000\000\000\
\\000\000\
\\000\000\
\\002\000\022\000\000\000\
\\002\000\023\000\000\000\
\\002\000\024\000\000\000\
\\002\000\025\000\000\000\
\\003\000\026\000\000\000\
\\002\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 31
val numrules = 17
val s = ref "" and index = ref 0
val string_to_int = fn () =>
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue =
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | VAR of unit ->  (string) | RATIONAL of unit ->  (Rational.rational)
 | DECLARATION of unit ->  (Rational.rational)
 | EXPRESSION of unit ->  (Rational.rational)
 | STATEMENT of unit ->  (Rational.rational option)
end
type svalue = MlyValue.svalue
type result = Rational.rational option
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 6) => true | _ => false
val preferred_change : (term list * term list) list =
(nil
,nil
 $$ (T 1))::
(nil
,nil
 $$ (T 3))::
(nil
,nil
 $$ (T 4))::
(nil
,nil
 $$ (T 2))::
nil
val noShift =
fn (T 5) => true | _ => false
val showTerminal =
fn (T 0) => "RATIONAL"
  | (T 1) => "PLUS"
  | (T 2) => "MINUS"
  | (T 3) => "MULT"
  | (T 4) => "DIV"
  | (T 5) => "EOF"
  | (T 6) => "SEMI"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "PRINT_RAT"
  | (T 10) => "PRINT_DEC"
  | (T 11) => "EQUAL"
  | (T 12) => "COMMA"
  | (T 13) => "VAR"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$
(T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct
exception mlyAction of int
local open Header in
val actions =
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left,
EXPRESSION1right)) :: rest671)) => let val  result =
MlyValue.STATEMENT (fn _ => let val  (EXPRESSION as EXPRESSION1) =
EXPRESSION1 ()
 in ( SOME(EXPRESSION)  )
end)
 in ( LrTable.NT 0, ( result, EXPRESSION1left, EXPRESSION1right),
rest671)
end
|  ( 1, ( ( _, ( MlyValue.DECLARATION DECLARATION1, DECLARATION1left,
DECLARATION1right)) :: rest671)) => let val  result =
MlyValue.STATEMENT (fn _ => let val  DECLARATION1 = DECLARATION1 ()
 in (NONE)
end)
 in ( LrTable.NT 0, ( result, DECLARATION1left, DECLARATION1right),
rest671)
end
|  ( 2, ( ( _, ( MlyValue.ntVOID PRINT1, PRINT1left, PRINT1right)) ::
rest671)) => let val  result = MlyValue.STATEMENT (fn _ => let val
PRINT1 = PRINT1 ()
 in (NONE)
end)
 in ( LrTable.NT 0, ( result, PRINT1left, PRINT1right), rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.STATEMENT (fn _ => (
NONE))
 in ( LrTable.NT 0, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( MlyValue.VAR VAR1, _, VAR1right)) :: ( _, ( _,
PRINT_RAT1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  (VAR as VAR1) = VAR1 ()
 in (
 print(VAR ^ "= " ^ Rational.showRat(SymTable.lookup_entry(VAR))^"\n" )
)
end; ()))
 in ( LrTable.NT 3, ( result, PRINT_RAT1left, VAR1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.VAR VAR1, _, VAR1right)) :: ( _, ( _,
PRINT_DEC1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  (VAR as VAR1) = VAR1 ()
 in (
 print(VAR ^ "= " ^  Rational.showDecimal(SymTable.lookup_entry(VAR)) ^"\n" )
)
end; ()))
 in ( LrTable.NT 3, ( result, PRINT_DEC1left, VAR1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXPRESSION EXPRESSION1, _, EXPRESSION1right)
) :: _ :: ( _, ( MlyValue.VAR VAR1, VAR1left, _)) :: rest671)) => let
 val  result = MlyValue.DECLARATION (fn _ => let val  (VAR as VAR1) =
VAR1 ()
 val  (EXPRESSION as EXPRESSION1) = EXPRESSION1 ()
 in (let val x = SymTable.add_entry(VAR,EXPRESSION) in EXPRESSION end
)
end)
 in ( LrTable.NT 2, ( result, VAR1left, EXPRESSION1right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.DECLARATION DECLARATION1, _,
DECLARATION1right)) :: _ :: ( _, ( MlyValue.VAR VAR1, VAR1left, _)) ::
 rest671)) => let val  result = MlyValue.DECLARATION (fn _ => let val
 (VAR as VAR1) = VAR1 ()
 val  (DECLARATION as DECLARATION1) = DECLARATION1 ()
 in (
 let val x = SymTable.add_entry(VAR, DECLARATION) in DECLARATION end
)
end)
 in ( LrTable.NT 2, ( result, VAR1left, DECLARATION1right), rest671)

end
|  ( 8, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right)
) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _))
 :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (Rational.add(EXPRESSION1, EXPRESSION2) )
end)
 in ( LrTable.NT 1, ( result, EXPRESSION1left, EXPRESSION2right),
rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right)
) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _))
 :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (Rational.subtract(EXPRESSION1, EXPRESSION2) )
end)
 in ( LrTable.NT 1, ( result, EXPRESSION1left, EXPRESSION2right),
rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (Rational.multiply(EXPRESSION1, EXPRESSION2) )
end)
 in ( LrTable.NT 1, ( result, EXPRESSION1left, EXPRESSION2right),
rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (valOf(Rational.divide(EXPRESSION1, EXPRESSION2)))
end)
 in ( LrTable.NT 1, ( result, EXPRESSION1left, EXPRESSION2right),
rest671)
end
|  ( 12, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXPRESSION
EXPRESSION1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.EXPRESSION (fn _ => let val  EXPRESSION1 =
EXPRESSION1 ()
 in (EXPRESSION1)
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.RATIONAL RATIONAL1, RATIONAL1left,
RATIONAL1right)) :: rest671)) => let val  result = MlyValue.EXPRESSION
 (fn _ => let val  (RATIONAL as RATIONAL1) = RATIONAL1 ()
 in (RATIONAL)
end)
 in ( LrTable.NT 1, ( result, RATIONAL1left, RATIONAL1right), rest671)

end
|  ( 14, ( ( _, ( MlyValue.VAR VAR1, VAR1left, VAR1right)) :: rest671)
) => let val  result = MlyValue.EXPRESSION (fn _ => let val  (VAR as
VAR1) = VAR1 ()
 in (SymTable.lookup_entry(VAR))
end)
 in ( LrTable.NT 1, ( result, VAR1left, VAR1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.RATIONAL RATIONAL1, _, RATIONAL1right)) ::
( _, ( _, MINUS1left, _)) :: rest671)) => let val  result =
MlyValue.EXPRESSION (fn _ => let val  (RATIONAL as RATIONAL1) =
RATIONAL1 ()
 in (Rational.neg(RATIONAL))
end)
 in ( LrTable.NT 1, ( result, MINUS1left, RATIONAL1right), rest671)

end
|  ( 16, ( ( _, ( MlyValue.VAR VAR1, _, VAR1right)) :: ( _, ( _,
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.EXPRESSION
 (fn _ => let val  (VAR as VAR1) = VAR1 ()
 in (Rational.neg(SymTable.lookup_entry(VAR)))
end)
 in ( LrTable.NT 1, ( result, MINUS1left, VAR1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.STATEMENT x => x
| _ => let exception ParseInternal
        in raise ParseInternal end) a ()
end
end
structure Tokens : Rational_Calc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun RATIONAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.RATIONAL (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun MULT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT_RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT_DEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VAR (fn () => i),p1,p2))
end
end