structure Tokens = Tokens


type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)
val error = fn (e,l : int,_) =>
              TextIO.output(TextIO.stdOut ,"line " ^ (Int.toString l) ^
                               ": " ^ e ^ "\n")

%%

%header (functor Rational_CalcLexFun(structure Tokens: Rational_Calc_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
sign = [+|~] ;
ws = [\ \t];

%%
\n => (pos := (!pos) + 1 ; lex() ) ;
{ws}+ => (lex()) ;
{sign}?{digit}+ => (Tokens.RATIONAL( (valOf(Rational.rat(yytext))),!pos,!pos)) ;
{sign}?({digit}*).({digit}*)"("({digit}+)")"  => (Tokens.RATIONAL( (Rational.fromDecimal(yytext)),!pos,!pos)) ;
"+"      => (Tokens.PLUS(!pos,!pos));
"*"      => (Tokens.MULT(!pos,!pos));
";"      => (Tokens.SEMI(!pos,!pos));
"-"      => (Tokens.MINUS(!pos,!pos));
"/"      => (Tokens.DIV(!pos,!pos));
"="      => (Tokens.EQUAL(!pos,!pos));
","      => (Tokens.COMMA(!pos,!pos)) ;
{alpha}+  => (if (yytext = "printrat") then Tokens.PRINT_RAT ( !pos, !pos)
               else if (yytext = "printdec") then Tokens.PRINT_DEC ( !pos, !pos)
               else Tokens.VAR(yytext, !pos, !pos));
"("      => (Tokens.LPAREN(!pos,!pos)) ;
")"      => (Tokens.RPAREN(!pos,!pos)) ;
"."      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());