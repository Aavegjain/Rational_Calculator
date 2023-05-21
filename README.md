In this assignment, we first make a "Rational" library and a "Bigint" libarary, capable of processing rational numbers and integers with arbitrary number of significant digits. We also 
make a command line interface calculator, which is capable of performing 
calculations on these datatypes.

For more info check the attached pdf file. 

Instructions to use the calculator-
1. Download the folder and launch the smlnj interpreter. 
2. If you have ml-lex installed, run - "ml-lex calc.lex" . this will generate a new file -
calc.lex.sml If you dont have ml-lex, you can use the version of calc.lex.sml in the downloaded
folder.
3. If you have ml-yacc installed, run - "ml-yacc calc.grm" . this will generate two new files -
calc.grm.sml and calc.grm.sig If you dont have ml-yacc, you can use the version of calc.grm.sml and
calc.grm.sig in the downloaded folder.
4. after these steps, run the file rational.sml on the smlnj interpreter. 
A command line interface will begin.  
5. If the command line interface doesnt show up, write "parse() ;" . This should launch the 
interface. 
6. A sample use of the calculator is shown below, with more details below it. 

example session- 
- parse() ; 
23 + 34;
x = 34 ; 
y = x + 45.9(0) ; 
printrat x ; 
result = 57/1
x= 34/1
printdec y ; 
y= 79.9(0)


Interface - 
1. variable declaration of form - "x1, x2, x3 = <expression> "  
2. for simplicity all variables are converted to rational under the hood. 
3. if one needs to update the value of a variable, then she has to redeclare the variable( in spirit of functional programming) . 
4. one can see the variable value by typing - printrat <variablename> for fractional normal form 
and printdec <variablename> for decimal normal form. 

references - ml lex and ml yacc documentation (have used some code as such)

--------------------------------------------------------------
pragmatic decisions (when using rational.sml without parsing) - 
1. we have declared the structure Rational to be abstract, thus to see a rational, use showRat or showDecimal on a rational. a rational can be generated from make_rat,rat,reci,fromDecimal. 
2. thorughout the library we have assumed that rational(x,y) is in std form, since it can only be obtained through make_rat, rat, reci, fromDecimal all of which use make_rat which converts the rational to fractional normal form.
--------------------------------------------------------------- 
(in grammars, we denote non terminals by <>)
grammar for rational numbers - 
<rational> -> <bigint> | <decimal> | ( <bigint>, <denum> ) 
<denum> -> <sign><d>
<d> -> digit <d> | nonzero
<bigint> -> <sign><digits> 
<digits> -> digit <digits> | digit 
<sign> -> + | ~ | null  

terminals - 1. digit - [0-9] 
            2. nonzero - [1-9] 
            2. null - epsilon 

-------------------------------------------------------------------
grammar for rational number expressions - 

<statement> -> <expression> | <declaration> | <print> | null 

<declaration> -> <var> equal <expression> | <var> comma <declaration> 

<print> -> printrat <var> | printdec <var> 

<expression> -> <expression> plus <term> | <expression> minus <term> | <term> 
<term> -> <factor>  mult <term> | <factor> div <term> | <factor> 
<factor> -> <var> | <rational> | tilde <var> | tilde <rational> | lparen <expression> rparen 

<var> -> alpha | alpha <var> 

terminals - plus - "+", minus - "-", mult - "*" , div - "/" , tilde - "~" , lparen - "(", rparen - ")" 
            printrat - "printrat", printdec - "printdec", equal - "=" , comma - "," null - epsilon 