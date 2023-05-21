CM.make("$/basis.cm") ;
CM.make("$/ml-yacc-lib.cm") ;
use "bigint.sml" ;
use "rational_structure.sml" ;
use "symbol_table.sml" ;
use "calc.grm.sig" ;
use "calc.grm.sml" ;
use "calc.lex.sml" ;
use "glue.sml" ;


  fun invoke lexstream =
    let fun print_error (s,i:int,_) =
            TextIO.output(TextIO.stdOut,
                          "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
     in Rational_CalcParser.parse(0,lexstream,print_error,())
    end

fun parse () =
    let val input_line = fn f =>
          let fun loop result =
             let val option = TextIO.input1 f ; val test = Option.isSome(option)
             in if ( (not test) orelse Option.getOpt(option,#"%") = #"\n") then String.implode (rev result)
                 else loop ( (Option.getOpt(option, #"%")) :: result)
             end
          in loop nil
          end
        val lexer = Rational_CalcParser.makeLexer
                      (fn _ => input_line TextIO.stdIn)
        val dummyEOF = Rational_CalcLrVals.Tokens.EOF(0,0)
        val dummySEMI = Rational_CalcLrVals.Tokens.SEMI(0,0)
        fun loop lexer =
            let val (result,lexer) = invoke lexer
                val (nextToken,lexer) = Rational_CalcParser.Stream.get lexer
             in case result
                  of SOME r =>
                      TextIO.output(TextIO.stdOut,
                             "result = " ^ (Rational.showRat(r)) ^ "\n")
                   | NONE => ();
                if Rational_CalcParser.sameToken(nextToken,dummyEOF) then ()
                else loop lexer
            end
     in loop lexer
    end

val x = parse() ; 