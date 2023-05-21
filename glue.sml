structure Rational_CalcLrVals =
        Rational_CalcLrValsFun(structure Token = LrParser.Token)

        structure Rational_CalcLex =
            Rational_CalcLexFun(structure Tokens =
                                        Rational_CalcLrVals.Tokens)
                                        structure Rational_CalcParser=
                                                Join(structure ParserData =
                                                Rational_CalcLrVals.ParserData
                                                              structure
                                                            Lex=Rational_CalcLex
                                                                        structure
                                                                        LrParser=LrParser)