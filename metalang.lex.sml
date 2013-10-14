structure MetaLangLex  = struct

    datatype yystart_state = 
INITIAL
    structure UserDeclarations = 
      struct

 
    structure Token = MetaLangTokens;
    type lex_result = Token.token;
    fun eof() = Token.EOF


      end

    local
    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector = 
Vector.fromList []
    fun yystreamify' p input = ULexBuffer.mkStream (p, input)

    fun yystreamifyReader' p readFn strm = let
          val s = ref strm
	  fun iter(strm, n, accum) = 
	        if n > 1024 then (String.implode (rev accum), strm)
		else (case readFn strm
		       of NONE => (String.implode (rev accum), strm)
			| SOME(c, strm') => iter (strm', n+1, c::accum))
          fun input() = let
	        val (data, strm) = iter(!s, 0, [])
	        in
	          s := strm;
		  data
	        end
          in
            yystreamify' p input
          end

    fun yystreamifyInstream' p strm = yystreamify' p (fn ()=>TextIO.input strm)

    fun innerLex 
(yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yysetStrm strm = yystrm := strm
	  fun yygetPos() = ULexBuffer.getpos (!yystrm)
	  fun yystreamify input = yystreamify' (yygetPos()) input
	  fun yystreamifyReader readFn strm = yystreamifyReader' (yygetPos()) readFn strm
	  fun yystreamifyInstream strm = yystreamifyInstream' (yygetPos()) strm
        (* start position of token -- can be updated via skip() *)
	  val yystartPos = ref (yygetPos())
	(* get one char of input *)
	  fun yygetc strm = (case UTF8.getu ULexBuffer.getc strm
                of (SOME (0w10, s')) => 
		     (AntlrStreamPos.markNewLine yysm (ULexBuffer.getpos strm);
		      SOME (0w10, s'))
		 | x => x)
          fun yygetList getc strm = let
            val get1 = UTF8.getu getc
            fun iter (strm, accum) = 
	        (case get1 strm
	          of NONE => rev accum
	           | SOME (w, strm') => iter (strm', w::accum)
	         (* end case *))
          in
            iter (strm, [])
          end
	(* create yytext *)
	  fun yymksubstr(strm) = ULexBuffer.subtract (strm, !yystrm)
	  fun yymktext(strm) = Substring.string (yymksubstr strm)
	  fun yymkunicode(strm) = yygetList Substring.getc (yymksubstr strm)
          open UserDeclarations
          fun lex () = let
            fun yystuck (yyNO_MATCH) = raise Fail "lexer reached a stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yygetPos()
	    fun yygetlineNo strm = AntlrStreamPos.lineNo yysm (ULexBuffer.getpos strm)
	    fun yygetcolNo  strm = AntlrStreamPos.colNo  yysm (ULexBuffer.getpos strm)
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    val yylastwasnref = ref (ULexBuffer.lastWasNL (!yystrm))
	    fun continue() = let val yylastwasn = !yylastwasnref in
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Keyword_fun)
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Keyword_val)
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;  Token.Keyword_if)
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Keyword_then)
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Keyword_else)
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Keyword_datatype)
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;  Token.Keyword_of)
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_LAMBDA)
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_ALT)
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_EQ)
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_AND)
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_OR)
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_NOT)
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_GT)
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_LT)
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_GE)
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_LE)
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_NEQ)
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_MULT)
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_DIV)
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_ADD)
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_SUB)
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_MOD)
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_COMMA)
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_GET)
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_LP)
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
       Token.Operator_RP)
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;  Token.SEMICOLON)
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Token.VarName (yytext)
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;
         Token.Operator_QUERY (Substring.string (Substring.trimr 1 yysubstr))
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Token.Number (valOf (Int.fromString yytext))
      end
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;  skip())
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;  continue())
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
          print("error (pos:" ^ (AntlrStreamPos.toString yysm yypos) ^
                  "): Ignore unrecognized character '" ^ yytext ^ "'\n");
            continue() 
      end
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx3A
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ30(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx3A
                  then yyAction1(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction1(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction1(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction1(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ30(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ33(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ32(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ30(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx3A
                  then yyAction3(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction3(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction3(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction3(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ30(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ36(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ35(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx68
                  then yyQ34(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ30(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx3A
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction11(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction11(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ30(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx3A
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction6(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx61
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx61
                  then yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx66
                  then yyQ37(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx73
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx72
                  then yyQ38(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ30(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx3A
                  then yyAction12(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction12(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction12(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction12(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ30(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx70
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx6F
                  then yyQ39(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ30(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx3A
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction2(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ30(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx66
                  then yyQ41(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ30(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx3A
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction0(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ30(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ43(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ42(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ30(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx3A
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction4(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ30(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ46(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx73
                  then yyQ45(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ44(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ30(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx3A
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction5(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction5(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ30(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ53(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx71
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx71
              then if inp = 0wx70
                  then yyQ52(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx7A
              then if inp = 0wx79
                  then yyQ51(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ50(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ49(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ48(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ47(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ30(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx3A
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction10(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction10(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ30(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx64
                  then yyQ55(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx30
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ54(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx3A
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = 0wx3F
                  then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ30(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ56(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ57(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyQ58(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ59(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction30(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ59(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ59(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction30(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ59(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ60(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyAction32(strm, yyNO_MATCH)
              else yyQ61(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyAction32(strm, yyNO_MATCH)
              else yyQ61(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ17(strm', lastMatch)
            else if inp < 0wx3E
              then if inp = 0wx29
                  then yyQ6(strm', lastMatch)
                else if inp < 0wx29
                  then if inp = 0wx21
                      then yyQ1(strm', lastMatch)
                    else if inp < 0wx21
                      then if inp = 0wxD
                          then yyQ2(strm', lastMatch)
                        else if inp < 0wxD
                          then if inp = 0wx9
                              then yyQ2(strm', lastMatch)
                            else if inp < 0wx9
                              then yyQ1(strm', lastMatch)
                            else if inp <= 0wxA
                              then yyQ2(strm', lastMatch)
                              else yyQ1(strm', lastMatch)
                        else if inp = 0wx20
                          then yyQ2(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                    else if inp = 0wx25
                      then yyQ4(strm', lastMatch)
                    else if inp < 0wx25
                      then if inp = 0wx23
                          then yyQ3(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                    else if inp = 0wx28
                      then yyQ5(strm', lastMatch)
                      else yyQ1(strm', lastMatch)
                else if inp = 0wx2F
                  then yyQ11(strm', lastMatch)
                else if inp < 0wx2F
                  then if inp = 0wx2C
                      then yyQ9(strm', lastMatch)
                    else if inp < 0wx2C
                      then if inp = 0wx2A
                          then yyQ7(strm', lastMatch)
                          else yyQ8(strm', lastMatch)
                    else if inp = 0wx2D
                      then yyQ10(strm', lastMatch)
                      else yyQ1(strm', lastMatch)
                else if inp = 0wx3B
                  then yyQ14(strm', lastMatch)
                else if inp < 0wx3B
                  then if inp = 0wx3A
                      then yyQ13(strm', lastMatch)
                      else yyQ12(strm', lastMatch)
                else if inp = 0wx3C
                  then yyQ15(strm', lastMatch)
                  else yyQ16(strm', lastMatch)
            else if inp = 0wx69
              then yyQ24(strm', lastMatch)
            else if inp < 0wx69
              then if inp = 0wx61
                  then yyQ20(strm', lastMatch)
                else if inp < 0wx61
                  then if inp = 0wx5B
                      then yyQ1(strm', lastMatch)
                    else if inp < 0wx5B
                      then if inp <= 0wx40
                          then yyQ1(strm', lastMatch)
                          else yyQ18(strm', lastMatch)
                    else if inp = 0wx5C
                      then yyQ19(strm', lastMatch)
                      else yyQ1(strm', lastMatch)
                else if inp = 0wx65
                  then yyQ22(strm', lastMatch)
                else if inp < 0wx65
                  then if inp = 0wx64
                      then yyQ21(strm', lastMatch)
                      else yyQ18(strm', lastMatch)
                else if inp = 0wx66
                  then yyQ23(strm', lastMatch)
                  else yyQ18(strm', lastMatch)
            else if inp = 0wx75
              then yyQ18(strm', lastMatch)
            else if inp < 0wx75
              then if inp = 0wx6F
                  then yyQ26(strm', lastMatch)
                else if inp < 0wx6F
                  then if inp = 0wx6E
                      then yyQ25(strm', lastMatch)
                      else yyQ18(strm', lastMatch)
                else if inp = 0wx74
                  then yyQ27(strm', lastMatch)
                  else yyQ18(strm', lastMatch)
            else if inp = 0wx7B
              then yyQ1(strm', lastMatch)
            else if inp < 0wx7B
              then if inp = 0wx76
                  then yyQ28(strm', lastMatch)
                  else yyQ18(strm', lastMatch)
            else if inp = 0wx7C
              then yyQ29(strm', lastMatch)
              else yyQ1(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of INITIAL => yyQ0(!(yystrm), yyNO_MATCH)
  (* end case *))
end
end
            and skip() = (yystartPos := yygetPos(); 
			  yylastwasnref := ULexBuffer.lastWasNL (!yystrm);
			  continue())
	    in (continue(), (!yystartPos, yygetPos()), !yystrm, !yyss) end
          in 
            lex()
          end
  in
    type pos = AntlrStreamPos.pos
    type span = AntlrStreamPos.span
    type tok = UserDeclarations.lex_result

    datatype prestrm = STRM of ULexBuffer.stream * 
		(yystart_state * tok * span * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex sm 
(STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex 
(yystrm, ss, sm)
	     val strm' = STRM (yystrm', ref NONE);
	     in 
	       memo := SOME (ss, tok, span, strm', ss');
	       (tok, span, (strm', ss'))
	     end
	   | SOME (ss', tok, span, strm', ss'') => 
	       if ss = ss' then
		 (tok, span, (strm', ss''))
	       else (
		 memo := NONE;
		 lex sm 
(STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify input = (STRM (yystreamify' 0 input, ref NONE), INITIAL)
    fun streamifyReader readFn strm = (STRM (yystreamifyReader' 0 readFn strm, ref NONE), 
				       INITIAL)
    fun streamifyInstream strm = (STRM (yystreamifyInstream' 0 strm, ref NONE), 
				  INITIAL)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end
