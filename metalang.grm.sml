structure 
MetaLangTokens = struct

    datatype token = EOF
      | Verbatim of string
      | SEMICOLON
      | Operator_RP
      | Operator_LP
      | Operator_GET
      | Operator_COMMA
      | Operator_MOD
      | Operator_DIV
      | Operator_SUB
      | Operator_ADD
      | Operator_MULT
      | Operator_NEQ
      | Operator_LE
      | Operator_GE
      | Operator_LT
      | Operator_GT
      | Operator_NOT
      | Operator_OR
      | Operator_AND
      | Operator_EQ
      | Operator_ALT
      | Operator_LAMBDA
      | Keyword_of
      | Keyword_datatype
      | Keyword_else
      | Keyword_then
      | Keyword_if
      | Keyword_val
      | Keyword_fun
      | Operator_QUERY of string
      | Number of int
      | VarName of string

    val allToks = [EOF, SEMICOLON, Operator_RP, Operator_LP, Operator_GET, Operator_COMMA, Operator_MOD, Operator_DIV, Operator_SUB, Operator_ADD, Operator_MULT, Operator_NEQ, Operator_LE, Operator_GE, Operator_LT, Operator_GT, Operator_NOT, Operator_OR, Operator_AND, Operator_EQ, Operator_ALT, Operator_LAMBDA, Keyword_of, Keyword_datatype, Keyword_else, Keyword_then, Keyword_if, Keyword_val, Keyword_fun]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (Verbatim(_)) => "Verbatim"
  | (SEMICOLON) => ";"
  | (Operator_RP) => ")"
  | (Operator_LP) => "("
  | (Operator_GET) => "::"
  | (Operator_COMMA) => ","
  | (Operator_MOD) => "%"
  | (Operator_DIV) => "/"
  | (Operator_SUB) => "-"
  | (Operator_ADD) => "+"
  | (Operator_MULT) => "*"
  | (Operator_NEQ) => "/="
  | (Operator_LE) => "<="
  | (Operator_GE) => ">="
  | (Operator_LT) => "<"
  | (Operator_GT) => ">"
  | (Operator_NOT) => "not"
  | (Operator_OR) => "or"
  | (Operator_AND) => "and"
  | (Operator_EQ) => "="
  | (Operator_ALT) => "|"
  | (Operator_LAMBDA) => "\\"
  | (Keyword_of) => "of"
  | (Keyword_datatype) => "datatype"
  | (Keyword_else) => "else"
  | (Keyword_then) => "then"
  | (Keyword_if) => "if"
  | (Keyword_val) => "val"
  | (Keyword_fun) => "fun"
  | (Operator_QUERY(_)) => "Operator_QUERY"
  | (Number(_)) => "Number"
  | (VarName(_)) => "VarName"
(* end case *))
    fun isKW tok =
(case (tok)
 of (EOF) => false
  | (Verbatim(_)) => false
  | (SEMICOLON) => false
  | (Operator_RP) => false
  | (Operator_LP) => false
  | (Operator_GET) => false
  | (Operator_COMMA) => false
  | (Operator_MOD) => false
  | (Operator_DIV) => false
  | (Operator_SUB) => false
  | (Operator_ADD) => false
  | (Operator_MULT) => false
  | (Operator_NEQ) => false
  | (Operator_LE) => false
  | (Operator_GE) => false
  | (Operator_LT) => false
  | (Operator_GT) => false
  | (Operator_NOT) => false
  | (Operator_OR) => false
  | (Operator_AND) => false
  | (Operator_EQ) => false
  | (Operator_ALT) => false
  | (Operator_LAMBDA) => false
  | (Keyword_of) => true
  | (Keyword_datatype) => true
  | (Keyword_else) => true
  | (Keyword_then) => true
  | (Keyword_if) => true
  | (Keyword_val) => true
  | (Keyword_fun) => true
  | (Operator_QUERY(_)) => false
  | (Number(_)) => false
  | (VarName(_)) => false
(* end case *))


  fun toksToString toks = String.concatWith " " (map toString toks)

  fun isEOF EOF = true
    | isEOF _ = false

end

functor MetaLangParseFn(Lex : ANTLR_LEXER) = struct

  local
    structure Tok = 
MetaLangTokens
    structure UserCode = struct

 
    structure AST = MetaLangAST;
    fun apply_op expr sr =
        List.foldl (fn ((f,x), y) => f(y,x)) expr sr


fun datatype_binding_PROD_1_ACT (SR, VarName, Keyword_datatype, SR_SPAN : (Lex.pos * Lex.pos), VarName_SPAN : (Lex.pos * Lex.pos), Keyword_datatype_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  
            case SR of 
                NONE    => AST.DatatypeBinding (VarName, [])
              | SOME ls => AST.DatatypeBinding (VarName, ls)
        )
fun val_binding_PROD_1_ACT (expression, VarName, Operator_EQ, Keyword_val, expression_SPAN : (Lex.pos * Lex.pos), VarName_SPAN : (Lex.pos * Lex.pos), Operator_EQ_SPAN : (Lex.pos * Lex.pos), Keyword_val_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  AST.ValBinding (VarName, expression) )
fun function_definition_PROD_1_SUBRULE_1_PROD_1_ACT (VarName, Operator_EQ, Operator_LP, Operator_RP, Keyword_fun, compound_expression, Operator_ALT, expression_list, VarName_SPAN : (Lex.pos * Lex.pos), Operator_EQ_SPAN : (Lex.pos * Lex.pos), Operator_LP_SPAN : (Lex.pos * Lex.pos), Operator_RP_SPAN : (Lex.pos * Lex.pos), Keyword_fun_SPAN : (Lex.pos * Lex.pos), compound_expression_SPAN : (Lex.pos * Lex.pos), Operator_ALT_SPAN : (Lex.pos * Lex.pos), expression_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( AST.FunDef (expression_list, compound_expression))
fun function_definition_PROD_1_ACT (SR, VarName, Operator_EQ, Operator_LP, Operator_RP, Keyword_fun, compound_expression, expression_list, SR_SPAN : (Lex.pos * Lex.pos), VarName_SPAN : (Lex.pos * Lex.pos), Operator_EQ_SPAN : (Lex.pos * Lex.pos), Operator_LP_SPAN : (Lex.pos * Lex.pos), Operator_RP_SPAN : (Lex.pos * Lex.pos), Keyword_fun_SPAN : (Lex.pos * Lex.pos), compound_expression_SPAN : (Lex.pos * Lex.pos), expression_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  AST.FunBinding (VarName, AST.FunDef(expression_list, compound_expression) :: SR) )
fun verbatim_PROD_1_ACT (Verbatim, Verbatim_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( AST.Verbatim Verbatim)
fun expression_PROD_2_ACT (expression, argument_list, Operator_EQ, Operator_LP, Operator_RP, Operator_LAMBDA, expression_SPAN : (Lex.pos * Lex.pos), argument_list_SPAN : (Lex.pos * Lex.pos), Operator_EQ_SPAN : (Lex.pos * Lex.pos), Operator_LP_SPAN : (Lex.pos * Lex.pos), Operator_RP_SPAN : (Lex.pos * Lex.pos), Operator_LAMBDA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  AST.Lambda (argument_list, expression) )
fun expression_PROD_3_ACT (expression, compound_expression1, compound_expression2, Keyword_else, Keyword_then, Keyword_if, expression_SPAN : (Lex.pos * Lex.pos), compound_expression1_SPAN : (Lex.pos * Lex.pos), compound_expression2_SPAN : (Lex.pos * Lex.pos), Keyword_else_SPAN : (Lex.pos * Lex.pos), Keyword_then_SPAN : (Lex.pos * Lex.pos), Keyword_if_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  
            AST.IfThenElse (expression, compound_expression1, compound_expression2) )
fun compound_expression_PROD_1_ACT (SR, expression, SR_SPAN : (Lex.pos * Lex.pos), expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( 
        case SR of 
            NONE      => ( AST.CompoundExpr ([], expression) )
          | SOME decl => ( AST.CompoundExpr (decl, expression) ) )
fun expression_list_PROD_1_SUBRULE_1_PROD_1_ACT (SR, expression, SR_SPAN : (Lex.pos * Lex.pos), expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  expression :: SR )
fun expression_list_PROD_1_ACT (SR, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( 
                case SR of 
                     NONE    => []
                   | SOME es => es
                )
fun argument_list_PROD_1_SUBRULE_1_PROD_1_ACT (SR, VarName, SR_SPAN : (Lex.pos * Lex.pos), VarName_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  VarName :: SR )
fun argument_list_PROD_1_ACT (SR, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( 
                case SR of
                    NONE    => []
                  | SOME ss => ss
                )
fun member_list_PROD_1_SUBRULE_1_PROD_1_ACT (SR, VarName, SR_SPAN : (Lex.pos * Lex.pos), VarName_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  VarName :: SR )
fun member_list_PROD_1_ACT (SR, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( 
                case SR of
                    NONE    => []
                  | SOME ss => ss
                )
fun logical_expression_PROD_1_SUBRULE_1_PROD_1_ACT (relative_expression, Operator_AND, relative_expression_SPAN : (Lex.pos * Lex.pos), Operator_AND_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  (AST.opAND, relative_expression) )
fun logical_expression_PROD_1_SUBRULE_1_PROD_2_ACT (Operator_OR, relative_expression, Operator_OR_SPAN : (Lex.pos * Lex.pos), relative_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  (AST.opOR,  relative_expression) )
fun logical_expression_PROD_1_ACT (SR, relative_expression, SR_SPAN : (Lex.pos * Lex.pos), relative_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  apply_op relative_expression SR )
fun relative_expression_PROD_1_SUBRULE_1_PROD_1_ACT (Operator_LT, additive_expression, Operator_LT_SPAN : (Lex.pos * Lex.pos), additive_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  (AST.opLT, additive_expression) )
fun relative_expression_PROD_1_SUBRULE_1_PROD_2_ACT (Operator_GT, additive_expression, Operator_GT_SPAN : (Lex.pos * Lex.pos), additive_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  (AST.opGT, additive_expression) )
fun relative_expression_PROD_1_SUBRULE_1_PROD_3_ACT (Operator_LE, additive_expression, Operator_LE_SPAN : (Lex.pos * Lex.pos), additive_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  (AST.opLE, additive_expression) )
fun relative_expression_PROD_1_SUBRULE_1_PROD_4_ACT (Operator_GE, additive_expression, Operator_GE_SPAN : (Lex.pos * Lex.pos), additive_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  (AST.opGE, additive_expression) )
fun relative_expression_PROD_1_SUBRULE_1_PROD_5_ACT (Operator_EQ, additive_expression, Operator_EQ_SPAN : (Lex.pos * Lex.pos), additive_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  (AST.opEQ, additive_expression) )
fun relative_expression_PROD_1_SUBRULE_1_PROD_6_ACT (additive_expression, Operator_NEQ, additive_expression_SPAN : (Lex.pos * Lex.pos), Operator_NEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  (AST.opNE, additive_expression) )
fun relative_expression_PROD_1_ACT (SR, additive_expression, SR_SPAN : (Lex.pos * Lex.pos), additive_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  apply_op additive_expression SR )
fun additive_expression_PROD_1_SUBRULE_1_PROD_1_ACT (multiplicative_expression, Operator_ADD, multiplicative_expression_SPAN : (Lex.pos * Lex.pos), Operator_ADD_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  (AST.opADD, multiplicative_expression) )
fun additive_expression_PROD_1_SUBRULE_1_PROD_2_ACT (multiplicative_expression, Operator_SUB, multiplicative_expression_SPAN : (Lex.pos * Lex.pos), Operator_SUB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  (AST.opSUB, multiplicative_expression) )
fun additive_expression_PROD_1_ACT (SR, multiplicative_expression, SR_SPAN : (Lex.pos * Lex.pos), multiplicative_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  apply_op multiplicative_expression SR)
fun multiplicative_expression_PROD_1_SUBRULE_1_PROD_1_ACT (Operator_MULT, unary_expression, Operator_MULT_SPAN : (Lex.pos * Lex.pos), unary_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  (AST.opMUL, unary_expression) )
fun multiplicative_expression_PROD_1_SUBRULE_1_PROD_2_ACT (Operator_DIV, unary_expression, Operator_DIV_SPAN : (Lex.pos * Lex.pos), unary_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  (AST.opDIV, unary_expression) )
fun multiplicative_expression_PROD_1_SUBRULE_1_PROD_3_ACT (Operator_MOD, unary_expression, Operator_MOD_SPAN : (Lex.pos * Lex.pos), unary_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  (AST.opMOD, unary_expression) )
fun multiplicative_expression_PROD_1_ACT (SR, unary_expression, SR_SPAN : (Lex.pos * Lex.pos), unary_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  apply_op unary_expression SR )
fun unary_expression_PROD_1_ACT (postfix_expression, Operator_NOT, postfix_expression_SPAN : (Lex.pos * Lex.pos), Operator_NOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  AST.opNOT postfix_expression )
fun unary_expression_PROD_2_ACT (postfix_expression, Operator_SUB, postfix_expression_SPAN : (Lex.pos * Lex.pos), Operator_SUB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  AST.opNEG postfix_expression )
fun unary_expression_PROD_3_ACT (postfix_expression, Operator_QUERY, postfix_expression_SPAN : (Lex.pos * Lex.pos), Operator_QUERY_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  AST.opISA (Operator_QUERY, postfix_expression) )
fun postfix_expression_PROD_1_SUBRULE_1_PROD_1_ACT (SR, primary_expression, SR_SPAN : (Lex.pos * Lex.pos), primary_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  List.foldl (fn (x,y) => AST.opMEM(y,x)) primary_expression SR )
fun postfix_expression_PROD_1_SUBRULE_1_PROD_2_ACT (Operator_LP, Operator_RP, primary_expression, expression_list, Operator_LP_SPAN : (Lex.pos * Lex.pos), Operator_RP_SPAN : (Lex.pos * Lex.pos), primary_expression_SPAN : (Lex.pos * Lex.pos), expression_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  AST.Funcall (primary_expression, expression_list) )
fun postfix_expression_PROD_1_ACT (SR, primary_expression, SR_SPAN : (Lex.pos * Lex.pos), primary_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( 
        case SR of
            NONE      => primary_expression
          | SOME expr => expr
    )
fun primary_expression_PROD_1_ACT (VarName, VarName_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  AST.Identifier VarName )
fun primary_expression_PROD_2_ACT (Number, Number_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  AST.Integer Number )

    end

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)
    structure EBNF = AntlrEBNF(struct
			         type strm = Err.wstream
			         val getSpan = Err.getSpan
			       end)

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) = 
	        (Err.whileDisabled eh (fn() => prod strm)) 
		handle Err.ParseError => try (prods)
          in try prods end
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchVerbatim strm = (case (lex(strm))
 of (Tok.Verbatim(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchSEMICOLON strm = (case (lex(strm))
 of (Tok.SEMICOLON, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_RP strm = (case (lex(strm))
 of (Tok.Operator_RP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_LP strm = (case (lex(strm))
 of (Tok.Operator_LP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_GET strm = (case (lex(strm))
 of (Tok.Operator_GET, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_COMMA strm = (case (lex(strm))
 of (Tok.Operator_COMMA, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_MOD strm = (case (lex(strm))
 of (Tok.Operator_MOD, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_DIV strm = (case (lex(strm))
 of (Tok.Operator_DIV, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_SUB strm = (case (lex(strm))
 of (Tok.Operator_SUB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_ADD strm = (case (lex(strm))
 of (Tok.Operator_ADD, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_MULT strm = (case (lex(strm))
 of (Tok.Operator_MULT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_NEQ strm = (case (lex(strm))
 of (Tok.Operator_NEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_LE strm = (case (lex(strm))
 of (Tok.Operator_LE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_GE strm = (case (lex(strm))
 of (Tok.Operator_GE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_LT strm = (case (lex(strm))
 of (Tok.Operator_LT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_GT strm = (case (lex(strm))
 of (Tok.Operator_GT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_NOT strm = (case (lex(strm))
 of (Tok.Operator_NOT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_OR strm = (case (lex(strm))
 of (Tok.Operator_OR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_AND strm = (case (lex(strm))
 of (Tok.Operator_AND, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_EQ strm = (case (lex(strm))
 of (Tok.Operator_EQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_ALT strm = (case (lex(strm))
 of (Tok.Operator_ALT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_LAMBDA strm = (case (lex(strm))
 of (Tok.Operator_LAMBDA, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKeyword_of strm = (case (lex(strm))
 of (Tok.Keyword_of, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKeyword_datatype strm = (case (lex(strm))
 of (Tok.Keyword_datatype, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKeyword_else strm = (case (lex(strm))
 of (Tok.Keyword_else, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKeyword_then strm = (case (lex(strm))
 of (Tok.Keyword_then, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKeyword_if strm = (case (lex(strm))
 of (Tok.Keyword_if, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKeyword_val strm = (case (lex(strm))
 of (Tok.Keyword_val, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKeyword_fun strm = (case (lex(strm))
 of (Tok.Keyword_fun, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOperator_QUERY strm = (case (lex(strm))
 of (Tok.Operator_QUERY(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchNumber strm = (case (lex(strm))
 of (Tok.Number(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchVarName strm = (case (lex(strm))
 of (Tok.VarName(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))

val (stmts_NT) = 
let
fun verbatim_NT (strm) = let
      val (Verbatim_RES, Verbatim_SPAN, strm') = matchVerbatim(strm)
      val FULL_SPAN = (#1(Verbatim_SPAN), #2(Verbatim_SPAN))
      in
        (UserCode.verbatim_PROD_1_ACT (Verbatim_RES, Verbatim_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun argument_list_NT (strm) = let
      fun argument_list_PROD_1_SUBRULE_1_NT (strm) = let
            val (VarName_RES, VarName_SPAN, strm') = matchVarName(strm)
            fun argument_list_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  val (Operator_COMMA_RES, Operator_COMMA_SPAN, strm') = matchOperator_COMMA(strm)
                  val (VarName_RES, VarName_SPAN, strm') = matchVarName(strm')
                  val FULL_SPAN = (#1(Operator_COMMA_SPAN), #2(VarName_SPAN))
                  in
                    ((VarName_RES), FULL_SPAN, strm')
                  end
            fun argument_list_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.Operator_COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(argument_list_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED, argument_list_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(VarName_SPAN), #2(SR_SPAN))
            in
              (UserCode.argument_list_PROD_1_SUBRULE_1_PROD_1_ACT (SR_RES, VarName_RES, SR_SPAN : (Lex.pos * Lex.pos), VarName_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun argument_list_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.VarName(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(argument_list_PROD_1_SUBRULE_1_PRED, argument_list_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
      in
        (UserCode.argument_list_PROD_1_ACT (SR_RES, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun member_list_NT (strm) = let
      fun member_list_PROD_1_SUBRULE_1_NT (strm) = let
            val (VarName_RES, VarName_SPAN, strm') = matchVarName(strm)
            fun member_list_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  val (Operator_MULT_RES, Operator_MULT_SPAN, strm') = matchOperator_MULT(strm)
                  val (VarName_RES, VarName_SPAN, strm') = matchVarName(strm')
                  val FULL_SPAN = (#1(Operator_MULT_SPAN), #2(VarName_SPAN))
                  in
                    ((VarName_RES), FULL_SPAN, strm')
                  end
            fun member_list_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.Operator_MULT, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(member_list_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED, member_list_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(VarName_SPAN), #2(SR_SPAN))
            in
              (UserCode.member_list_PROD_1_SUBRULE_1_PROD_1_ACT (SR_RES, VarName_RES, SR_SPAN : (Lex.pos * Lex.pos), VarName_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun member_list_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.VarName(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(member_list_PROD_1_SUBRULE_1_PRED, member_list_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
      in
        (UserCode.member_list_PROD_1_ACT (SR_RES, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun datatype_binding_NT (strm) = let
      val (Keyword_datatype_RES, Keyword_datatype_SPAN, strm') = matchKeyword_datatype(strm)
      val (VarName_RES, VarName_SPAN, strm') = matchVarName(strm')
      fun datatype_binding_PROD_1_SUBRULE_1_NT (strm) = let
            val (Keyword_of_RES, Keyword_of_SPAN, strm') = matchKeyword_of(strm)
            val (member_list_RES, member_list_SPAN, strm') = member_list_NT(strm')
            val FULL_SPAN = (#1(Keyword_of_SPAN), #2(member_list_SPAN))
            in
              ((member_list_RES), FULL_SPAN, strm')
            end
      fun datatype_binding_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.Keyword_of, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(datatype_binding_PROD_1_SUBRULE_1_PRED, datatype_binding_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(Keyword_datatype_SPAN), #2(SR_SPAN))
      in
        (UserCode.datatype_binding_PROD_1_ACT (SR_RES, VarName_RES, Keyword_datatype_RES, SR_SPAN : (Lex.pos * Lex.pos), VarName_SPAN : (Lex.pos * Lex.pos), Keyword_datatype_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun statement_NT (strm) = let
      val (SR_RES, SR_SPAN, strm') = let
      fun statement_PROD_1_SUBRULE_1_NT (strm) = let
            fun statement_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (datatype_binding_RES, datatype_binding_SPAN, strm') = datatype_binding_NT(strm)
                  val FULL_SPAN = (#1(datatype_binding_SPAN),
                    #2(datatype_binding_SPAN))
                  in
                    ((datatype_binding_RES), FULL_SPAN, strm')
                  end
            fun statement_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (val_binding_RES, val_binding_SPAN, strm') = val_binding_NT(strm)
                  val FULL_SPAN = (#1(val_binding_SPAN), #2(val_binding_SPAN))
                  in
                    ((val_binding_RES), FULL_SPAN, strm')
                  end
            fun statement_PROD_1_SUBRULE_1_PROD_3 (strm) = let
                  val (function_definition_RES, function_definition_SPAN, strm') = function_definition_NT(strm)
                  val FULL_SPAN = (#1(function_definition_SPAN),
                    #2(function_definition_SPAN))
                  in
                    ((function_definition_RES), FULL_SPAN, strm')
                  end
            fun statement_PROD_1_SUBRULE_1_PROD_4 (strm) = let
                  val (verbatim_RES, verbatim_SPAN, strm') = verbatim_NT(strm)
                  val FULL_SPAN = (#1(verbatim_SPAN), #2(verbatim_SPAN))
                  in
                    ((verbatim_RES), FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.Verbatim(_), _, strm') =>
                    statement_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.Keyword_val, _, strm') =>
                    statement_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.Keyword_datatype, _, strm') =>
                    statement_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.Keyword_fun, _, strm') =>
                    statement_PROD_1_SUBRULE_1_PROD_3(strm)
                | _ => fail()
              (* end case *))
            end
      in
        statement_PROD_1_SUBRULE_1_NT(strm)
      end
      val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
      in
        ((SR_RES), FULL_SPAN, strm')
      end
and function_definition_NT (strm) = let
      val (Keyword_fun_RES, Keyword_fun_SPAN, strm') = matchKeyword_fun(strm)
      val (VarName_RES, VarName_SPAN, strm') = matchVarName(strm')
      val (Operator_LP_RES, Operator_LP_SPAN, strm') = matchOperator_LP(strm')
      val (expression_list_RES, expression_list_SPAN, strm') = expression_list_NT(strm')
      val (Operator_RP_RES, Operator_RP_SPAN, strm') = matchOperator_RP(strm')
      val (Operator_EQ_RES, Operator_EQ_SPAN, strm') = matchOperator_EQ(strm')
      val (compound_expression_RES, compound_expression_SPAN, strm') = compound_expression_NT(strm')
      fun function_definition_PROD_1_SUBRULE_1_NT (strm) = let
            val (Operator_ALT_RES, Operator_ALT_SPAN, strm') = matchOperator_ALT(strm)
            val (Operator_LP_RES, Operator_LP_SPAN, strm') = matchOperator_LP(strm')
            val (expression_list_RES, expression_list_SPAN, strm') = expression_list_NT(strm')
            val (Operator_RP_RES, Operator_RP_SPAN, strm') = matchOperator_RP(strm')
            val (Operator_EQ_RES, Operator_EQ_SPAN, strm') = matchOperator_EQ(strm')
            val (compound_expression_RES, compound_expression_SPAN, strm') = compound_expression_NT(strm')
            val FULL_SPAN = (#1(Operator_ALT_SPAN),
              #2(compound_expression_SPAN))
            in
              (UserCode.function_definition_PROD_1_SUBRULE_1_PROD_1_ACT (VarName_RES, Operator_EQ_RES, Operator_LP_RES, Operator_RP_RES, Keyword_fun_RES, compound_expression_RES, Operator_ALT_RES, expression_list_RES, VarName_SPAN : (Lex.pos * Lex.pos), Operator_EQ_SPAN : (Lex.pos * Lex.pos), Operator_LP_SPAN : (Lex.pos * Lex.pos), Operator_RP_SPAN : (Lex.pos * Lex.pos), Keyword_fun_SPAN : (Lex.pos * Lex.pos), compound_expression_SPAN : (Lex.pos * Lex.pos), Operator_ALT_SPAN : (Lex.pos * Lex.pos), expression_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun function_definition_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.Operator_ALT, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(function_definition_PROD_1_SUBRULE_1_PRED, function_definition_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(Keyword_fun_SPAN), #2(SR_SPAN))
      in
        (UserCode.function_definition_PROD_1_ACT (SR_RES, VarName_RES, Operator_EQ_RES, Operator_LP_RES, Operator_RP_RES, Keyword_fun_RES, compound_expression_RES, expression_list_RES, SR_SPAN : (Lex.pos * Lex.pos), VarName_SPAN : (Lex.pos * Lex.pos), Operator_EQ_SPAN : (Lex.pos * Lex.pos), Operator_LP_SPAN : (Lex.pos * Lex.pos), Operator_RP_SPAN : (Lex.pos * Lex.pos), Keyword_fun_SPAN : (Lex.pos * Lex.pos), compound_expression_SPAN : (Lex.pos * Lex.pos), expression_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and compound_expression_NT (strm) = let
      fun compound_expression_PROD_1_SUBRULE_1_NT (strm) = let
            fun compound_expression_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  val (statement_RES, statement_SPAN, strm') = statement_NT(strm)
                  val FULL_SPAN = (#1(statement_SPAN), #2(statement_SPAN))
                  in
                    ((statement_RES), FULL_SPAN, strm')
                  end
            fun compound_expression_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.Keyword_fun, _, strm') => true
                    | (Tok.Keyword_val, _, strm') => true
                    | (Tok.Keyword_datatype, _, strm') => true
                    | (Tok.Verbatim(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.posclos(compound_expression_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED, compound_expression_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT, strm)
            val (SEMICOLON_RES, SEMICOLON_SPAN, strm') = matchSEMICOLON(strm')
            val FULL_SPAN = (#1(SR_SPAN), #2(SEMICOLON_SPAN))
            in
              ((SR_RES), FULL_SPAN, strm')
            end
      fun compound_expression_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.Keyword_fun, _, strm') => true
              | (Tok.Keyword_val, _, strm') => true
              | (Tok.Keyword_datatype, _, strm') => true
              | (Tok.Verbatim(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(compound_expression_PROD_1_SUBRULE_1_PRED, compound_expression_PROD_1_SUBRULE_1_NT, strm)
      val (expression_RES, expression_SPAN, strm') = expression_NT(strm')
      val FULL_SPAN = (#1(SR_SPAN), #2(expression_SPAN))
      in
        (UserCode.compound_expression_PROD_1_ACT (SR_RES, expression_RES, SR_SPAN : (Lex.pos * Lex.pos), expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and expression_NT (strm) = let
      fun expression_PROD_1 (strm) = let
            val (logical_expression_RES, logical_expression_SPAN, strm') = logical_expression_NT(strm)
            val FULL_SPAN = (#1(logical_expression_SPAN),
              #2(logical_expression_SPAN))
            in
              ((logical_expression_RES), FULL_SPAN, strm')
            end
      fun expression_PROD_2 (strm) = let
            val (Operator_LAMBDA_RES, Operator_LAMBDA_SPAN, strm') = matchOperator_LAMBDA(strm)
            val (Operator_LP_RES, Operator_LP_SPAN, strm') = matchOperator_LP(strm')
            val (argument_list_RES, argument_list_SPAN, strm') = argument_list_NT(strm')
            val (Operator_RP_RES, Operator_RP_SPAN, strm') = matchOperator_RP(strm')
            val (Operator_EQ_RES, Operator_EQ_SPAN, strm') = matchOperator_EQ(strm')
            val (expression_RES, expression_SPAN, strm') = expression_NT(strm')
            val FULL_SPAN = (#1(Operator_LAMBDA_SPAN), #2(expression_SPAN))
            in
              (UserCode.expression_PROD_2_ACT (expression_RES, argument_list_RES, Operator_EQ_RES, Operator_LP_RES, Operator_RP_RES, Operator_LAMBDA_RES, expression_SPAN : (Lex.pos * Lex.pos), argument_list_SPAN : (Lex.pos * Lex.pos), Operator_EQ_SPAN : (Lex.pos * Lex.pos), Operator_LP_SPAN : (Lex.pos * Lex.pos), Operator_RP_SPAN : (Lex.pos * Lex.pos), Operator_LAMBDA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun expression_PROD_3 (strm) = let
            val (Keyword_if_RES, Keyword_if_SPAN, strm') = matchKeyword_if(strm)
            val (expression_RES, expression_SPAN, strm') = expression_NT(strm')
            val (Keyword_then_RES, Keyword_then_SPAN, strm') = matchKeyword_then(strm')
            val (compound_expression1_RES, compound_expression1_SPAN, strm') = compound_expression_NT(strm')
            val (Keyword_else_RES, Keyword_else_SPAN, strm') = matchKeyword_else(strm')
            val (compound_expression2_RES, compound_expression2_SPAN, strm') = compound_expression_NT(strm')
            val FULL_SPAN = (#1(Keyword_if_SPAN),
              #2(compound_expression2_SPAN))
            in
              (UserCode.expression_PROD_3_ACT (expression_RES, compound_expression1_RES, compound_expression2_RES, Keyword_else_RES, Keyword_then_RES, Keyword_if_RES, expression_SPAN : (Lex.pos * Lex.pos), compound_expression1_SPAN : (Lex.pos * Lex.pos), compound_expression2_SPAN : (Lex.pos * Lex.pos), Keyword_else_SPAN : (Lex.pos * Lex.pos), Keyword_then_SPAN : (Lex.pos * Lex.pos), Keyword_if_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.Keyword_if, _, strm') => expression_PROD_3(strm)
          | (Tok.VarName(_), _, strm') => expression_PROD_1(strm)
          | (Tok.Number(_), _, strm') => expression_PROD_1(strm)
          | (Tok.Operator_QUERY(_), _, strm') => expression_PROD_1(strm)
          | (Tok.Operator_NOT, _, strm') => expression_PROD_1(strm)
          | (Tok.Operator_SUB, _, strm') => expression_PROD_1(strm)
          | (Tok.Operator_LP, _, strm') => expression_PROD_1(strm)
          | (Tok.Operator_LAMBDA, _, strm') => expression_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and logical_expression_NT (strm) = let
      val (relative_expression_RES, relative_expression_SPAN, strm') = relative_expression_NT(strm)
      fun logical_expression_PROD_1_SUBRULE_1_NT (strm) = let
            fun logical_expression_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (Operator_AND_RES, Operator_AND_SPAN, strm') = matchOperator_AND(strm)
                  val (relative_expression_RES, relative_expression_SPAN, strm') = relative_expression_NT(strm')
                  val FULL_SPAN = (#1(Operator_AND_SPAN),
                    #2(relative_expression_SPAN))
                  in
                    (UserCode.logical_expression_PROD_1_SUBRULE_1_PROD_1_ACT (relative_expression_RES, Operator_AND_RES, relative_expression_SPAN : (Lex.pos * Lex.pos), Operator_AND_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun logical_expression_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (Operator_OR_RES, Operator_OR_SPAN, strm') = matchOperator_OR(strm)
                  val (relative_expression_RES, relative_expression_SPAN, strm') = relative_expression_NT(strm')
                  val FULL_SPAN = (#1(Operator_OR_SPAN),
                    #2(relative_expression_SPAN))
                  in
                    (UserCode.logical_expression_PROD_1_SUBRULE_1_PROD_2_ACT (Operator_OR_RES, relative_expression_RES, Operator_OR_SPAN : (Lex.pos * Lex.pos), relative_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.Operator_OR, _, strm') =>
                    logical_expression_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.Operator_AND, _, strm') =>
                    logical_expression_PROD_1_SUBRULE_1_PROD_1(strm)
                | _ => fail()
              (* end case *))
            end
      fun logical_expression_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.Operator_AND, _, strm') => true
              | (Tok.Operator_OR, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(logical_expression_PROD_1_SUBRULE_1_PRED, logical_expression_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(relative_expression_SPAN), #2(SR_SPAN))
      in
        (UserCode.logical_expression_PROD_1_ACT (SR_RES, relative_expression_RES, SR_SPAN : (Lex.pos * Lex.pos), relative_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and relative_expression_NT (strm) = let
      val (additive_expression_RES, additive_expression_SPAN, strm') = additive_expression_NT(strm)
      fun relative_expression_PROD_1_SUBRULE_1_NT (strm) = let
            fun relative_expression_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (Operator_LT_RES, Operator_LT_SPAN, strm') = matchOperator_LT(strm)
                  val (additive_expression_RES, additive_expression_SPAN, strm') = additive_expression_NT(strm')
                  val FULL_SPAN = (#1(Operator_LT_SPAN),
                    #2(additive_expression_SPAN))
                  in
                    (UserCode.relative_expression_PROD_1_SUBRULE_1_PROD_1_ACT (Operator_LT_RES, additive_expression_RES, Operator_LT_SPAN : (Lex.pos * Lex.pos), additive_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun relative_expression_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (Operator_GT_RES, Operator_GT_SPAN, strm') = matchOperator_GT(strm)
                  val (additive_expression_RES, additive_expression_SPAN, strm') = additive_expression_NT(strm')
                  val FULL_SPAN = (#1(Operator_GT_SPAN),
                    #2(additive_expression_SPAN))
                  in
                    (UserCode.relative_expression_PROD_1_SUBRULE_1_PROD_2_ACT (Operator_GT_RES, additive_expression_RES, Operator_GT_SPAN : (Lex.pos * Lex.pos), additive_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun relative_expression_PROD_1_SUBRULE_1_PROD_3 (strm) = let
                  val (Operator_LE_RES, Operator_LE_SPAN, strm') = matchOperator_LE(strm)
                  val (additive_expression_RES, additive_expression_SPAN, strm') = additive_expression_NT(strm')
                  val FULL_SPAN = (#1(Operator_LE_SPAN),
                    #2(additive_expression_SPAN))
                  in
                    (UserCode.relative_expression_PROD_1_SUBRULE_1_PROD_3_ACT (Operator_LE_RES, additive_expression_RES, Operator_LE_SPAN : (Lex.pos * Lex.pos), additive_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun relative_expression_PROD_1_SUBRULE_1_PROD_4 (strm) = let
                  val (Operator_GE_RES, Operator_GE_SPAN, strm') = matchOperator_GE(strm)
                  val (additive_expression_RES, additive_expression_SPAN, strm') = additive_expression_NT(strm')
                  val FULL_SPAN = (#1(Operator_GE_SPAN),
                    #2(additive_expression_SPAN))
                  in
                    (UserCode.relative_expression_PROD_1_SUBRULE_1_PROD_4_ACT (Operator_GE_RES, additive_expression_RES, Operator_GE_SPAN : (Lex.pos * Lex.pos), additive_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun relative_expression_PROD_1_SUBRULE_1_PROD_5 (strm) = let
                  val (Operator_EQ_RES, Operator_EQ_SPAN, strm') = matchOperator_EQ(strm)
                  val (additive_expression_RES, additive_expression_SPAN, strm') = additive_expression_NT(strm')
                  val FULL_SPAN = (#1(Operator_EQ_SPAN),
                    #2(additive_expression_SPAN))
                  in
                    (UserCode.relative_expression_PROD_1_SUBRULE_1_PROD_5_ACT (Operator_EQ_RES, additive_expression_RES, Operator_EQ_SPAN : (Lex.pos * Lex.pos), additive_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun relative_expression_PROD_1_SUBRULE_1_PROD_6 (strm) = let
                  val (Operator_NEQ_RES, Operator_NEQ_SPAN, strm') = matchOperator_NEQ(strm)
                  val (additive_expression_RES, additive_expression_SPAN, strm') = additive_expression_NT(strm')
                  val FULL_SPAN = (#1(Operator_NEQ_SPAN),
                    #2(additive_expression_SPAN))
                  in
                    (UserCode.relative_expression_PROD_1_SUBRULE_1_PROD_6_ACT (additive_expression_RES, Operator_NEQ_RES, additive_expression_SPAN : (Lex.pos * Lex.pos), Operator_NEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.Operator_NEQ, _, strm') =>
                    relative_expression_PROD_1_SUBRULE_1_PROD_6(strm)
                | (Tok.Operator_GE, _, strm') =>
                    relative_expression_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.Operator_GT, _, strm') =>
                    relative_expression_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.Operator_LT, _, strm') =>
                    relative_expression_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.Operator_LE, _, strm') =>
                    relative_expression_PROD_1_SUBRULE_1_PROD_3(strm)
                | (Tok.Operator_EQ, _, strm') =>
                    relative_expression_PROD_1_SUBRULE_1_PROD_5(strm)
                | _ => fail()
              (* end case *))
            end
      fun relative_expression_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.Operator_EQ, _, strm') => true
              | (Tok.Operator_GT, _, strm') => true
              | (Tok.Operator_LT, _, strm') => true
              | (Tok.Operator_GE, _, strm') => true
              | (Tok.Operator_LE, _, strm') => true
              | (Tok.Operator_NEQ, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(relative_expression_PROD_1_SUBRULE_1_PRED, relative_expression_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(additive_expression_SPAN), #2(SR_SPAN))
      in
        (UserCode.relative_expression_PROD_1_ACT (SR_RES, additive_expression_RES, SR_SPAN : (Lex.pos * Lex.pos), additive_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and additive_expression_NT (strm) = let
      val (multiplicative_expression_RES, multiplicative_expression_SPAN, strm') = multiplicative_expression_NT(strm)
      fun additive_expression_PROD_1_SUBRULE_1_NT (strm) = let
            fun additive_expression_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (Operator_ADD_RES, Operator_ADD_SPAN, strm') = matchOperator_ADD(strm)
                  val (multiplicative_expression_RES, multiplicative_expression_SPAN, strm') = multiplicative_expression_NT(strm')
                  val FULL_SPAN = (#1(Operator_ADD_SPAN),
                    #2(multiplicative_expression_SPAN))
                  in
                    (UserCode.additive_expression_PROD_1_SUBRULE_1_PROD_1_ACT (multiplicative_expression_RES, Operator_ADD_RES, multiplicative_expression_SPAN : (Lex.pos * Lex.pos), Operator_ADD_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun additive_expression_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (Operator_SUB_RES, Operator_SUB_SPAN, strm') = matchOperator_SUB(strm)
                  val (multiplicative_expression_RES, multiplicative_expression_SPAN, strm') = multiplicative_expression_NT(strm')
                  val FULL_SPAN = (#1(Operator_SUB_SPAN),
                    #2(multiplicative_expression_SPAN))
                  in
                    (UserCode.additive_expression_PROD_1_SUBRULE_1_PROD_2_ACT (multiplicative_expression_RES, Operator_SUB_RES, multiplicative_expression_SPAN : (Lex.pos * Lex.pos), Operator_SUB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.Operator_SUB, _, strm') =>
                    additive_expression_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.Operator_ADD, _, strm') =>
                    additive_expression_PROD_1_SUBRULE_1_PROD_1(strm)
                | _ => fail()
              (* end case *))
            end
      fun additive_expression_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.Operator_ADD, _, strm') => true
              | (Tok.Operator_SUB, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(additive_expression_PROD_1_SUBRULE_1_PRED, additive_expression_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(multiplicative_expression_SPAN), #2(SR_SPAN))
      in
        (UserCode.additive_expression_PROD_1_ACT (SR_RES, multiplicative_expression_RES, SR_SPAN : (Lex.pos * Lex.pos), multiplicative_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and multiplicative_expression_NT (strm) = let
      val (unary_expression_RES, unary_expression_SPAN, strm') = unary_expression_NT(strm)
      fun multiplicative_expression_PROD_1_SUBRULE_1_NT (strm) = let
            fun multiplicative_expression_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (Operator_MULT_RES, Operator_MULT_SPAN, strm') = matchOperator_MULT(strm)
                  val (unary_expression_RES, unary_expression_SPAN, strm') = unary_expression_NT(strm')
                  val FULL_SPAN = (#1(Operator_MULT_SPAN),
                    #2(unary_expression_SPAN))
                  in
                    (UserCode.multiplicative_expression_PROD_1_SUBRULE_1_PROD_1_ACT (Operator_MULT_RES, unary_expression_RES, Operator_MULT_SPAN : (Lex.pos * Lex.pos), unary_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun multiplicative_expression_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (Operator_DIV_RES, Operator_DIV_SPAN, strm') = matchOperator_DIV(strm)
                  val (unary_expression_RES, unary_expression_SPAN, strm') = unary_expression_NT(strm')
                  val FULL_SPAN = (#1(Operator_DIV_SPAN),
                    #2(unary_expression_SPAN))
                  in
                    (UserCode.multiplicative_expression_PROD_1_SUBRULE_1_PROD_2_ACT (Operator_DIV_RES, unary_expression_RES, Operator_DIV_SPAN : (Lex.pos * Lex.pos), unary_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun multiplicative_expression_PROD_1_SUBRULE_1_PROD_3 (strm) = let
                  val (Operator_MOD_RES, Operator_MOD_SPAN, strm') = matchOperator_MOD(strm)
                  val (unary_expression_RES, unary_expression_SPAN, strm') = unary_expression_NT(strm')
                  val FULL_SPAN = (#1(Operator_MOD_SPAN),
                    #2(unary_expression_SPAN))
                  in
                    (UserCode.multiplicative_expression_PROD_1_SUBRULE_1_PROD_3_ACT (Operator_MOD_RES, unary_expression_RES, Operator_MOD_SPAN : (Lex.pos * Lex.pos), unary_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.Operator_MOD, _, strm') =>
                    multiplicative_expression_PROD_1_SUBRULE_1_PROD_3(strm)
                | (Tok.Operator_MULT, _, strm') =>
                    multiplicative_expression_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.Operator_DIV, _, strm') =>
                    multiplicative_expression_PROD_1_SUBRULE_1_PROD_2(strm)
                | _ => fail()
              (* end case *))
            end
      fun multiplicative_expression_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.Operator_MULT, _, strm') => true
              | (Tok.Operator_DIV, _, strm') => true
              | (Tok.Operator_MOD, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(multiplicative_expression_PROD_1_SUBRULE_1_PRED, multiplicative_expression_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(unary_expression_SPAN), #2(SR_SPAN))
      in
        (UserCode.multiplicative_expression_PROD_1_ACT (SR_RES, unary_expression_RES, SR_SPAN : (Lex.pos * Lex.pos), unary_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and unary_expression_NT (strm) = let
      fun unary_expression_PROD_1 (strm) = let
            val (Operator_NOT_RES, Operator_NOT_SPAN, strm') = matchOperator_NOT(strm)
            val (postfix_expression_RES, postfix_expression_SPAN, strm') = postfix_expression_NT(strm')
            val FULL_SPAN = (#1(Operator_NOT_SPAN),
              #2(postfix_expression_SPAN))
            in
              (UserCode.unary_expression_PROD_1_ACT (postfix_expression_RES, Operator_NOT_RES, postfix_expression_SPAN : (Lex.pos * Lex.pos), Operator_NOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun unary_expression_PROD_2 (strm) = let
            val (Operator_SUB_RES, Operator_SUB_SPAN, strm') = matchOperator_SUB(strm)
            val (postfix_expression_RES, postfix_expression_SPAN, strm') = postfix_expression_NT(strm')
            val FULL_SPAN = (#1(Operator_SUB_SPAN),
              #2(postfix_expression_SPAN))
            in
              (UserCode.unary_expression_PROD_2_ACT (postfix_expression_RES, Operator_SUB_RES, postfix_expression_SPAN : (Lex.pos * Lex.pos), Operator_SUB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun unary_expression_PROD_3 (strm) = let
            val (Operator_QUERY_RES, Operator_QUERY_SPAN, strm') = matchOperator_QUERY(strm)
            val (postfix_expression_RES, postfix_expression_SPAN, strm') = postfix_expression_NT(strm')
            val FULL_SPAN = (#1(Operator_QUERY_SPAN),
              #2(postfix_expression_SPAN))
            in
              (UserCode.unary_expression_PROD_3_ACT (postfix_expression_RES, Operator_QUERY_RES, postfix_expression_SPAN : (Lex.pos * Lex.pos), Operator_QUERY_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun unary_expression_PROD_4 (strm) = let
            val (postfix_expression_RES, postfix_expression_SPAN, strm') = postfix_expression_NT(strm)
            val FULL_SPAN = (#1(postfix_expression_SPAN),
              #2(postfix_expression_SPAN))
            in
              ((postfix_expression_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.VarName(_), _, strm') => unary_expression_PROD_4(strm)
          | (Tok.Number(_), _, strm') => unary_expression_PROD_4(strm)
          | (Tok.Operator_LP, _, strm') => unary_expression_PROD_4(strm)
          | (Tok.Operator_SUB, _, strm') => unary_expression_PROD_2(strm)
          | (Tok.Operator_NOT, _, strm') => unary_expression_PROD_1(strm)
          | (Tok.Operator_QUERY(_), _, strm') => unary_expression_PROD_3(strm)
          | _ => fail()
        (* end case *))
      end
and postfix_expression_NT (strm) = let
      val (primary_expression_RES, primary_expression_SPAN, strm') = primary_expression_NT(strm)
      fun postfix_expression_PROD_1_SUBRULE_1_NT (strm) = let
            fun postfix_expression_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  fun postfix_expression_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                        val (Operator_GET_RES, Operator_GET_SPAN, strm') = matchOperator_GET(strm)
                        val (VarName_RES, VarName_SPAN, strm') = matchVarName(strm')
                        val FULL_SPAN = (#1(Operator_GET_SPAN),
                          #2(VarName_SPAN))
                        in
                          ((VarName_RES), FULL_SPAN, strm')
                        end
                  fun postfix_expression_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                         of (Tok.Operator_GET, _, strm') => true
                          | _ => false
                        (* end case *))
                  val (SR_RES, SR_SPAN, strm') = EBNF.posclos(postfix_expression_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED, postfix_expression_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT, strm)
                  val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
                  in
                    (UserCode.postfix_expression_PROD_1_SUBRULE_1_PROD_1_ACT (SR_RES, primary_expression_RES, SR_SPAN : (Lex.pos * Lex.pos), primary_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun postfix_expression_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (Operator_LP_RES, Operator_LP_SPAN, strm') = matchOperator_LP(strm)
                  val (expression_list_RES, expression_list_SPAN, strm') = expression_list_NT(strm')
                  val (Operator_RP_RES, Operator_RP_SPAN, strm') = matchOperator_RP(strm')
                  val FULL_SPAN = (#1(Operator_LP_SPAN), #2(Operator_RP_SPAN))
                  in
                    (UserCode.postfix_expression_PROD_1_SUBRULE_1_PROD_2_ACT (Operator_LP_RES, Operator_RP_RES, primary_expression_RES, expression_list_RES, Operator_LP_SPAN : (Lex.pos * Lex.pos), Operator_RP_SPAN : (Lex.pos * Lex.pos), primary_expression_SPAN : (Lex.pos * Lex.pos), expression_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.Operator_LP, _, strm') =>
                    postfix_expression_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.Operator_GET, _, strm') =>
                    postfix_expression_PROD_1_SUBRULE_1_PROD_1(strm)
                | _ => fail()
              (* end case *))
            end
      fun postfix_expression_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.Operator_GET, _, strm') => true
              | (Tok.Operator_LP, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(postfix_expression_PROD_1_SUBRULE_1_PRED, postfix_expression_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(primary_expression_SPAN), #2(SR_SPAN))
      in
        (UserCode.postfix_expression_PROD_1_ACT (SR_RES, primary_expression_RES, SR_SPAN : (Lex.pos * Lex.pos), primary_expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and expression_list_NT (strm) = let
      fun expression_list_PROD_1_SUBRULE_1_NT (strm) = let
            val (expression_RES, expression_SPAN, strm') = expression_NT(strm)
            fun expression_list_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  val (Operator_COMMA_RES, Operator_COMMA_SPAN, strm') = matchOperator_COMMA(strm)
                  val (expression_RES, expression_SPAN, strm') = expression_NT(strm')
                  val FULL_SPAN = (#1(Operator_COMMA_SPAN),
                    #2(expression_SPAN))
                  in
                    ((expression_RES), FULL_SPAN, strm')
                  end
            fun expression_list_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.Operator_COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(expression_list_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED, expression_list_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(expression_SPAN), #2(SR_SPAN))
            in
              (UserCode.expression_list_PROD_1_SUBRULE_1_PROD_1_ACT (SR_RES, expression_RES, SR_SPAN : (Lex.pos * Lex.pos), expression_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun expression_list_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.VarName(_), _, strm') => true
              | (Tok.Number(_), _, strm') => true
              | (Tok.Operator_QUERY(_), _, strm') => true
              | (Tok.Keyword_if, _, strm') => true
              | (Tok.Operator_LAMBDA, _, strm') => true
              | (Tok.Operator_NOT, _, strm') => true
              | (Tok.Operator_SUB, _, strm') => true
              | (Tok.Operator_LP, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(expression_list_PROD_1_SUBRULE_1_PRED, expression_list_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
      in
        (UserCode.expression_list_PROD_1_ACT (SR_RES, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and primary_expression_NT (strm) = let
      fun primary_expression_PROD_1 (strm) = let
            val (VarName_RES, VarName_SPAN, strm') = matchVarName(strm)
            val FULL_SPAN = (#1(VarName_SPAN), #2(VarName_SPAN))
            in
              (UserCode.primary_expression_PROD_1_ACT (VarName_RES, VarName_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun primary_expression_PROD_2 (strm) = let
            val (Number_RES, Number_SPAN, strm') = matchNumber(strm)
            val FULL_SPAN = (#1(Number_SPAN), #2(Number_SPAN))
            in
              (UserCode.primary_expression_PROD_2_ACT (Number_RES, Number_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun primary_expression_PROD_3 (strm) = let
            val (Operator_LP_RES, Operator_LP_SPAN, strm') = matchOperator_LP(strm)
            val (expression_RES, expression_SPAN, strm') = expression_NT(strm')
            val (Operator_RP_RES, Operator_RP_SPAN, strm') = matchOperator_RP(strm')
            val FULL_SPAN = (#1(Operator_LP_SPAN), #2(Operator_RP_SPAN))
            in
              ((expression_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.Operator_LP, _, strm') => primary_expression_PROD_3(strm)
          | (Tok.VarName(_), _, strm') => primary_expression_PROD_1(strm)
          | (Tok.Number(_), _, strm') => primary_expression_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and val_binding_NT (strm) = let
      val (Keyword_val_RES, Keyword_val_SPAN, strm') = matchKeyword_val(strm)
      val (VarName_RES, VarName_SPAN, strm') = matchVarName(strm')
      val (Operator_EQ_RES, Operator_EQ_SPAN, strm') = matchOperator_EQ(strm')
      val (expression_RES, expression_SPAN, strm') = expression_NT(strm')
      val FULL_SPAN = (#1(Keyword_val_SPAN), #2(expression_SPAN))
      in
        (UserCode.val_binding_PROD_1_ACT (expression_RES, VarName_RES, Operator_EQ_RES, Keyword_val_RES, expression_SPAN : (Lex.pos * Lex.pos), VarName_SPAN : (Lex.pos * Lex.pos), Operator_EQ_SPAN : (Lex.pos * Lex.pos), Keyword_val_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun stmts_NT (strm) = let
      fun stmts_PROD_1_SUBRULE_1_NT (strm) = let
            val (statement_RES, statement_SPAN, strm') = statement_NT(strm)
            val FULL_SPAN = (#1(statement_SPAN), #2(statement_SPAN))
            in
              ((statement_RES), FULL_SPAN, strm')
            end
      fun stmts_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.Keyword_fun, _, strm') => true
              | (Tok.Keyword_val, _, strm') => true
              | (Tok.Keyword_datatype, _, strm') => true
              | (Tok.Verbatim(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(stmts_PROD_1_SUBRULE_1_PRED, stmts_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
      in
        ((SR_RES), FULL_SPAN, strm')
      end
in
  (stmts_NT)
end
val stmts_NT =  fn s => unwrap (Err.launch (eh, lexFn, stmts_NT , true) s)

in (stmts_NT) end
  in
fun parse lexFn  s = let val (stmts_NT) = mk lexFn in stmts_NT s end

  end

end
