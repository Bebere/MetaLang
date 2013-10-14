structure MetaLangCompiler = 
struct

structure AST = MetaLangAST
exception UndefinedSymbol of string
exception UndefinedDatatype of string
exception UndefinedField of string
exception InvalidArgument


(* The environment *)

structure SymbolSet = RedBlackSetFn (struct type ord_key = string
                                            val  compare = String.compare
                                     end)

structure DataTypeSet = RedBlackSetFn (struct type ord_key = string * int
                                              val  compare = (fn (x: ord_key ,y: ord_key) => 
                                                                 String.compare(#1 x, #1 y))
                                       end)

datatype ContextType = TemplateCode | NonTemplateCode

type environment = { context           : ContextType, 
                     defined_datatype  : DataTypeSet.set,
                     defined_field     : SymbolSet.set,
                     defined_symbol    : SymbolSet.set,
                     distinguisher     : int }

fun add_symbol_to_env (sym, env : environment) = 
    let
        val symset = (#defined_symbol env)
        val newsymset = SymbolSet.add (symset, sym)
    in
        { context          = (#context env),
          defined_datatype = (#defined_datatype env),
          defined_field    = (#defined_field env),
          defined_symbol   = newsymset,
          distinguisher    = (#distinguisher env) }
    end

fun add_field_to_env (field, env : environment) = 
    let
        val fieldset = (#defined_field env)
        val newfieldset = SymbolSet.add (fieldset, field)
    in
        { context          = (#context env),
          defined_datatype = (#defined_datatype env),
          defined_field    = newfieldset,
          defined_symbol   = (#defined_symbol env),
          distinguisher    = (#distinguisher env) }
    end

fun add_datatype_to_env (dty, env : environment) = 
    let
        val dtyset = (#defined_datatype env)
        val newdtyset = DataTypeSet.add (dtyset, dty)
    in
        { context          = (#context env),
          defined_datatype = newdtyset,
          defined_field    = (#defined_field env),
          defined_symbol   = (#defined_symbol env),
          distinguisher    = (#distinguisher env) }
    end

fun is_defined_datatype (dtyname, env : environment) = 
    DataTypeSet.exists 
        (fn s : string * int => (#1 s) = dtyname) (#defined_datatype env)

fun get_datatype_id (dtyname, env : environment) = 
    let
        val result = DataTypeSet.find 
                         (fn s : string * int => (#1 s) = dtyname) 
                         (#defined_datatype env)
    in
        case result of
            NONE => (print("referencing undefined datatype '" ^ dtyname ^ "'\n");
                     raise UndefinedDatatype dtyname)
          | SOME (name, id) => id
    end


fun template_env (env : environment) = 
    { context          = TemplateCode,
      defined_datatype = (#defined_datatype env),
      defined_field    = (#defined_field env),
      defined_symbol   = (#defined_symbol env),
      distinguisher    = (#distinguisher env) }


fun add_qualifier (code, expr, env : environment) = 
    case ((#context env), expr) of
        (TemplateCode, AST.Integer _) => code
      | (TemplateCode, AST.Identifier _) => code
      | (TemplateCode, AST.opISA _) => code
      | (TemplateCode, AST.Lambda _) => code
      | (TemplateCode, AST.Funcall (e,_)) => 
        (case e of
             AST.Identifier name => if is_defined_datatype (name, env) then 
                                        code
                                    else
                                        "typename " ^ code
           | _ => "typename " ^ code)
      | (TemplateCode, _) => "typename " ^ code
      | _ => code
        

fun distinguish_env (env : environment) = 
    { context          = (#context env),
      defined_datatype = (#defined_datatype env),
      defined_field    = (#defined_field env),
      defined_symbol   = (#defined_symbol env),
      distinguisher    = ((#distinguisher env) + 1) }
          

(* The compiler *)

fun compile_statement (ast, env) = 
    case ast of
        AST.ValBinding _ => compile_val_binding (ast, env)
      | AST.DatatypeBinding _ => compile_datatype_binding (ast, env)
      | AST.FunBinding _ => compile_fun_binding (ast, env)


and compile_val_binding (ast, env) = 
    let
        val AST.ValBinding (name, expr) = ast
        val ((exprcode, predef), newenv) = compile_expression (expr, env)
                                           handle e => (print("  in val-binding for '" ^ name ^ "'\n");
                                                        raise e)
        val retenv = add_symbol_to_env (name, newenv)
        val retcode = "typedef " ^ (add_qualifier (exprcode, expr, env)) ^ " " ^ name ^ ";\n"
    in
        ((predef ^ retcode, ""), retenv)
    end

and compile_datatype_binding (ast, env) = 
    let
        val AST.DatatypeBinding (name, field_lst) = ast
        val dty_id = (#distinguisher env)
        val tmpenv = add_datatype_to_env ((name, dty_id), env)
        val retenv = List.foldr add_field_to_env tmpenv field_lst
                                
        val template_arg = case field_lst of 
                               [] => ""
                             | _ => "template <" ^ 
                                    String.concatWith "," (List.map (fn s => "typename _" ^ s ^ "_ty") field_lst)
                                    ^ ">\n"
                                             
        val template_def = "struct " ^ name ^ "{\n" ^ 
                           (String.concatWith "\n" (List.map (fn s => "typedef _" ^ s ^ "_ty " ^ s ^ ";") field_lst))
                           ^ "\nstatic const unsigned int __type_id = " ^ (Int.toString dty_id) ^ ";\n"
                           ^ "};\n"
    in
        ((template_arg ^ template_def, ""), (distinguish_env retenv))
    end


and compile_fun_definition (ast, env) = 
    let
        val AST.FunDef (arglist, AST.CompoundExpr (stmts, expr)) = ast

        (* we need to analyze the arglist in order to separate "real" arguments from
           partial specialized args *)
        fun gen_arglists (arglst, param, spec, full) = 
            case arglst of 
                [] => (List.rev param, List.rev spec, List.rev full)
              | arg::rest => (case arg of 
                                  AST.Identifier name => if is_defined_datatype (name, env) 
                                                         then gen_arglists(rest, param, name::spec, name::full)
                                                         else gen_arglists(rest, name::param, spec, name::full)
                                | AST.Integer i => gen_arglists(rest, param,
                                                                ("meta_integer<" ^ (Int.toString i) ^ ">")::spec, 
                                                                ("meta_integer<" ^ (Int.toString i) ^ ">")::full)
                                | _ => raise (print("invalid parameter in function argument list\n");
                                              raise InvalidArgument))

        val (param_lst, spec_lst, full_lst) = gen_arglists (arglist, [], [], [])
        val inner_env = template_env (List.foldr add_symbol_to_env env param_lst)
        val ((code_expr, predef), env_expr) = compile_compound_expression(stmts, expr, inner_env)
        val dummy_id = (Int.toString (#distinguisher env))
    in
        "template <" ^ (String.concatWith 
                            "," (List.map (fn s => "typename " ^ s)
                                          param_lst)) ^ (if null param_lst then "typename _"
                                                         else ", typename _") ^ dummy_id ^ ">\n" ^ 
        "struct apply " ^ (case spec_lst of 
                               [] => ""
                             | _ => "<" ^ (String.concatWith ", " full_lst) ^ ", _" ^ dummy_id ^ ">")
        ^ "{\n" ^
        predef ^
        "typedef " ^ (add_qualifier (code_expr, expr, inner_env)) ^ " value;\n" ^
        "};\n"
    end     

and compile_fun_binding (ast, env) = 
    let
        val AST.FunBinding (name, fundef_lst) = ast
        val inner_env = add_symbol_to_env (name, env)
        val funcode = String.concatWith 
                          "\n" (List.map (fn s => compile_fun_definition (s, inner_env)) fundef_lst)
                      handle e => (print("  in fun-binding for '" ^ name ^ "'\n");
                                   raise e)
    in
        (("struct " ^ name ^ " {\n" ^ funcode ^ "};\n",
          ""),
         (distinguish_env (add_symbol_to_env (name, env))))
    end


and compile_expression (ast, env) = 
    case ast of
        AST.Identifier id => if SymbolSet.member (#defined_symbol env, id) orelse 
                                is_defined_datatype (id, env)
                             then 
                                 ((id, ""), env)
                             else 
                                 (print("referencing undefined identifier '" ^ id ^ "'\n");
                                  raise UndefinedSymbol id)

      | AST.Integer i => (("meta_integer<" ^ (Int.toString i) ^ ">", ""), env)
      | AST.IfThenElse _ => compile_ifthenelse (ast, env)
      | AST.Lambda _ => compile_lambda (ast, env)
      | AST.Funcall _ => compile_funcall (ast, env)
      | AST.opADD (e1, e2) => compile_binary_op ("__meta_add", e1, e2, env)
      | AST.opSUB (e1, e2) => compile_binary_op ("__meta_sub", e1, e2, env)
      | AST.opMUL (e1, e2) => compile_binary_op ("__meta_mul", e1, e2, env)
      | AST.opDIV (e1, e2) => compile_binary_op ("__meta_div", e1, e2, env)
      | AST.opMOD (e1, e2) => compile_binary_op ("__meta_mod", e1, e2, env)
      | AST.opLT  (e1, e2) => compile_binary_op ("__meta_lt",  e1, e2, env)
      | AST.opGT  (e1, e2) => compile_binary_op ("__meta_gt",  e1, e2, env)
      | AST.opLE  (e1, e2) => compile_binary_op ("__meta_le",  e1, e2, env)
      | AST.opGE  (e1, e2) => compile_binary_op ("__meta_ge",  e1, e2, env)
      | AST.opEQ  (e1, e2) => compile_binary_op ("__meta_eq",  e1, e2, env)
      | AST.opNE  (e1, e2) => compile_binary_op ("__meta_ne",  e1, e2, env)
      | AST.opAND (e1, e2) => compile_binary_op ("__meta_and", e1, e2, env)
      | AST.opOR  (e1, e2) => compile_binary_op ("__meta_or",  e1, e2, env)
      | AST.opNOT (e) => compile_unary_op ("__meta_not", e, env)
      | AST.opNEG (e) => compile_unary_op ("__meta_neg", e, env)
      | AST.opMEM _ => compile_get_member (ast, env)
      | AST.opISA _ => compile_is_same_type (ast, env)

and compile_compound_expression (stmts, expr, env) = 
    let
        val (codelst_stmts, env_stmts) = compile_stmts (stmts, distinguish_env env)
        val ((code_expr, predef_expr), env_expr) = compile_expression (expr, env_stmts)
        val code_stmts = List.foldl (fn (s,t) => t ^ (#1 s) ^ (#2 s)) "" codelst_stmts
    in
        ((code_expr,
          code_stmts ^ predef_expr),
         env_expr)
    end


and compile_binary_op (fname, e1, e2, env) =
    let 
        val ((c1, p1), env1) = compile_expression (e1, env)
        val ((c2, p2), env2) = compile_expression (e2, env1)
    in
        ((fname ^ "<" ^
          (add_qualifier (c1,e1,env1)) ^ ", " ^
          (add_qualifier (c2,e2,env2)) ^ " >::value",
          p1 ^ p2),
         env2)
    end

and compile_unary_op (fname, e, env) = 
    let
        val ((c, p), retenv) = compile_expression (e, env)
    in
        ((fname ^ "<" ^
          (add_qualifier (c, e, env)) ^ " >::value",
          p),
         retenv)
    end

and compile_get_member (ast, env) = 
    let
        val AST.opMEM (expr, mem) = ast
        val ((code, pred), retenv) = compile_expression (expr, env)
    in
        if SymbolSet.member (#defined_field env, mem) then 
            ((code ^ "::" ^ mem, pred), retenv)
        else
            (print("referencing undefined datafield '" ^ mem ^ "'\n");
             raise UndefinedField mem)
    end

and compile_is_same_type (ast, env) = 
    let
        val AST.opISA (tyname, expr) = ast
        val ((code, pred), retenv) = compile_expression (expr, env)
    in
        if is_defined_datatype (tyname, env) then
            (("meta_integer<" ^ (Int.toString (get_datatype_id (tyname, env))) ^ " == " ^ 
              code ^ "::__type_id>",
              pred),
             retenv)
        else if tyname = "int" then
            (("meta_integer< meta_integer<0>::__type_id == " ^ code ^ "::__type_id>",
              pred),
             retenv)
        else
            (print("referencing undefined datatype '" ^ tyname ^ "'\n");
             raise UndefinedDatatype tyname)
    end


and compile_ifthenelse (ast, env) =
    let
        val AST.IfThenElse (cond, 
                            AST.CompoundExpr(stmts_t, expr_t), 
                            AST.CompoundExpr(stmts_f, expr_f)) = ast

        val ((code_cond, predef_cond), env_cond) = compile_expression (cond, (distinguish_env env))
                                                                      
        val (((code_t, predef_t), env_t),
             ((code_f, predef_f), env_f)) = (compile_compound_expression (
                                                  stmts_t, expr_t, template_env (distinguish_env env_cond)),
                                             compile_compound_expression (
                                                 stmts_f, expr_f, template_env (distinguish_env env_cond)))                   

        val defid = (Int.toString (#distinguisher env_cond))
    in
        ((
            (* the value of if-then-else expression *)
            "__branch_" ^ defid ^ "< " ^ (add_qualifier (code_cond, cond, env)) ^ " , __dummy_arg >::value",
            predef_cond ^ 

            (* true branch *)
            "template <typename _t" ^ defid ^ ", typename _" ^ defid ^ ">\n" ^
            "struct __branch_" ^ defid ^ "{\n" ^
            predef_t ^ "typedef " ^ (add_qualifier (code_t, expr_t, env_t)) ^ " value;\n" ^
            "};\n" ^ 

            (* false branch *)
            "template <typename _" ^ defid ^ ">\n" ^
            "struct __branch_" ^ defid ^ "<meta_integer<0>, _" ^ defid ^ "> {\n" ^
            predef_f ^ "typedef " ^ (add_qualifier (code_f, expr_f, env_f)) ^ " value;\n" ^
            "};\n"),
         
         (distinguish_env env_cond))
    end

and compile_lambda (ast, env) = 
    let
        val AST.Lambda (arglist, expr) = ast
        val inner_env = (template_env (List.foldr add_symbol_to_env env arglist))
        val ((code_expr, predef_expr), env_expr) = compile_expression (expr, distinguish_env inner_env)
        val defid = (Int.toString (#distinguisher env))
    in
        ((
            (* value of this lambda *)
            "__lambda_" ^ defid,

            (* definition of this lambda *)
            "struct __lambda_" ^ defid ^ "{\n" ^
            "template <" ^ (String.concatWith "," (List.map (fn s => "typename " ^ s) 
                                                            arglist)) ^ ", typename _" ^ defid ^ ">\n" ^
            "struct apply {\n" ^
            predef_expr ^ "typedef " ^ (add_qualifier (code_expr, expr, inner_env)) ^ " value;\n" ^
            "};\n" ^
            "};\n"),

         (distinguish_env env))
    end

and compile_funcall (ast, env) = 
    let
        val AST.Funcall (efunc, _) = ast
    in
        case efunc of
            AST.Identifier name => if is_defined_datatype (name, env) then 
                                       compile_constructor_call (ast, env)
                                   else
                                       compile_function_call (ast, env)
                                                             
          | _ => compile_function_call (ast, env)
    end

and flatten_arglist (arglist, env) = 
    case arglist of 
        [] => ([], env)
      | arg::rest => let val ((c,p), newenv) = compile_expression (arg, env)
                         val (codelst, finalenv) = flatten_arglist (rest, newenv)
                     in
                         ((add_qualifier (c, arg, env), p) :: codelst, finalenv)
                     end

and compile_constructor_call (ast, env) =
    let
        val AST.Funcall (AST.Identifier dtyname, earglist) = ast
        val (code_arglist, retenv) = flatten_arglist (earglist, env)
    in
        ((dtyname ^ "<" ^
          (String.concatWith ", " (List.map (fn s => (#1 s)) code_arglist)) ^ " >",
          String.concat (List.map (fn s => (#2 s)) code_arglist)),
         retenv)
    end

and compile_function_call (ast, env) = 
    let
        val AST.Funcall (efunc, earglist) = ast
        val ((code_f, pred_f), env_f) = compile_expression (efunc, env)
        val (code_arglist, retenv) = flatten_arglist (earglist, env_f)
    in
        ((code_f ^ (case (#context env) of 
                        TemplateCode    => "::template apply<"
                      | NonTemplateCode => "::apply<") ^
          (String.concatWith ", " (List.map (fn s => (#1 s)) code_arglist)) ^ ", __dummy_arg>::value",
          pred_f ^ (String.concat (List.map (fn s => (#2 s)) code_arglist))),
         retenv)
    end


and compile_stmts (ast_list, env) = 
    let
        fun compile_all (astlst, env) = 
            case astlst of 
                [] => ([], env)
              | ast::rest => let val (cod, newenv) = compile_statement (ast, env)
                                 val (lst, finalenv) = compile_all (rest, newenv)
                             in
                                 (cod :: lst, finalenv)
                             end
    in
        compile_all (ast_list, env)
    end


fun Compile prog_ast = 
    compile_stmts (prog_ast, {
                      context          = NonTemplateCode,
                      defined_datatype = DataTypeSet.empty,
                      defined_field    = SymbolSet.empty,
                      defined_symbol   = SymbolSet.empty,
                      distinguisher    = 0
                  })

end


structure MetaLangMain = struct 

structure MetaLangParser = MetaLangParseFn(MetaLangLex)

fun parsefile filename = 
    let         
        val instream = TextIO.openIn filename
        fun input _ = case (TextIO.inputLine instream) of 
                          NONE => ""
                        | SOME s => s

        val sm = AntlrStreamPos.mkSourcemap()
        val lex = MetaLangLex.lex sm
        val strm = MetaLangLex.streamify input
        val (r, strm', errs) = MetaLangParser.parse lex strm
    in
        map (fn s => print(s ^ "\n")) 
            (map (AntlrRepair.repairToString 
                      MetaLangTokens.toString
                      sm) errs);
        r
    end

fun compile (filename) =
    case parsefile filename of
        NONE          => NONE
      | SOME prog_ast => let val (clst, e) = MetaLangCompiler.Compile prog_ast
                         in
                             SOME clst
                         end

fun main (cmd, args) : OS.Process.status = 
    case args of 
        [] => (print("usage: " ^ cmd ^ " sourcefile\n");
               OS.Process.success)
      | fname::_ => case compile fname of
                        NONE => OS.Process.failure
                      | SOME clst => (map (fn (p) => print((#1 p) ^ "\n")) clst;
                                      OS.Process.success)

end
