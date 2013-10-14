%name MetaLangLex;

%let digit = [0-9];
%let num   = {digit}+;
%let alpha = [a-zA-Z];
%let word  = [0-9a-zA-Z_];
%let id    = {alpha}{word}*;

%let whitespace = [ \r\n\t];
%let comments   = #[^\n]*;


%defs (
    structure Token = MetaLangTokens;
    type lex_result = Token.token;
    fun eof() = Token.EOF
);

     (* Keywords *)
     "fun"      => (Token.Keyword_fun);
     "val"      => (Token.Keyword_val);
     "if"       => (Token.Keyword_if);
     "then"     => (Token.Keyword_then);
     "else"     => (Token.Keyword_else);
     "datatype" => (Token.Keyword_datatype);
     "of"       => (Token.Keyword_of);
     
     (* Operators *)
     "\\"  => (Token.Operator_LAMBDA);
     "|"   => (Token.Operator_ALT);
     "="   => (Token.Operator_EQ);
     "and" => (Token.Operator_AND);
     "or"  => (Token.Operator_OR);
     "not" => (Token.Operator_NOT);
     ">"   => (Token.Operator_GT);
     "<"   => (Token.Operator_LT);
     ">="  => (Token.Operator_GE);
     "<="  => (Token.Operator_LE);
     "/="  => (Token.Operator_NEQ);
     "*"   => (Token.Operator_MULT);
     "/"   => (Token.Operator_DIV);
     "+"   => (Token.Operator_ADD);
     "-"   => (Token.Operator_SUB);
     "%"   => (Token.Operator_MOD);
     ","   => (Token.Operator_COMMA);
     "::"  => (Token.Operator_GET);
     "("   => (Token.Operator_LP);
     ")"   => (Token.Operator_RP);
     ";"   => (Token.SEMICOLON);

     (* Identifier, integer literals *)
     {id}  => (Token.VarName (yytext));
     {id}\? => (Token.Operator_QUERY (Substring.string (Substring.trimr 1 yysubstr))); (* "type?" -> "type" *)
     {num} => (Token.Number (valOf (Int.fromString yytext)));

     (* Whitespace, comments and unrecognized tokens *)
     {whitespace} => (skip());
     {comments} => (continue());
     . => ( print("error (pos:" ^ (AntlrStreamPos.toString yysm yypos) ^
                  "): Ignore unrecognized character '" ^ yytext ^ "'\n");
            continue() );
     
