structure MetaLangAST = struct

	datatype MetaLangProgram = ASTList of Statement list

		 and Statement = 
			 ValBinding of string * Expression |
			 DatatypeBinding of string * string list |
			 FunBinding of string * FunctionDefinition list |
             Verbatim of string
														
		 and FunctionDefinition = 
			 FunDef of Expression list * CompoundExpression

		 and Expression = 
			 Identifier of string |
			 Integer of int |
			 Lambda of string list * Expression |
			 IfThenElse of Expression * CompoundExpression * CompoundExpression |
			 Funcall of Expression * Expression list |
			 opISA of string * Expression |
			 opMEM of Expression * string |
			 opADD of Expression * Expression |
			 opSUB of Expression * Expression |
			 opMUL of Expression * Expression |
			 opDIV of Expression * Expression |
			 opMOD of Expression * Expression |
			 opLT  of Expression * Expression |
			 opGT  of Expression * Expression |
			 opLE  of Expression * Expression |
			 opGE  of Expression * Expression |
			 opEQ  of Expression * Expression |
			 opNE  of Expression * Expression |
			 opAND of Expression * Expression |
			 opOR  of Expression * Expression |
			 opNOT of Expression |
			 opNEG of Expression 

		 and CompoundExpression =
			 CompoundExpr of Statement list * Expression ;

end
