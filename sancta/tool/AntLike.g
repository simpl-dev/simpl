grammar AntLike;

options {
    output=AST;
    ASTLabelType=CommonTree;
//    backtrack=true;
//    memoize=true;
}

tokens {
    NODE;
    ARG;
    BODY;
    MATCH;
}

@header {
    package ee.cyber.simplicitas.parse;
}

@lexer::header {
    package ee.cyber.simplicitas.parse;
}

grammarDef  : header ruleDef+ EOF!;
header      : packageDef (optionDef | scalaHeader | lexerStates)*;
packageDef  : 'grammar'^ dottedId ';'!;
optionDef   : 'options'^ '('! optionList ')'!;
optionList  : option*;
option      : ID '=' ID ';' -> ^(ID ID);
dottedId    : ID ('.' ID)*;
scalaHeader : 'scalaheader'^ CODE;
lexerStates : 'lexer-states'^ '('! ID (','! ID)* ')'!;

ruleDef:
      ('hidden'?) 'terminal'^ ID body? stateOp* ':'! termAltList ';'!
    | 'fragment'^ ID stateOp* ':'! termAltList ';'!
    | 'option'^ ID body? returnType? ':'! ID ('|'! ID)* ';'!
    | ID body? returnType? ':'^ altList ';'!;

stateOp     : enterState | exitState | checkLast | checkAny;
enterState  : 'enter-state'^ '('! ID (','! ID)* ')'!;
exitState   : 'exit-state'^ '('! ID (','! ID)* ')'!;
checkLast    : 'check-Last'^ '('! ID (','! ID)* ')'!;
checkAny    : 'check-any'^ '('! ID (','! ID)* ')'!;
body        : CODE -> ^(BODY CODE);
returnType  : 'returns'^ dottedId? body?;
altList     : matchList ('|'! matchList)*;
matchList   : match+ -> ^(NODE match+);
match       : token modifier? -> ^(MATCH modifier? token);
name        : ID '='^;
modifier    : '?' | '*' | '+';
token       : '('^ altList ')'! | (ID '='^)? ID | (ID '='^)? STR;

// terminals
//termParams  : '('! termParam (','! termParam)* ')'!;
//termParam   : ID ':' dottedId '=' CODE -> ^(ARG ID CODE dottedId);
termAltList : termList ('|'! termList)*;
termList    : termMatch+ -> ^(NODE termMatch+);
termMatch   : (inv='~')? terminal modifier?
                -> ^(MATCH modifier? $inv? terminal);
terminal    : '('^ termAltList ')'! | ID | STR '..'^ STR | STR | '.';

fragment LETTER: 'a'..'z' | 'A'..'Z' | '_';
ID: LETTER (LETTER | '0'..'9')*;
fragment SINGLE_QUOTED_STR : '\'' (~('\''|'\\') | '\\' .)* '\'';
fragment DOUBLE_QUOTED_STR : '"' (~('"'|'\\') | '\\' .)* '"'
	{setText("'" + getText().substring(1, getText().length() - 1)
		.replaceAll("(?<!\\\\)(\\\\{2})*'", "\\\\$0") + "'");};
STR: SINGLE_QUOTED_STR | DOUBLE_QUOTED_STR;
CODE: '{' (~('{'|'}') | CODE)* '}';
fragment SL_COMMENT: '//' ~('\r'|'\n')*;
fragment ML_COMMENT: '/*' (ML_COMMENT | ~('*'|'/') | '/' ~'*' | '*' ~'/')* '*/';
WS: (' '|'\t'|'\n'|'\r'|ML_COMMENT|SL_COMMENT)+ {skip();};
