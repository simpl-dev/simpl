grammar AntLike;

options {
    output=AST;
    ASTLabelType=CommonTree;
//    backtrack=true;
//    memoize=true;
}

tokens {
    NODE;
    NAME;
    ARG;
    BODY;
}

@header {
    package ee.cyber.simplicitas.parse;
}

@lexer::header {
    package ee.cyber.simplicitas.parse;
}

grammarDef  : headerDef ';'! ruleDef+ EOF!;
headerDef   : 'grammar'^ dottedId;
dottedId    : ID ('.' ID)*;

ruleDef:
      ('hidden'?) 'terminal'^ ID termParams? body? ':'! termAltList ';'!
    | 'fragment'^ ID ':'! termAltList ';'!
    | 'option'^ ID body? ':'! ID ('|'! ID)* ';'!
    | ID body? ':'^ altList ';'!;

body        : CODE -> ^(BODY CODE);
altList     : matchList ('|'! matchList)*;
matchList   : match+ -> ^(NODE match+);
match       : name? token modifier? -> name? modifier? token;
name        : ID '='^;
modifier    : '?' | '*' | '+';
token       : '('^ altList ')'! | ID | STR;

// terminals
termParams  : '('! termParam (','! termParam)* ')'!;
termParam   : ID ':' dottedId '=' CODE -> ^(ARG ID CODE dottedId);
termAltList : termList ('|'! termList)*;
termList    : termMatch+ -> ^(NODE termMatch+);
termMatch   : (inv='~')? terminal modifier? CODE*
                -> CODE* modifier? $inv? terminal;
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
