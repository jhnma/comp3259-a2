{
module Parser (parseExpr) where
import Data.Char (isDigit, isSpace, isAlpha)
import Prelude hiding (LT, GT, EQ)
import Declare
import Tokens
}


%name parser
%tokentype { Token }
%error { parseError }

%token
    var     { TokenVar }
    id      { TokenSym $$ }
    int     { TokenInt $$ }
    Int     { TokenTInt }
    Bool    { TokenTBool }
    '+'     { TokenPlus }
    '-'     { TokenMinus }
    '*'     { TokenTimes }
    '/'     { TokenDiv }
    '('     { TokenLParen }
    ')'     { TokenRParen }
    '}'     { TokenRB }
    '{'     { TokenLB }
    ';'     { TokenSemiColon }
    ':'     { TokenColon }
    ','     { TokenComma }
    '='     { TokenEq }
    if      { TokenIf }
    else    { TokenElse }
    true    { TokenTrue }
    false   { TokenFalse }
    '<'     { TokenLT }
    '<='    { TokenLE }
    '>'     { TokenGT }
    '>='    { TokenGE }
    '=='    { TokenComp }
    '&&'    { TokenAnd }
    '!'     { TokenNot }
    '||'    { TokenOr }
    fun { TokenFunc }

%right ';' else
%left '||'
%left '&&'
%nonassoc '=='
%nonassoc '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'
%left NEG NOT


%%

Program : Functions Exp        { Program $1 $2 }

Functions: Functions Function  { $1 ++ [$2] }
         |                     { [] }

Function : fun id '(' ids ')' '{' Exp '}'   { ($2, Function $4 $7) }

ids : ids ',' id ':' typ    { $1 ++ [($3, $5)] }
    | id ':' typ            { [($1, $3)] }
    |                       { [] }

typ : Int   { TInt }
    | Bool  { TBool }

Exp : var id '=' Exp ';' Exp          { Decl $2 $4 $6 }
    | if '(' Exp ')' Exp ';' else Exp { If $3 $5 $8 }
    | Exp '||' Exp                    { Bin Or $1 $3 }
    | Exp '&&' Exp                    { Bin And $1 $3 }
    | Exp '==' Exp                    { Bin EQ $1 $3 }
    | Exp '<' Exp                     { Bin LT $1 $3 }
    | Exp '>' Exp                     { Bin GT $1 $3 }
    | Exp '<=' Exp                    { Bin LE $1 $3 }
    | Exp '>=' Exp                    { Bin GE $1 $3 }
    | Exp '+' Exp                     { Bin Add $1 $3 }
    | Exp '-' Exp                     { Bin Sub $1 $3 }
    | Exp '*' Exp                     { Bin Mult $1 $3 }
    | Exp '/' Exp                     { Bin Div $1 $3 }
    | '-' Exp %prec NEG               { Unary Neg $2 }
    | '!' Exp %prec NOT               { Unary Not $2 }
    | int                             { Lit (IntV $1) }
    | true                            { Lit (BoolV True) }
    | false                           { Lit (BoolV False) }
    | id '(' Exps ')'                 { Call $1 $3 }
    | id                              { Var $1 }
    | '(' Exp ')'                     { $2 }

Exps : Exps ',' Exp                   { $1 ++ [$3] }
     | Exp                            { [$1] }
     |                                { [] }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr = parser . scanTokens

}
