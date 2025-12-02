Program
    ::= { Declaration } ;

Declaration
    ::= TypeDecl
     | FnDecl ;

TypeDecl
    ::= "type" TypeName [ TypeParams ] "=" TypeExpr ";" ;

FnDecl
    ::= "fn" Ident "(" [ ParamList ] ")" ":" TypeExpr ";" ;

ParamList
    ::= ParamDecl { "," ParamDecl } ;

ParamDecl
    ::= Ident ":" TypeExpr ;

TypeName
    ::= Ident ;

TypeParams
    ::= "<" Ident { "," Ident } ">" ;

TypeExpr
    ::= RecType
     | SumType
     | PipeType ;

RecType
    ::= "rec" Ident "." TypeExpr
     | "rec" "(" Ident { "," Ident } ")" "." "{" RecBodyList "}"
     | SumType ;

RecBodyList
    ::= RecBody { RecBody } ;

RecBody
    ::= Ident "=" TypeExpr ";" ;

SumType
    ::= Variant { "|" Variant } ;

Variant
    ::= ConstrName VariantPayloadOpt ;

ConstrName
    ::= Ident ;

VariantPayloadOpt
    ::=                            (* no payload *)
     | "(" TypeList ")"            (* tuple payload *)
     | RecordType ;                (* record payload *)

RecordType
    ::= "{" FieldList "}" ;

FieldList
    ::= FieldDecl { ";" FieldDecl } [ ";" ] ;

FieldDecl
    ::= Ident ":" TypeExpr ;

TupleType
    ::= "(" TypeList ")" ;

TypeList
    ::= TypeExprAtom { "," TypeExprAtom } ;

TypeExprApp
    ::= TypeExprAtom [ TypeArgs ] ;

PipeType
    ::= PipeChain ;

PipeChain
    ::= TypeExprApp
     | PipeChain "|>" TypeExprApp ;

TypeArgs
    ::= "<" TypeExpr { "," TypeExpr } ">" ;

TypeExprAtom
    ::= PrimitiveType
     | TypeVar
     | NamedType
     | TupleType
     | RecordType
     | "(" TypeExpr ")" ;

NamedType
    ::= TypeName ;

TypeVar
    ::= Ident ;

PrimitiveType
    ::= "Unit"
     | "Bool"
     | "Int"
     | "Float"
     | "Date"
     | "DateTime"
     | "Guid"
     | "Byte"
     | "SByte"
     | "Int16"
     | "Int32"
     | "Int64"
     | "UInt16"
     | "UInt32"
     | "UInt64"
     | "Decimal"
     | "Char"
     | "String" ;

Ident
    ::= Letter { LetterOrDigitOrUnderscore } ;

Letter
    ::= "A" | … | "Z"
     | "a" | … | "z"
     | "_" ;

LetterOrDigitOrUnderscore
    ::= Letter | Digit | "_" ;

Digit
    ::= "0" | … | "9" ;
