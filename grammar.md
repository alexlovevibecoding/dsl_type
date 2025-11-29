Program
    ::= { TypeDecl } ;

TypeDecl
    ::= "type" TypeName [ TypeParams ] "=" TypeExpr ";" ;

TypeName
    ::= Ident ;

TypeParams
    ::= "<" Ident { "," Ident } ">" ;

TypeExpr
    ::= RecType
     | SumType ;

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