
module DslType.TypeLexer
open FSharp.Text.Lexing
open DslType.TypeParser/// Rule token
val token: lexbuf: LexBuffer<char> -> token
/// Rule blockComment
val blockComment: depth: obj -> lexbuf: LexBuffer<char> -> token
