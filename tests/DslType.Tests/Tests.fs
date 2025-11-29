module DslType.Tests.ParserTests

open Xunit
open FSharp.Text.Lexing
open DslType

let private parseProgram source =
    let lexbuf = LexBuffer<char>.FromString source
    TypeParser.main TypeLexer.token lexbuf

let private parseSingleDecl source =
    let doc = parseProgram source
    match doc.Declarations with
    | [decl] -> decl
    | other -> failwithf "Expected exactly one declaration but got %d" (List.length other)

let private expectPrimitive expected expr =
    match expr with
    | TypeExpr.Primitive prim -> Assert.Equal(expected, prim)
    | _ -> failwith "Expected primitive type"

[<Fact>]
let ``record declarations parse`` () =
    let decl = parseSingleDecl "type Point = { x: Int; y: Int };"
    Assert.Equal("Point", decl.Name)
    Assert.Empty decl.TypeParams

    match decl.Body with
    | TypeExpr.Record fields ->
        match fields with
        | [xField; yField] ->
            Assert.Equal("x", xField.Name)
            expectPrimitive PrimitiveType.Int xField.Type
            Assert.Equal("y", yField.Name)
            expectPrimitive PrimitiveType.Int yField.Type
        | _ -> failwith "Expected two fields"
    | _ -> failwith "Expected record type"

[<Fact>]
let ``sum type with tuple and record payload`` () =
    let decl =
        parseSingleDecl
            """
            type Shape =
                | Circle(Float)
                | Rect { width: Float; height: Float }
                | Unknown;
            """

    match decl.Body with
    | TypeExpr.Sum variants ->
        match variants with
        | [circle; rect; unknown] ->
            Assert.Equal("Circle", circle.Name)
            match circle.Payload with
            | VariantPayload.TuplePayload [payload] ->
                expectPrimitive PrimitiveType.Float payload
            | _ -> failwith "Expected tuple payload"

            Assert.Equal("Rect", rect.Name)
            match rect.Payload with
            | VariantPayload.RecordPayload [width; height] ->
                Assert.Equal("width", width.Name)
                expectPrimitive PrimitiveType.Float width.Type
                Assert.Equal("height", height.Name)
                expectPrimitive PrimitiveType.Float height.Type
            | _ -> failwith "Expected record payload"

            Assert.Equal("Unknown", unknown.Name)
            Assert.Equal(VariantPayload.NoPayload, unknown.Payload)
        | _ -> failwith "Expected three variants"
    | _ -> failwith "Expected sum type"

[<Fact>]
let ``recursive list with type application`` () =
    let decl =
        parseSingleDecl
            """
            type List<T> =
                rec Self.
                    | Nil
                    | Cons { head: T; tail: Self };
            """

    Assert.Equal<string list>([ "T" ], decl.TypeParams)

    match decl.Body with
    | TypeExpr.Rec (RecBinding.Single (name, TypeExpr.Sum variants)) ->
        Assert.Equal("Self", name)
        Assert.Equal(2, variants.Length)
        let cons = variants[1]
        match cons.Payload with
        | VariantPayload.RecordPayload fields ->
            let tailField = fields |> List.find (fun f -> f.Name = "tail")
            match tailField.Type with
            | TypeExpr.Identifier "Self" -> ()
            | _ -> failwith "Expected recursive reference"
        | _ -> failwith "Expected record payload"
    | _ -> failwith "Expected recursive sum"

[<Fact>]
let ``mutually recursive block tracks names and bodies`` () =
    let decl =
        parseSingleDecl
            """
            type Graph =
                rec (Node, Edge).
                {
                    Node = | Node { id: Int; edges: List<Edge> };
                    Edge = | Edge { target: Node };
                };
            """

    match decl.Body with
    | TypeExpr.Rec (RecBinding.Multi (names, bodies)) ->
        Assert.Equal<string list>([ "Node"; "Edge" ], names)
        Assert.Equal(2, bodies.Length)
        let nodeBody = bodies |> List.find (fun b -> b.Name = "Node")
        match nodeBody.Body with
        | TypeExpr.Sum nodeVariants ->
            let variant = nodeVariants |> List.exactlyOne
            match variant.Payload with
            | VariantPayload.RecordPayload fields ->
                let edgesField = fields |> List.find (fun f -> f.Name = "edges")
                match edgesField.Type with
                | TypeExpr.TypeApp (TypeExpr.Identifier "List", [TypeExpr.Identifier "Edge"]) -> ()
                | _ -> failwith "Expected type application"
            | _ -> failwith "Expected record payload"
        | _ -> failwith "Expected sum for Node"
    | _ -> failwith "Expected multi-recursive binding"
