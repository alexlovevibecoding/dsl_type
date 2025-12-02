module DslType.Tests.ParserTests

open Xunit
open FSharp.Text.Lexing
open DslType

let private parseProgram source =
    let lexbuf = LexBuffer<char>.FromString source
    TypeParser.main TypeLexer.token lexbuf

let private parseSingleTypeDecl source =
    let doc = parseProgram source
    match doc.Declarations with
    | [Declaration.Type decl] -> decl
    | [other] -> failwithf "Expected type declaration but got %A" other
    | other -> failwithf "Expected exactly one declaration but got %d" (List.length other)

let private parseSingleFnDecl source =
    let doc = parseProgram source
    match doc.Declarations with
    | [Declaration.Function decl] -> decl
    | [other] -> failwithf "Expected function declaration but got %A" other
    | other -> failwithf "Expected exactly one declaration but got %d" (List.length other)

let private expectPrimitive expected expr =
    match expr with
    | TypeExpr.Primitive prim -> Assert.Equal(expected, prim)
    | _ -> failwith "Expected primitive type"

let private expectIdentifier expected expr =
    match expr with
    | TypeExpr.Identifier name -> Assert.Equal(expected, name)
    | _ -> failwith "Expected identifier"

let private expectTypeApp (expectedSubject: string) expectedArgs expr =
    match expr with
    | TypeExpr.TypeApp (subject, args) ->
        expectIdentifier expectedSubject subject
        Assert.Equal<TypeExpr list>(expectedArgs, args)
    | _ -> failwith "Expected type application"

let private expectPipeline expectedSubject expectedSteps expr =
    match expr with
    | TypeExpr.Pipeline (subject, steps) ->
        Assert.Equal<TypeExpr>(expectedSubject, subject)
        Assert.Equal<TypeExpr list>(expectedSteps, steps)
    | _ -> failwith "Expected pipeline"

let private primitiveFromName name =
    match name with
    | "Unit" -> PrimitiveType.Unit
    | "Bool" -> PrimitiveType.Bool
    | "Int" -> PrimitiveType.Int
    | "Float" -> PrimitiveType.Float
    | "String" -> PrimitiveType.String
    | "Date" -> PrimitiveType.Date
    | "DateTime" -> PrimitiveType.DateTime
    | "Guid" -> PrimitiveType.Guid
    | "Byte" -> PrimitiveType.Byte
    | "SByte" -> PrimitiveType.SByte
    | "Int16" -> PrimitiveType.Int16
    | "Int32" -> PrimitiveType.Int32
    | "Int64" -> PrimitiveType.Int64
    | "UInt16" -> PrimitiveType.UInt16
    | "UInt32" -> PrimitiveType.UInt32
    | "UInt64" -> PrimitiveType.UInt64
    | "Decimal" -> PrimitiveType.Decimal
    | "Char" -> PrimitiveType.Char
    | other -> failwith $"Unexpected primitive {other}"

[<Theory>]
[<InlineData("Date", "Date")>]
[<InlineData("DateTime", "DateTime")>]
[<InlineData("Guid", "Guid")>]
[<InlineData("Byte", "Byte")>]
[<InlineData("SByte", "SByte")>]
[<InlineData("Int16", "Int16")>]
[<InlineData("Int32", "Int32")>]
[<InlineData("Int64", "Int64")>]
[<InlineData("UInt16", "UInt16")>]
[<InlineData("UInt32", "UInt32")>]
[<InlineData("UInt64", "UInt64")>]
[<InlineData("Decimal", "Decimal")>]
[<InlineData("Char", "Char")>]
let ``common primitives parse`` (name: string, expectedName: string) =
    let decl = parseSingleTypeDecl $"type Shared = {name};"
    Assert.Equal("Shared", decl.Name)
    Assert.Empty decl.TypeParams

    expectPrimitive (primitiveFromName expectedName) decl.Body

[<Fact>]
let ``all primitives round-trip`` () =
    let primitives =
        [ "Unit"
          "Bool"
          "Int"
          "Float"
          "String"
          "Date"
          "DateTime"
          "Guid"
          "Byte"
          "SByte"
          "Int16"
          "Int32"
          "Int64"
          "UInt16"
          "UInt32"
          "UInt64"
          "Decimal"
          "Char" ]

    for name in primitives do
        let decl = parseSingleTypeDecl $"type T = {name};"
        expectPrimitive (primitiveFromName name) decl.Body

[<Fact>]
let ``pipeline applies type constructor`` () =
    let decl = parseSingleTypeDecl "type Wrapped = Int |> Option;"
    let intPrim = TypeExpr.Primitive PrimitiveType.Int
    expectPipeline intPrim [TypeExpr.Identifier "Option"] decl.Body

[<Fact>]
let ``pipeline supports chaining`` () =
    let decl = parseSingleTypeDecl "type Wrapped = Int |> Option |> List;"
    let intPrim = TypeExpr.Primitive PrimitiveType.Int
    expectPipeline intPrim [TypeExpr.Identifier "Option"; TypeExpr.Identifier "List"] decl.Body

[<Fact>]
let ``pipeline supports identifier subject`` () =
    let decl = parseSingleTypeDecl "type Wrapped = Custom |> Option;"
    let subject = TypeExpr.Identifier "Custom"
    expectPipeline subject [TypeExpr.Identifier "Option"] decl.Body

[<Fact>]
let ``pipeline supports partial application`` () =
    let decl = parseSingleTypeDecl "type Wrapped = Int |> Result<String>;"
    let intPrim = TypeExpr.Primitive PrimitiveType.Int
    let stringPrim = TypeExpr.Primitive PrimitiveType.String
    let resultPartial = TypeExpr.TypeApp(TypeExpr.Identifier "Result", [stringPrim])
    expectPipeline intPrim [resultPartial] decl.Body

[<Fact>]
let ``function declaration parses parameters and return type`` () =
    let fn = parseSingleFnDecl "fn add(x: Int, y: Int): Int;"
    Assert.Equal("add", fn.Name)

    match fn.Parameters with
    | [x; y] ->
        Assert.Equal("x", x.Name)
        expectPrimitive PrimitiveType.Int x.Type
        Assert.Equal("y", y.Name)
        expectPrimitive PrimitiveType.Int y.Type
    | _ -> failwith "Expected two parameters"

    expectPrimitive PrimitiveType.Int fn.ReturnType

[<Fact>]
let ``function declaration supports piped return type`` () =
    let fn = parseSingleFnDecl "fn fetch(id: Guid): User |> Option;"
    Assert.Equal("fetch", fn.Name)

    match fn.Parameters with
    | [id] ->
        Assert.Equal("id", id.Name)
        expectPrimitive PrimitiveType.Guid id.Type
    | _ -> failwith "Expected one parameter"

    let subject = TypeExpr.Identifier "User"
    expectPipeline subject [TypeExpr.Identifier "Option"] fn.ReturnType

[<Fact>]
let ``record declarations parse`` () =
    let decl = parseSingleTypeDecl "type Point = { x: Int; y: Int };"
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
        parseSingleTypeDecl
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
        parseSingleTypeDecl
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
        parseSingleTypeDecl
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
