namespace DslType

/// AST representing the DSL types described in grammar.md
type PrimitiveType =
    | Unit
    | Bool
    | Int
    | Float
    | String

type VariantPayload =
    | NoPayload
    | TuplePayload of TypeExpr list
    | RecordPayload of Field list

and TypeExpr =
    | Primitive of PrimitiveType
    | Identifier of string
    | TypeApp of TypeExpr * TypeExpr list
    | Tuple of TypeExpr list
    | Record of Field list
    | Sum of Variant list
    | Rec of RecBinding

and RecBinding =
    | Single of string * TypeExpr
    | Multi of string list * RecBody list

and Field =
    { Name: string
      Type: TypeExpr }

and Variant =
    { Name: string
      Payload: VariantPayload }

and RecBody =
    { Name: string
      Body: TypeExpr }

and TypeDecl =
    { Name: string
      TypeParams: string list
      Body: TypeExpr }

and Program =
    { Declarations: TypeDecl list }

module Pretty =
    let private primitiveToString = function
        | PrimitiveType.Unit -> "Unit"
        | PrimitiveType.Bool -> "Bool"
        | PrimitiveType.Int -> "Int"
        | PrimitiveType.Float -> "Float"
        | PrimitiveType.String -> "String"

    let rec typeExpr expr =
        match expr with
        | TypeExpr.Primitive prim -> primitiveToString prim
        | TypeExpr.Identifier id -> id
        | TypeExpr.TypeApp (subject, args) ->
            let renderedArgs = args |> List.map typeExpr |> String.concat ", "
            $"{typeExpr subject}<{renderedArgs}>"
        | TypeExpr.Tuple members ->
            members
            |> List.map typeExpr
            |> String.concat ", "
            |> sprintf "(%s)"
        | TypeExpr.Record fields ->
            fields
            |> List.map field
            |> String.concat "; "
            |> sprintf "{ %s }"
        | TypeExpr.Sum variants ->
            variants
            |> List.map variant
            |> String.concat " \n| "
            |> sprintf "| %s"
        | TypeExpr.Rec binding -> recBinding binding

    and field (fieldDecl: Field) =
        $"{fieldDecl.Name}: {typeExpr fieldDecl.Type}"

    and variant (variantDecl: Variant) =
        match variantDecl.Payload with
        | VariantPayload.NoPayload -> variantDecl.Name
        | VariantPayload.TuplePayload payload ->
            payload
            |> List.map typeExpr
            |> String.concat ", "
            |> sprintf "%s(%s)" variantDecl.Name
        | VariantPayload.RecordPayload fields ->
            fields
            |> List.map field
            |> String.concat "; "
            |> sprintf "%s { %s }" variantDecl.Name

    and recBinding binding =
        match binding with
        | RecBinding.Single (name, body) ->
            $"rec {name}. {typeExpr body}"
        | RecBinding.Multi (names, bodies) ->
            let header =
                names
                |> String.concat ", "
                |> sprintf "rec (%s)."

            let renderedBodies =
                bodies
                |> List.map (fun b -> $"{b.Name} = {typeExpr b.Body};")
                |> String.concat " \n"

            $"{header} {renderedBodies}"

    let program (doc: Program) =
        doc.Declarations
        |> List.map (fun decl ->
            let renderedParams =
                match decl.TypeParams with
                | [] -> ""
                | items -> items |> String.concat ", " |> sprintf "<%s>"

            $"type {decl.Name}{renderedParams} = {typeExpr decl.Body};")
        |> String.concat "\n\n"
