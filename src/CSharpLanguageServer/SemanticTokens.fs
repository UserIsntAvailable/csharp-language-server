module CSharpLanguageServer.SemanticTokens

open System
open System.Text.Json
open Ionide.LanguageServerProtocol.Types
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Text

type private TokenType =
    | Namespace = 0u
    | Type = 1u
    | Class = 2u
    | Enum = 3u
    | Interface = 4u
    | Struct = 5u
    | TypeParameter = 6u
    | Parameter = 7u
    | Variable = 8u
    | Property = 9u
    | EnumMember = 10u
    | Event = 11u
    | Function = 12u
    | Method = 13u
    | Macro = 14u
    | Keyword = 15u
    | Modifier = 16u
    | Comment = 17u
    | String = 18u
    | Number = 19u
    | Regexp = 20u
    | Operator = 21u
    | Decorator = 22u

[<Flags>]
type private TokenModifiers =
    | None = 0b0000000u
    | Declaration = 0b0000001u
    | Definition = 0b0000010u
    | Readonly = 0b0000100u
    | Static = 0b0001000u
    | Deprecated = 0b0010000u
    | Abstract = 0b0100000u
    | Async = 0b1000000u
    | Modification = 0b10000000u
    | Documentation = 0b100000000u
    | DefaultLibrary = 0b1000000000u

type private TreeItem =
    | Node of SyntaxNode
    | Token of SyntaxToken
    | Trivia of SyntaxTrivia

type private SemanticTokenData =
    { LineSpan: FileLinePositionSpan
      Type: uint32
      Modifiers: uint32 }

type private SimpleNameSyntaxData =
    { IdentifierLineSpan: FileLinePositionSpan
      ReferenceNode: SyntaxNode
      ReferenceSymbol: ISymbol }

let semanticTokensLegend =
    { TokenTypes =
        Enum.GetNames typeof<TokenType>
        |> Array.map JsonNamingPolicy.CamelCase.ConvertName
      TokenModifiers =
        Enum.GetNames typeof<TokenModifiers>
        |> Array.filter (fun name -> name <> "None") // None should not be included on the legend
        |> Array.map JsonNamingPolicy.CamelCase.ConvertName }

// TODO(Unavailable): What is structured trivia?
type SemanticTokenFullCollector(semanticModel: SemanticModel) =
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Trivia)

    let mutable collectedTokens = []

    let collect (tokenType: TokenType) (tokenModifiers: TokenModifiers) (lineSpan: FileLinePositionSpan) =
        let tokenData =
            { LineSpan = lineSpan
              Type = uint tokenType
              Modifiers = uint tokenModifiers }

        collectedTokens <- tokenData :: collectedTokens

    let syntaxTokensToSemanticTokenModifiers (modifiers: SyntaxTokenList) : TokenModifiers =
        modifiers
        |> Seq.fold
            (fun state modifier ->
                state
                + match modifier.Kind() with
                  | SyntaxKind.ReadOnlyKeyword -> TokenModifiers.Readonly
                  | SyntaxKind.ConstKeyword -> TokenModifiers.Readonly
                  | SyntaxKind.StaticKeyword -> TokenModifiers.Static
                  | SyntaxKind.AbstractKeyword -> TokenModifiers.Abstract
                  | SyntaxKind.AsyncKeyword -> TokenModifiers.Async
                  // FIX(Unavailable): SemanticTokenModifiers.Modification is pretty vague. Im not really sure if I should remove them
                  // FIX(Unavailable): If I remove them Enums and Interfaces should not call collectTypeDeclaration, since they will not need to aggregate their modifiers
                  | SyntaxKind.NewKeyword -> TokenModifiers.Modification
                  | SyntaxKind.OverrideKeyword -> TokenModifiers.Modification
                  | _ -> TokenModifiers.None)
            TokenModifiers.None

    let getLineSpan (treeItem: TreeItem) =
        match treeItem with
        | Node node -> node.GetLocation().GetLineSpan()
        | Token token -> token.GetLocation().GetLineSpan()
        | Trivia trivia -> trivia.GetLocation().GetLineSpan()

    let getSyntaxAsync (syntaxReference: SyntaxReference) =
        async {
            let! ct = Async.CancellationToken
            let! syntaxNode = syntaxReference.GetSyntaxAsync(ct) |> Async.AwaitTask

            return syntaxNode
        }

    let getAggregatedModifiers
        (initialSpan: TextSpan)
        (initialModifiers: SyntaxTokenList)
        (namedTypeSymbol: INamedTypeSymbol)
        : SyntaxTokenList =
        let modifiers =
            namedTypeSymbol.DeclaringSyntaxReferences
            |> Seq.filter (fun syntaxReference -> syntaxReference.Span <> initialSpan)
            |> Seq.fold
                (fun state syntaxReference ->
                    let referenceNode = syntaxReference |> getSyntaxAsync |> Async.RunSynchronously
                    let typeNode = referenceNode :?> BaseTypeDeclarationSyntax

                    typeNode.Modifiers
                    |> List.ofSeq
                    |> List.append state)
                (initialModifiers |> List.ofSeq)
            |> Seq.distinctBy (fun (token: SyntaxToken) -> token.RawKind)

        SyntaxTokenList(modifiers)

    let multilineLineSpanToLineSpans (lineSpan: FileLinePositionSpan) (text: string) : FileLinePositionSpan array =
        if lineSpan.StartLinePosition.Line = lineSpan.EndLinePosition.Line then
            [| lineSpan |]
        else
            let splitComment = text.ReplaceLineEndings().Split "\n"

            splitComment
            |> Array.mapi (fun idx comment ->
                let lineSpanPath = lineSpan.Path
                let commentLength = comment.Length

                if idx <> 0 then
                    let line = lineSpan.StartLinePosition.Line + idx
                    let indentSize = commentLength - (comment.TrimStart().Length)

                    FileLinePositionSpan(
                        lineSpanPath,
                        LinePosition(line, indentSize),
                        LinePosition(line, commentLength)
                    )
                else
                    let startPos = lineSpan.StartLinePosition

                    FileLinePositionSpan(
                        lineSpanPath,
                        startPos,
                        LinePosition(startPos.Line, commentLength + startPos.Character)
                    ))

    let rec visit (nodeInfo: U2<SyntaxNode, SimpleNameSyntaxData>) =
        let node, identifierLineSpan, referenceSymbol =
            match nodeInfo with
            | First node -> (node, None, None)
            | Second { IdentifierLineSpan = lineSpan
                       ReferenceNode = referenceNode
                       ReferenceSymbol = referenceSymbol } -> (referenceNode, Some lineSpan, Some referenceSymbol)

        let getLineSpan (treeItem: TreeItem) =
            match identifierLineSpan with
            | None -> getLineSpan treeItem
            | Some lineSpan -> lineSpan

        let getNamedTypeSymbol (node: BaseTypeDeclarationSyntax) =
            match referenceSymbol with
            | None -> semanticModel.GetDeclaredSymbol(node)
            | Some referenceSymbol -> referenceSymbol :?> INamedTypeSymbol

        let collectTypeDeclaration (node: BaseTypeDeclarationSyntax) (tokenType: TokenType) =
            node
            |> getNamedTypeSymbol
            |> getAggregatedModifiers node.Span node.Modifiers
            |> syntaxTokensToSemanticTokenModifiers
            |> (+) TokenModifiers.Declaration
            |> collect tokenType
            <| getLineSpan (Token node.Identifier)

        match node with
        | :? BaseNamespaceDeclarationSyntax as node ->
            getLineSpan (Node node.Name) |> collect TokenType.Namespace TokenModifiers.Declaration

        | :? DelegateDeclarationSyntax as node ->
            let modifiers = syntaxTokensToSemanticTokenModifiers node.Modifiers + TokenModifiers.Declaration

            getLineSpan (Token node.Identifier) |> collect TokenType.Type modifiers

        | :? RecordDeclarationSyntax as node -> collectTypeDeclaration node TokenType.Type
        | :? ClassDeclarationSyntax as node -> collectTypeDeclaration node TokenType.Class
        | :? EnumDeclarationSyntax as node -> collectTypeDeclaration node TokenType.Enum
        | :? InterfaceDeclarationSyntax as node -> collectTypeDeclaration node TokenType.Interface
        | :? StructDeclarationSyntax as node -> collectTypeDeclaration node TokenType.Struct

        | :? TypeParameterListSyntax as node ->
            for typeParameter in node.Parameters do
                getLineSpan (Token typeParameter.Identifier) |> collect TokenType.TypeParameter TokenModifiers.None

        | :? ParameterListSyntax as node ->
            for parameter in node.Parameters do
                // TODO(Unavailable): [Ref, In, Out] parameter modifiers can be interesting to work with
                getLineSpan (Token parameter.Identifier) |> collect TokenType.Parameter TokenModifiers.None

        | :? FieldDeclarationSyntax as node ->
            let modifiers = syntaxTokensToSemanticTokenModifiers node.Modifiers + TokenModifiers.Declaration

            for variable in node.Declaration.Variables do
                getLineSpan (Node variable) |> collect TokenType.Variable modifiers

        | :? LocalDeclarationStatementSyntax as node ->
            let modifiers = syntaxTokensToSemanticTokenModifiers node.Modifiers + TokenModifiers.Declaration

            for variable in node.Declaration.Variables do
                getLineSpan (Node variable)
                |> collect TokenType.Variable modifiers

        | :? PropertyDeclarationSyntax as node ->
            let hasSetOrInitAccessor =
                let accessors = node.AccessorList.Accessors

                match accessors.Count with
                | 2 -> true
                | 1 ->
                    match (accessors.Item 0).Keyword.Kind() with
                    | SyntaxKind.SetKeyword
                    | SyntaxKind.InitKeyword -> true
                    | _ -> false
                | _ -> true // Auto-property

            let mutable modifiers = syntaxTokensToSemanticTokenModifiers node.Modifiers + TokenModifiers.Declaration

            if not hasSetOrInitAccessor then
                modifiers <- modifiers + TokenModifiers.Readonly

            getLineSpan (Token node.Identifier)
            |> collect TokenType.Property modifiers

        // FIX(Unavailable): An Indexer is referred as a function member in the language specification, but technically they are properties
        // FIX(Unavailable): An Indexer doesn't have an identifier ( 'this' could be an alternative )

        | :? EnumMemberDeclarationSyntax as node ->
            let modifiers = TokenModifiers.Declaration + TokenModifiers.Readonly + TokenModifiers.Static

            getLineSpan (Token node.Identifier) |> collect TokenType.EnumMember modifiers

        | :? EventDeclarationSyntax as node ->
            let modifiers = syntaxTokensToSemanticTokenModifiers node.Modifiers + TokenModifiers.Declaration

            getLineSpan (Token node.Identifier) |> collect TokenType.Event modifiers

        | :? EventFieldDeclarationSyntax as node ->
            let modifiers = syntaxTokensToSemanticTokenModifiers node.Modifiers + TokenModifiers.Declaration

            for variable in node.Declaration.Variables do
                getLineSpan (Node variable) |> collect TokenType.Event modifiers

        // SimpleLambdaExpressionSyntax, ParenthesizedLambdaExpressionSyntax, AnonymousMethodExpressionSyntax -> Functions
        // FIX(Unavailable): What would be the best way to report these nodes?, since the don't have identifiers:
        // For lambdas an alternative would be '=>' ( Lambda operator ), and for anonymous methods the 'delegate' keyword

        | :? MethodDeclarationSyntax as node ->
            let modifiers = syntaxTokensToSemanticTokenModifiers node.Modifiers + TokenModifiers.Declaration

            getLineSpan (Token node.Identifier) |> collect TokenType.Method modifiers

        | :? LocalFunctionStatementSyntax as node ->
            let modifiers = syntaxTokensToSemanticTokenModifiers node.Modifiers + TokenModifiers.Declaration

            getLineSpan (Token node.Identifier) |> collect TokenType.Method modifiers

        // FIX(Unavailable): A Constructor is referred as a function member in the language specification, but technically they are methods
        | :? ConstructorDeclarationSyntax as node ->
            let modifiers = syntaxTokensToSemanticTokenModifiers node.Modifiers + TokenModifiers.Declaration

            getLineSpan (Token node.Identifier) |> collect TokenType.Method modifiers

        // FIX(Unavailable): A Finalizer is referred as a function member in the language specification, but technically they are methods
        | :? DestructorDeclarationSyntax as node ->
            getLineSpan (Token node.Identifier) |> collect TokenType.Method TokenModifiers.Declaration

        | :? LiteralExpressionSyntax as node ->
            let literalToken = node.Token
            let literalTokenLineSpan = getLineSpan (Token literalToken)

            match node.Kind() with
            // TODO(Unavailable): Find regex expressions
            // FIX(Unavailable): Interpolated strings are not being collected
            | SyntaxKind.StringLiteralExpression ->
                let collect = collect TokenType.String TokenModifiers.None

                if literalToken.IsVerbatimStringLiteral() then
                    for lineSpan in multilineLineSpanToLineSpans literalTokenLineSpan literalToken.Text do
                        collect lineSpan
                else
                    collect literalTokenLineSpan

            | SyntaxKind.NumericLiteralExpression -> collect TokenType.Number TokenModifiers.None literalTokenLineSpan

            | _ -> ()

        | :? AttributeSyntax as node ->
            getLineSpan (Node node.Name) |> collect TokenType.Decorator TokenModifiers.None

        | :? SimpleNameSyntax as node ->
            let rec getRealParent (node: SyntaxNode) =
                match node with
                | null -> None
                | node ->
                    let parent = node.Parent

                    match parent with
                    | :? QualifiedNameSyntax -> getRealParent parent
                    | _ -> Some parent

            match getRealParent node with
            | None -> ()
            | Some parent ->
                match parent with
                // Ignore already collected syntax nodes
                | :? BaseNamespaceDeclarationSyntax | :? AttributeSyntax -> ()
                | _ ->
                    // FIX(Unavailable): Check if symbol was found
                    let symbol = semanticModel.GetSymbolInfo(node).Symbol
                    let modifier = TokenModifiers.None
                    let lineSpan = getLineSpan (Token node.Identifier)

                    match symbol with
                    | :? INamespaceSymbol -> collect TokenType.Namespace modifier lineSpan

                    | :? INamedTypeSymbol as symbol when symbol.TypeKind = TypeKind.Enum ->
                        collect TokenType.Enum modifier lineSpan

                    | :? INamedTypeSymbol as symbol when symbol.TypeKind = TypeKind.Interface ->
                        collect TokenType.Interface modifier lineSpan

                    | :? ITypeParameterSymbol -> collect TokenType.TypeParameter modifier lineSpan

                    | :? IParameterSymbol -> collect TokenType.Parameter modifier lineSpan

                    // FIX(Unavailable): A Finalizer is referred as a function member in the language specification, but technically they are methods
                    | :? IMethodSymbol as symbol when symbol.MethodKind = MethodKind.Destructor ->
                        collect TokenType.Method modifier lineSpan

                    | _ ->
                        let declaringSyntaxReferences = symbol.DeclaringSyntaxReferences

                        // FIX(Unavailable): DeclaringSyntaxReferences can not be inferred from metadata symbols:
                        // FIX(Unavailable): I can (1) get the TokenType through the SymbolKind, and get _some_ modifiers through the Is[Modifier] methods,
                        // FIX(Unavailable):       (2) just ignore all metadata symbols, or
                        // FIX(Unavailable):       (3) parse the whole metadata source file from scope.DecompiledMetadata
                        if not declaringSyntaxReferences.IsEmpty then
                            visit (
                                Second
                                    { IdentifierLineSpan = lineSpan
                                      ReferenceNode = declaringSyntaxReferences[0]
                                        |> getSyntaxAsync
                                        |> Async.RunSynchronously
                                      ReferenceSymbol = symbol }
                            )

        // TODO(Unavailable): Match types that can be deprecated: https://docs.microsoft.com/en-us/dotnet/api/system.obsoleteattribute?view=net-6.0
        // TODO(Unavailable): DefaultLibrary modifier specification: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/standard-library

        | _ -> ()

    let solveOverlapping (semanticTokens: SemanticTokenData list) : SemanticTokenData list =
        // FIX(Unavailable): Change names?
        let (<) (left: LinePosition) (right: LinePosition) = left.CompareTo(right) < 0
        let (>) (left: LinePosition) (right: LinePosition) = left.CompareTo(right) > 0
        let (<=) (left: LinePosition) (right: LinePosition) = left.CompareTo(right) <= 0
        let (>=) (left: LinePosition) (right: LinePosition) = left.CompareTo(right) >= 0

        let mutable tokens = []

        let findNextOverlappingToken (pos: LinePosition) : SemanticTokenData option =
            let rec loop =
                function
                | [] -> None
                | token :: tokens ->
                    let startPos = token.LineSpan.StartLinePosition
                    let endPos = token.LineSpan.EndLinePosition

                    if startPos <= pos && endPos >= pos then
                        Some token
                    else if endPos < pos then
                        None
                    else
                        loop tokens

            loop tokens

        for current in semanticTokens do
            let startPos = current.LineSpan.StartLinePosition
            let endPos = current.LineSpan.EndLinePosition

            match findNextOverlappingToken startPos with
            | None -> tokens <- current :: tokens
            | Some { LineSpan = leftSpan
                     Type = leftTokenType
                     Modifiers = leftTokenModifiers } ->

                let maybeRight = findNextOverlappingToken endPos

                tokens <- current :: tokens.Tail

                if leftSpan.StartLinePosition < startPos then
                    let token =
                        { LineSpan =
                            FileLinePositionSpan(
                                leftSpan.Path,
                                leftSpan.StartLinePosition,
                                current.LineSpan.StartLinePosition
                            )
                          Type = leftTokenType
                          Modifiers = leftTokenModifiers }

                    tokens <- tokens.Head :: token :: tokens.Tail

                match maybeRight with
                | Some { LineSpan = rightSpan
                         Type = rightTokenType
                         Modifiers = rightTokenModifiers } ->

                    if rightSpan.EndLinePosition > endPos then
                        let token =
                            { LineSpan =
                                FileLinePositionSpan(
                                    rightSpan.Path,
                                    current.LineSpan.EndLinePosition,
                                    rightSpan.EndLinePosition
                                )
                              Type = rightTokenType
                              Modifiers = rightTokenModifiers }

                        tokens <- token :: tokens
                | None -> ()

        tokens |> List.rev

    let lspTokensDataForSemanticTokensData (semanticTokens: SemanticTokenData list) : uint32 array =
        let tokens = Array.zeroCreate (semanticTokens.Length * 5)
        let mutable lastLine, lastStartChar = 0u, 0u
        
        semanticTokens
        |> List.iteri (fun idx token ->
            let startPos = token.LineSpan.StartLinePosition
            let endPos = token.LineSpan.EndLinePosition

            let line = uint startPos.Line
            let startChar = uint startPos.Character
            let deltaLine = line - lastLine

            let deltaStarChar =
                match deltaLine with
                | 0u -> startChar - lastStartChar
                | _ -> startChar

            lastLine <- line
            lastStartChar <- startChar

            let len = (idx + 1) * 5 - 5
            tokens[len]     <- deltaLine
            tokens[len + 1] <- deltaStarChar
            tokens[len + 2] <- uint endPos.Character - startChar
            tokens[len + 3] <- token.Type
            tokens[len + 4] <- token.Modifiers)

        tokens

    member _.GetTokens() =
        collectedTokens
        |> List.sortWith (fun { LineSpan = firstLineSpan } { LineSpan = secondLineSpan } ->
            let firstStartPos = firstLineSpan.StartLinePosition
            let secondStartPos = secondLineSpan.StartLinePosition

            match firstStartPos = secondStartPos with
            | true -> firstLineSpan.EndLinePosition.CompareTo secondLineSpan.EndLinePosition
            | false -> firstStartPos.CompareTo secondStartPos)
        |> solveOverlapping
        |> lspTokensDataForSemanticTokensData

    override _.Visit node =
        First node |> visit
        base.Visit node

    override _.VisitToken token =
        let kind = token.Kind()
        let lineSpan = getLineSpan (Token token)

        // TODO(Unavailable): Collect Modifiers as SemanticTokenType.Modifiers
        if token.IsKeyword() || kind |> SyntaxFacts.IsPreprocessorKeyword then
            collect TokenType.Keyword TokenModifiers.None lineSpan

        // TODO(Unavailable): Collect more operators
        else if kind = SyntaxKind.DotToken then
            collect TokenType.Operator TokenModifiers.None lineSpan

        base.VisitToken token

    override _.VisitTrivia trivia =
        let triviaLineSpan = getLineSpan (Trivia trivia)

        match trivia.Kind() with
        | SyntaxKind.SingleLineCommentTrivia -> collect TokenType.Comment TokenModifiers.None triviaLineSpan

        | SyntaxKind.MultiLineCommentTrivia ->
            for lineSpan in trivia.ToString() |> multilineLineSpanToLineSpans triviaLineSpan do
                collect TokenType.Comment TokenModifiers.None lineSpan

        // TODO(Unavailable): Collect Tags, Keywords, and Identifiers inside Xml comments
        | SyntaxKind.SingleLineDocumentationCommentTrivia ->
            collect TokenType.Comment TokenModifiers.Documentation triviaLineSpan

        | SyntaxKind.MultiLineDocumentationCommentTrivia ->
            for lineSpan in trivia.ToString() |> multilineLineSpanToLineSpans triviaLineSpan do
                collect TokenType.Comment TokenModifiers.Documentation lineSpan

        | _ -> ()

        base.VisitTrivia trivia

#nowarn "0086"
