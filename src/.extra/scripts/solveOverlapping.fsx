#r "nuget: Microsoft.CodeAnalysis.CSharp";;
#r "nuget: Microsoft.CodeAnalysis.Common";;

open Microsoft.CodeAnalysis;;
open Microsoft.CodeAnalysis.Text;;

type SemanticTokenData =
    { LineSpan: FileLinePositionSpan
      Type: uint32
      Modifiers: uint32 }

let logTokens msg tokens =
    printfn "%s" msg
    for token in tokens |> List.rev do
        let startChar = token.LineSpan.StartLinePosition.Character
        let endChar = token.LineSpan.EndLinePosition.Character

        printfn "{ Start = %d; End = %d; Type = %d Modifiers = %d }" startChar endChar token.Type token.Modifiers

let solveOverlapping (semanticTokens : SemanticTokenData list): SemanticTokenData list =
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
                let startPos =
                    token.LineSpan.StartLinePosition

                let endPos = token.LineSpan.EndLinePosition

                if startPos <= pos && endPos >= pos then
                    Some token
                else if endPos < pos then
                    None
                else
                    loop tokens

        loop tokens

    for current in semanticTokens do
        let startPos =
            current.LineSpan.StartLinePosition

        let endPos =
            current.LineSpan.EndLinePosition

        match findNextOverlappingToken startPos with
        | None -> tokens <- current :: tokens
        | Some { LineSpan = leftSpan
                 Type = leftTokenType
                 Modifiers = leftTokenModifiers } ->

            let maybeRight =
                findNextOverlappingToken endPos

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

let create startPoint endPoint (tokenType : int32) (tokenModifier : int32) =
    { LineSpan =
        FileLinePositionSpan(
            "",
            LinePosition(0, startPoint),
            LinePosition(0, endPoint)
        )
      Type = uint tokenType
      Modifiers = uint tokenModifier }

let tokens = [
    create 0 9 15 0
    create 10 1 0 1
    create 11 1 21 0
]

let solvedTokens = tokens |> solveOverlapping

logTokens "Result" solvedTokens
