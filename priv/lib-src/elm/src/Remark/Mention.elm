module Remark.Mention exposing (Candidate, findCandidate, insertCandidate)

import Parser exposing ((|.), (|=))
import Types exposing (Mention, PointIndex)


type alias Candidate =
    { before : String
    , candidate : String
    , after : String
    }


findCandidate : String -> PointIndex -> Maybe Candidate
findCandidate text pointIndex =
    let
        chompUntilEnd =
            Parser.chompUntilEndOr "thiswillneverhappen"

        toStringReverse =
            String.fromList >> String.reverse

        prefix =
            String.left (pointIndex + 1) text
                |> String.reverse
                |> Parser.run prefixParser
                |> Result.map (Tuple.mapBoth String.reverse String.reverse)

        prefixParser =
            Parser.succeed Tuple.pair
                |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '@' && c /= ' '))
                |. Parser.symbol "@"
                |= Parser.getChompedString
                    (Parser.oneOf [ Parser.chompIf (\c -> c == ' ' || c == '\n' || c == '\t'), Parser.end ]
                        |. chompUntilEnd
                    )

        postfix =
            String.dropLeft (pointIndex + 1) text
                |> Parser.run postfixParser

        postfixParser =
            Parser.succeed Tuple.pair
                |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ' '))
                |. Parser.oneOf [ Parser.spaces, Parser.end ]
                |= Parser.getChompedString chompUntilEnd
    in
    case ( prefix, postfix ) of
        ( Ok ( left, before ), Ok ( right, after ) ) ->
            Just (Candidate before (left ++ right) after)

        _ ->
            Nothing



-- INSERT CANDIDATE IN TEXT


insertCandidate : Mention -> Candidate -> String
insertCandidate { userName } { before, after } =
    before ++ " @" ++ userName ++ after
