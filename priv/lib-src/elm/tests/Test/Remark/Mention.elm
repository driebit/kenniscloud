module Test.Remark.Mention exposing (..)

import Expect exposing (Expectation)
import Remark.Mention as Mention exposing (Candidate, findCandidate)
import Test exposing (..)


testFindCandidate : String -> (Candidate -> String) -> Maybe String
testFindCandidate text field =
    Mention.findCandidate text (String.length text - 1)
        |> Maybe.map field


testFindCandidatePointOffset : String -> (Candidate -> String) -> Int -> Maybe String
testFindCandidatePointOffset text field offset =
    Mention.findCandidate text (String.length text - 1 - offset)
        |> Maybe.map field


suite : Test
suite =
    describe "The Remark Mention module"
        [ describe "Remark.findCandidate candidate field"
            [ test "Mention at start of a text" <|
                \_ ->
                    Expect.equal (Just "jimi") (testFindCandidate "@jimi" .candidate)
            , test "Mention following a space" <|
                \_ ->
                    Expect.equal (Just "jimi") (testFindCandidate " @jimi" .candidate)
            , test "Mention following a new line" <|
                \_ ->
                    Expect.equal (Just "jimi") (testFindCandidate "\n@jimi" .candidate)
            , test "Mention following a tab" <|
                \_ ->
                    Expect.equal (Just "jimi") (testFindCandidate "\t@jimi" .candidate)
            , test "Point at middle mention" <|
                \_ ->
                    Expect.equal (Just "jimi") (testFindCandidatePointOffset "@jimi" .candidate 2)
            , test "Point at start mention" <|
                \_ ->
                    Expect.equal (Just "jimi") (testFindCandidatePointOffset "@jimi" .candidate 4)
            , test "No mention" <|
                \_ ->
                    Expect.equal Nothing (testFindCandidate "jimi" .candidate)
            , test "Mention following other text" <|
                \_ ->
                    Expect.equal Nothing (testFindCandidate "sometext@jimi" .candidate)
            , test "Space following a mention" <|
                \_ ->
                    Expect.equal Nothing (testFindCandidate "@jimi " .candidate)
            ]
        , describe "Remark.findCandidate before field"
            [ test "No text followed by mention" <|
                \_ ->
                    Expect.equal (Just "") (testFindCandidate "@jimi" .before)
            , test "Some text followed by mention" <|
                \_ ->
                    Expect.equal (Just "Last night I went to see ") (testFindCandidate "Last night I went to see @jimi" .before)
            ]
        , describe "Remark.findCandidate after field"
            [ test "Some text following a mention" <|
                \_ ->
                    Expect.equal Nothing (testFindCandidate "@jimi at Woodstock" .after)
            ]
        ]
