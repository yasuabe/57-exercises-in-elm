module Pages.Ex23 exposing (Diagnosis, LastStep(..), Model, Msg(..), init, traverse, update, view)

import Common.CmdEx exposing (withNone)
import Html exposing (Html, div, input, label, span, text)
import Html.Attributes exposing (checked, class, name, type_)
import Html.Events exposing (onClick)
import List exposing (indexedMap, length)
import Maybe exposing (withDefault)



-- MODEL


type Tree a
    = Leaf a
    | Node a (Tree a) (Tree a)


decisionTree : Tree String
decisionTree =
    Node
        "Is the car silent when you turn the key? "
        (Node
            "Are the battery terminals corroded? "
            (Leaf "Clean terminals and try starting again.")
            (Leaf "Replace cables and try again.")
        )
        (Node
            "Does the car make a clicking noise? "
            (Leaf "Replace the battery.")
            (Node
                "Does the car crank up but fail to start? "
                (Leaf "Check spark plug connections.")
                (Node
                    "Does the engine start and then die? "
                    (Node
                        "Does your car have fuel injection? "
                        (Leaf "Get it in for service.")
                        (Leaf "Check to ensure the choke is opening and closing.")
                    )
                    (Leaf "---")
                )
            )
        )


type alias Answer =
    ( String, Bool )


type LastStep
    = Question String
    | Solution String


type alias Diagnosis =
    { steps : List Answer
    , lastStep : LastStep
    }


type alias Model =
    List Bool


init : Model
init =
    []


traverse : List Bool -> Diagnosis
traverse decisions =
    let
        loop answers decisions_ tree =
            case ( decisions_, tree ) of
                ( _, Leaf msg ) ->
                    { steps = answers, lastStep = Solution msg }

                ( [], Node msg _ _ ) ->
                    { steps = answers, lastStep = Question msg }

                ( False :: ds, Node msg _ r ) ->
                    loop (answers ++ [ ( msg, False ) ]) ds r

                ( True :: ds, Node msg l _ ) ->
                    loop (answers ++ [ ( msg, True ) ]) ds l
    in
    loop [] decisions decisionTree



-- MSG


type Msg
    = SelectAnswer Int Bool



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectAnswer index answer ->
            withNone <| List.take index model ++ [ answer ]



-- VIEW


renderYesNo : Int -> Maybe Bool -> List (Html Msg)
renderYesNo n check =
    let
        ( yes, no ) =
            Maybe.map (\b -> ( [ checked b ], [ checked (not b) ] )) check
                |> withDefault ( [], [] )

        radio selected =
            [ type_ "radio", name <| String.fromInt n, onClick (SelectAnswer n selected) ]
    in
    [ input (radio True ++ yes) []
    , label [] [ text "Yes" ]
    , input (radio False ++ no) []
    , label [] [ text "No" ]
    ]


renderDiagnosis : List Bool -> Html Msg
renderDiagnosis decisions =
    let
        diagnosis =
            traverse decisions

        answered =
            indexedMap
                (\n ( q, a ) -> div [ class "ex23__answered" ] <| span [] [ text q ] :: (renderYesNo n <| Just a))
                diagnosis.steps

        lastRow =
            case diagnosis.lastStep of
                Question q ->
                    div [ class "ex23__questioning" ] <|
                        span [] [ text q ]
                            :: renderYesNo (length diagnosis.steps) Nothing

                Solution s ->
                    div [ class "ex23__solution" ]
                        [ span [] [ text "Diagnosis: " ], text s ]
    in
    div [] <| answered ++ [ lastRow ]


view : Model -> Html Msg
view model =
    div [ class "ex23 "] [ renderDiagnosis model ]
