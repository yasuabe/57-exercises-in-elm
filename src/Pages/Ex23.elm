module Pages.Ex23 exposing (Diagnosis, LastStep(..), Model, Msg(..), init, traverse, update, view)

import Html exposing (Html, div, input, label, span, text)
import Html.Attributes exposing (checked, name, style, type_)
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


sample : Diagnosis
sample =
    { steps =
        [ ( "question a", True )
        , ( "looooooooooooooooooong question", False )
        ]
    , lastStep = Solution "You are a human."
    }


type alias Model =
    { decisions : List Bool }


init : Model
init =
    { decisions = []
    }


traverse : List Bool -> Diagnosis
traverse decisions =
    let
        f : List Answer -> List Bool -> Tree String -> Diagnosis
        f answers decisions_ tree =
            case ( decisions_, tree ) of
                ( _, Leaf msg ) ->
                    { steps = answers, lastStep = Solution msg }

                ( [], Node msg _ _ ) ->
                    { steps = answers, lastStep = Question msg }

                ( False :: ds, Node msg _ r ) ->
                    f (answers ++ [ ( msg, False ) ]) ds r

                ( True :: ds, Node msg l _ ) ->
                    f (answers ++ [ ( msg, True ) ]) ds l
    in
    f [] decisions decisionTree



-- Ok { steps = [], lastStep = Question "" }
-- MSG


type Msg
    = Submit
    | SelectAnswer Int Bool



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            ( init
            , Cmd.none
            )

        SelectAnswer index answer ->
            ( { decisions = (List.take index <| model.decisions) ++ [answer] }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ span [] [ text "header" ]
            ]
        , renderDiagnosis model.decisions
        ]



-- TODO: define a function to render radio buttons


renderDiagnosis : List Bool -> Html Msg
renderDiagnosis decisions =
    let
        f : Int -> Maybe Bool -> List (Html Msg)
        f n check =
            let
                ( yes, no ) =
                    Maybe.map (\b -> ( [ checked b ], [ checked (not b) ] )) check
                        |> withDefault ( [], [] )

                number =
                    String.fromInt n
            in
            [ input ([ type_ "radio", name number, onClick (SelectAnswer n True) ] ++ yes) []
            , label [] [ text "yes" ]
            , input ([ type_ "radio", name number, onClick (SelectAnswer n False)  ] ++ no) []
            , label [] [ text "no" ]
            ]

        diagnosis : Diagnosis
        diagnosis =
            traverse decisions

        lastRow : Html Msg
        lastRow =
            case diagnosis.lastStep of
                Question q ->
                    div []
                        (span [] [ text q ] :: f (length diagnosis.steps) Nothing)

                Solution s ->
                    div [] [ text s ]

        steps : List (Html Msg)
        steps =
            indexedMap
                (\n ( q, a ) ->
                    div []
                        (span [] [ text q ] :: (f n <| Just a))
                )
                diagnosis.steps
    in
    div [] <| steps ++ [ lastRow ]
