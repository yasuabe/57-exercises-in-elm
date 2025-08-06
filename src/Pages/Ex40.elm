-- Ex40: Filtering Records
--
-- - Create a program to filter employee records.
-- - Search is based on first or last name containing a given substring.
-- - Display matching records in a formatted table.
-- - Data should be stored in an array of maps (or equivalent structure).


module Pages.Ex40 exposing (EmployeeRecord, Model, Msg(..), filterEmployees, init, update, view)

import Common.CmdEx exposing (withNone)
import Common.Events exposing (onBlur, onEnter)
import Common.Function exposing (on)
import Common.MaybeEx exposing (toMaybe)
import Html exposing (Html, div, fieldset, input, legend, option, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onInput)
import List.Extra as LX
import Maybe.Extra as MX
import String exposing (contains, toLower)



-- MODEL


type alias EmployeeRecord =
    { firstName : String
    , lastName : String
    , position : String
    , separation : Maybe String
    }


employees : List EmployeeRecord
employees =
    [ EmployeeRecord "John" "Johnson" "Manager" (Just "2016-12-31")
    , EmployeeRecord "Tou" "Xiong" "Software Engineer" (Just "2016-10-05")
    , EmployeeRecord "Michaela" "Michaelson" "District Manager" (Just "2015-12-19")
    , EmployeeRecord "Jake" "Jacobson" "Programmer" Nothing
    , EmployeeRecord "Jacquelyn" "Jackson" "DBA" Nothing
    , EmployeeRecord "Sally" "Weber" "Web Developer" (Just "2015-12-18")
    ]


type alias Model =
    { nameFilter : Maybe String
    , positionFilter : Maybe String
    }


init : Model
init =
    { nameFilter = Nothing
    , positionFilter = Nothing
    }



-- MSG


type Msg
    = NameChanged String
    | PositionChanged String


nameFilter : Model -> EmployeeRecord -> Bool
nameFilter model employee =
    model.nameFilter
        |> Maybe.map
            (\filterValue ->
                on (||)
                    (toLower >> contains (toLower filterValue))
                    employee.firstName
                    employee.lastName
            )
        |> Maybe.withDefault True


positionFilter : Model -> EmployeeRecord -> Bool
positionFilter model employee =
    model.positionFilter
        |> Maybe.map ((==) employee.position)
        |> Maybe.withDefault True


filterEmployees : Model -> List EmployeeRecord
filterEmployees model =
    let
        composedFilter employee =
            List.all (\f -> f model employee) [ nameFilter, positionFilter ]
    in
    List.filter composedFilter employees



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameChanged name ->
            withNone { model | nameFilter = toMaybe name }

        PositionChanged position ->
            withNone { model | positionFilter = MX.filter ((/=) "none") <| toMaybe position }



-- VIEW


viewNameFilter : Html Msg
viewNameFilter =
    div
        [ class "inputline" ]
        [ span [] [ text "First or Last Name: " ]
        , input
            [ class "filter-input"
            , type_ "text"
            , onEnter NameChanged
            , onBlur NameChanged
            ]
            []
        ]


viewPositionFilter : Html Msg
viewPositionFilter =
    div
        [ class "inputline" ]
        [ span [] [ text "Position: " ]
        , select
            [ onInput PositionChanged ]
            (option [ value "none" ] [ text "---" ]
                :: (employees
                        |> List.map .position
                        |> LX.unique
                        |> List.map (\position -> option [ value position ] [ text position ])
                   )
            )
        ]


viewFilters : Html Msg
viewFilters =
    fieldset [ class "ex40__filters" ]
        [ legend [] [ text "Filter Employees" ]
        , viewNameFilter
        , viewPositionFilter
        ]


viewEmployeeTable : Model -> Html Msg
viewEmployeeTable model =
    table [ class "contents-table" ]
        [ thead []
            [ tr []
                [ th [] [ text "First Name" ]
                , th [] [ text "Last Name" ]
                , th [] [ text "Position" ]
                , th [] [ text "Separation Date" ]
                ]
            ]
        , tbody []
            (List.map
                (\employee ->
                    tr []
                        [ td [] [ text employee.firstName ]
                        , td [] [ text employee.lastName ]
                        , td [] [ text employee.position ]
                        , td [] [ text (Maybe.withDefault "N/A" employee.separation) ]
                        ]
                )
                (filterEmployees model)
            )
        ]


view : Model -> Html Msg
view model =
    div [ class "ex40" ]
        [ div []
            [ viewFilters
            , viewEmployeeTable model
            ]
        ]
