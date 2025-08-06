module Pages.Ex47Test exposing (..)

import Expect
import Http
import Json.Decode as Decode
import Pages.Ex47 as Ex47
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


suite : Test
suite =
    describe "Ex47 Module"
        [ describe "init"
            [ test "initializes with empty state" <|
                \_ ->
                    Ex47.init
                        |> Expect.equal
                            { astroData = Ok Nothing
                            , loading = False
                            }
            ]
        , describe "update"
            [ test "FetchData sets loading to true and clears error" <|
                \_ ->
                    let
                        initialModel =
                            { astroData = Err Http.Timeout
                            , loading = False
                            }

                        ( newModel, _ ) =
                            Ex47.update Ex47.FetchData initialModel
                    in
                    newModel
                        |> Expect.equal
                            { astroData = Err Http.Timeout
                            , loading = True
                            }
            , test "GotData with Ok result updates astroData and sets loading to false" <|
                \_ ->
                    let
                        astroData =
                            { people = [ { name = "John Doe", craft = "ISS" } ]
                            , number = 1
                            , message = "success"
                            }

                        initialModel =
                            { astroData = Ok Nothing
                            , loading = True
                            }

                        ( newModel, _ ) =
                            Ex47.update (Ex47.GotData (Ok astroData)) initialModel
                    in
                    newModel
                        |> Expect.equal
                            { astroData = Ok <| Just astroData
                            , loading = False
                            }
            , test "GotData with Err result sets error and loading to false" <|
                \_ ->
                    let
                        initialModel =
                            { astroData = Ok Nothing
                            , loading = True
                            }

                        ( newModel, _ ) =
                            Ex47.update (Ex47.GotData (Err Http.NetworkError)) initialModel
                    in
                    newModel
                        |> Expect.equal
                            { astroData = Err Http.NetworkError
                            , loading = False
                            }
            ]
        , describe "view"
            [ test "renders fetch button" <|
                \_ ->
                    Ex47.init
                        |> Ex47.view
                        |> Query.fromHtml
                        |> Query.find [ tag "button" ]
                        |> Query.has [ text "Fetch Astronaut Data" ]
            , test "shows initial message when no data" <|
                \_ ->
                    Ex47.init
                        |> Ex47.view
                        |> Query.fromHtml
                        |> Query.has [ text "Click the button to fetch data" ]
            , test "shows loading message when loading" <|
                \_ ->
                    { astroData = Ok Nothing
                    , loading = True
                    }
                        |> Ex47.view
                        |> Query.fromHtml
                        |> Query.has [ text "Loading..." ]
            , test "shows error message when error exists" <|
                \_ ->
                    { astroData = Err Http.NetworkError
                    , loading = False
                    }
                        |> Ex47.view
                        |> Query.fromHtml
                        |> Query.has [ text "Network error" ]
            , test "renders table with data when astroData exists" <|
                \_ ->
                    let
                        astroData =
                            { people = 
                                [ { name = "John Doe", craft = "ISS" }
                                , { name = "Jane Smith", craft = "Soyuz" }
                                ]
                            , number = 2
                            , message = "success"
                            }

                        model =
                            { astroData = Ok <| Just astroData
                            , loading = False
                            }
                    in
                    Ex47.view model
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ text "Total people in space: 2" ]
                            , Query.find [ tag "table" ]
                                >> Query.has [ tag "thead" ]
                            , Query.find [ tag "table" ]
                                >> Query.has [ tag "tbody" ]
                            , Query.has [ text "John Doe" ]
                            , Query.has [ text "Jane Smith" ]
                            , Query.has [ text "ISS" ]
                            , Query.has [ text "Soyuz" ]
                            ]
            , test "table has correct headers" <|
                \_ ->
                    let
                        astroData =
                            { people = [ { name = "John Doe", craft = "ISS" } ]
                            , number = 1
                            , message = "success"
                            }

                        model =
                            { astroData = Ok <| Just astroData
                            , loading = False
                            }
                    in
                    Ex47.view model
                        |> Query.fromHtml
                        |> Query.find [ tag "thead" ]
                        |> Expect.all
                            [ Query.has [ text "Name" ]
                            , Query.has [ text "Craft" ]
                            ]
            ]
        , describe "JSON decoders"
            [ test "personDecoder decodes person correctly" <|
                \_ ->
                    let
                        json = """{"name": "John Doe", "craft": "ISS"}"""
                        expected = { name = "John Doe", craft = "ISS" }
                    in
                    Decode.decodeString Ex47.personDecoder json
                        |> Expect.equal (Ok expected)
            , test "astroDataDecoder decodes full response correctly" <|
                \_ ->
                    let
                        json = """
                        {
                            "people": [
                                {"name": "John Doe", "craft": "ISS"},
                                {"name": "Jane Smith", "craft": "Soyuz"}
                            ],
                            "number": 2,
                            "message": "success"
                        }
                        """
                        expected = 
                            { people = 
                                [ { name = "John Doe", craft = "ISS" }
                                , { name = "Jane Smith", craft = "Soyuz" }
                                ]
                            , number = 2
                            , message = "success"
                            }
                    in
                    Decode.decodeString Ex47.astroDataDecoder json
                        |> Expect.equal (Ok expected)
            ]
        ]
