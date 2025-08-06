module Exercises exposing (..)

import Dict exposing (Dict)


type alias Exercise =
    { suffix : String
    , title : String
    , done : Bool
    }


exercises : List Exercise
exercises =
    [ { suffix = "01", title = "Saying Hello", done = True }
    , { suffix = "02", title = "Counting the Number of Characters", done = True }
    , { suffix = "03", title = "Printing Quotes", done = True }
    , { suffix = "04", title = "Mad Lib", done = True }
    , { suffix = "05", title = "Simple Math", done = False }
    , { suffix = "06", title = "Retirement Calculator", done = False }
    , { suffix = "07", title = "Area of a Rectangular Room", done = True }
    , { suffix = "08", title = "Pizza Party", done = False }
    , { suffix = "09", title = "Paint Calculator", done = False }
    , { suffix = "10", title = "Self-Checkout", done = False }
    , { suffix = "11", title = "Currency Conversion", done = False }
    , { suffix = "12", title = "Computing Simple Interest", done = False }
    , { suffix = "13", title = "Determining Compound Interest", done = True }
    , { suffix = "14", title = "Tax Calculator", done = True }
    , { suffix = "15", title = "Password Validation", done = False }
    , { suffix = "16", title = "Legal Driving Age", done = False }
    , { suffix = "17", title = "Blood Alcohol Calculator", done = False }
    , { suffix = "18", title = "Temperature Converter", done = False }
    , { suffix = "19", title = "BMI Calculator", done = False }
    , { suffix = "20", title = "Multistate Sales Tax Calculator", done = False }
    , { suffix = "21", title = "Numbers to Names", done = False }
    , { suffix = "22", title = "Comparing Numbers", done = False }
    , { suffix = "23", title = "Troubleshooting Car Issues", done = True }
    , { suffix = "24", title = "Anagram Checker", done = True }
    , { suffix = "25", title = "Password Strength Indicator", done = False }
    , { suffix = "26", title = "Months to Pay Off a Credit Card", done = True }
    , { suffix = "27", title = "Validating Inputs", done = False }
    , { suffix = "28", title = "Adding Numbers", done = True }
    , { suffix = "29", title = "Handling Bad Input", done = False }
    , { suffix = "30", title = "Multiplication Table", done = False }
    , { suffix = "31", title = "Karvonen Heart Rate", done = True }
    , { suffix = "32", title = "Guess the Number Game", done = False }
    , { suffix = "33", title = "Magic 8 Ball", done = True }
    , { suffix = "34", title = "Employee List Removal", done = False }
    , { suffix = "35", title = "Picking a Winner", done = False }
    , { suffix = "36", title = "Computing Statistics", done = False }
    , { suffix = "37", title = "Password Generator", done = False }
    , { suffix = "38", title = "Filtering Values", done = False }
    , { suffix = "39", title = "Sorting Records", done = False }
    , { suffix = "40", title = "Filtering Records", done = True }
    , { suffix = "41", title = "Name Sorter", done = True }
    , { suffix = "42", title = "Parsing a Data File", done = False }
    , { suffix = "43", title = "Website Generator", done = False }
    , { suffix = "44", title = "Product Search", done = False }
    , { suffix = "45", title = "Word Finder", done = False }
    , { suffix = "46", title = "Word Frequency Finder", done = True }
    , { suffix = "47", title = "Who’s in Space?", done = True }
    , { suffix = "48", title = "Grabbing the Weather", done = True }
    , { suffix = "49", title = "Flickr Photo Search", done = False }
    , { suffix = "50", title = "Movie Recommendations", done = False }
    , { suffix = "51", title = "Pushing Notes to Firebase", done = False }
    , { suffix = "52", title = "Creating Your Own Time Service", done = True }
    , { suffix = "53", title = "Todo List", done = True }
    , { suffix = "54", title = "URL Shortener", done = False }
    , { suffix = "55", title = "Text Sharing", done = False }
    , { suffix = "56", title = "Tracking Inventory", done = False }
    , { suffix = "57", title = "Trivia App", done = True }
    ]


chapters : Dict String String
chapters =
    Dict.fromList
        [ ( "01", "Chapter 2: Input, Processing, and Output" )
        , ( "07", "Chapter 3: Calculations" )
        , ( "14", "Chapter 4: Making Decisions" )
        , ( "24", "Chapter 5: Functions" )
        , ( "28", "Chapter 6: Repetition" )
        , ( "33", "Chapter 7: Data Structures" )
        , ( "41", "Chapter 8: Working with Files" )
        , ( "47", "Chapter 9: Working with External Services" )
        , ( "53", "Chapter 10: Full Programs" )
        ]


toTitle : Exercise -> String
toTitle { suffix, title } =
    suffix ++ ": " ++ title
