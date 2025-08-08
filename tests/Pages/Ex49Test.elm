module Pages.Ex49Test exposing (..)

import Expect
import Pages.Ex49 exposing (decode)
import Test exposing (..)
import Test.Html.Selector exposing (..)



suite : Test
suite =
    describe "Ex49 Module"
        [ describe "decode"
            [ test "should decode the given Flickr feed into the list of Items" <|
                \_ ->
                    decode sampleResponse
                        |> Expect.equal
                            (Ok
                                [ { title = "HIDDEN GARDENS"
                                  , url = "https://live.staticflickr.com/65535/54704464772_f145f9d166_m.jpg"
                                  , date_taken = "2025-08-07T03:06:14-08:00"
                                  , author = """nobody@flickr.com ("trisha owen")"""
                                  }
                                , { title = "Ferrari F40 1987"
                                  , url = "https://live.staticflickr.com/65535/54704465152_a0008a07dc_m.jpg"
                                  , date_taken = "2025-06-05T15:16:38-08:00"
                                  , author = """nobody@flickr.com ("Dr. Dwarf")"""
                                  }
                                ]
                            )
            ]
        ]


sampleResponse : String
sampleResponse =
    """jsonFlickrFeed({
        "title": "Uploads from everyone",
        "link": "https:\\/\\/www.flickr.com\\/photos\\/",
        "description": "",
        "modified": "2025-08-07T10:06:14Z",
        "generator": "https:\\/\\/www.flickr.com",
        "items": [
       {
            "title": "HIDDEN GARDENS",
            "link": "https:\\/\\/www.flickr.com\\/photos\\/191917081@N04\\/54704464772\\/",
            "media": {"m":"https:\\/\\/live.staticflickr.com\\/65535\\/54704464772_f145f9d166_m.jpg"},
            "date_taken": "2025-08-07T03:06:14-08:00",
            "description": " <p><a href=\\"https:\\/\\/www.flickr.com\\/people\\/191917081@N04\\/\\">trisha owen<\\/a> posted a photo:<\\/p> <p><a href=\\"https:\\/\\/www.flickr.com\\/photos\\/191917081@N04\\/54704464772\\/\\" title=\\"HIDDEN GARDENS\\"><img src=\\"https:\\/\\/live.staticflickr.com\\/65535\\/54704464772_f145f9d166_m.jpg\\" width=\\"240\\" height=\\"173\\" alt=\\"HIDDEN GARDENS\\" \\/><\\/a><\\/p> <p>Blue Thistle Flower mainly refers to the &quot;Globe <br \\/> Thistle&quot;. [ Echinops ] Spherical heads on tall stems.<br \\/> Other plants include [ Eryngium ] species.<\\/p> ",
            "published": "2025-08-07T10:06:14Z",
            "author": "nobody@flickr.com (\\"trisha owen\\")",
            "author_id": "191917081@N04",
            "tags": ""
       },
       {
            "title": "Ferrari F40 1987",
            "link": "https:\\/\\/www.flickr.com\\/photos\\/davide_trasmunti\\/54704465152\\/",
            "media": {"m":"https:\\/\\/live.staticflickr.com\\/65535\\/54704465152_a0008a07dc_m.jpg"},
            "date_taken": "2025-06-05T15:16:38-08:00",
            "description": " <p><a href=\\"https:\\/\\/www.flickr.com\\/people\\/davide_trasmunti\\/\\">Dr. Dwarf<\\/a> posted a photo:<\\/p> <p><a href=\\"https:\\/\\/www.flickr.com\\/photos\\/davide_trasmunti\\/54704465152\\/\\" title=\\"Ferrari F40 1987\\"><img src=\\"https:\\/\\/live.staticflickr.com\\/65535\\/54704465152_a0008a07dc_m.jpg\\" width=\\"163\\" height=\\"240\\" alt=\\"Ferrari F40 1987\\" \\/><\\/a><\\/p> <p><\\/p> ",
            "published": "2025-08-07T10:06:38Z",
            "author": "nobody@flickr.com (\\"Dr. Dwarf\\")",
            "author_id": "199917264@N08",
            "tags": "sony a7r4 maranello sonya7r4 sonya7 sonya7r ferrari museo enzo ita italia italy red rosso rossa car macchine macchina design curve cavalli motori motore motor f40 f 40 1987 legend"
        }
        ]
    }
    """
