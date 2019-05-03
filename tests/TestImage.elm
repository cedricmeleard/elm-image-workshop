module TestImage exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Image exposing (..)
import Test exposing (..)



-- DATA


images =
    [ { thumbnailUrl = "image1"
      , url = "image1"
      , width = 3
      , height = 2
      }
    , { thumbnailUrl = "image2"
      , url = "image2"
      , width = 2
      , height = 3
      }
    ]



-- TESTS


suite : Test
suite =
    describe "The Image module"
        [ describe "Image.filterImageFormat with Any"
            [ test "filter image on Any with no image should return []" <|
                \_ ->
                    Expect.equal [] <| filterImageFormat Any []
            , test "filter image on Any with images lanscape and portrait should return all images" <|
                \_ ->
                    Expect.equal images <| filterImageFormat Any images
            ]
        , describe "Image.filterImageFormat with Portrait"
            [ test "filter image on Portrait with no image should return []" <|
                \_ ->
                    Expect.equal [] <| filterImageFormat Portrait []
            , test "filter image on Portrait with images lanscape and portrait should return image2 " <|
                \_ ->
                    Expect.equal
                        [ { thumbnailUrl = "image2"
                          , url = "image2"
                          , width = 2
                          , height = 3
                          }
                        ]
                    <|
                        filterImageFormat Portrait images
            , test "filter image on Landscape with square image should return []" <|
                \_ ->
                    let
                        square =
                            [ { thumbnailUrl = "square"
                              , url = "square"
                              , width = 2
                              , height = 2
                              }
                            ]
                    in
                    Expect.equal
                        []
                    <|
                        filterImageFormat Portrait square
            ]
        , describe "Image.filterImageFormat with Landscape"
            [ test "filter image on Landscape with no image should return []" <|
                \_ ->
                    Expect.equal [] <| filterImageFormat Landscape []
            , test "filter image on Landscape with images lanscape and portrait should return image1 " <|
                \_ ->
                    Expect.equal
                        [ { thumbnailUrl = "image1"
                          , url = "image1"
                          , width = 3
                          , height = 2
                          }
                        ]
                    <|
                        filterImageFormat Landscape images
            , test "filter image on Landscape with square image should return square " <|
                \_ ->
                    let
                        square =
                            [ { thumbnailUrl = "square"
                              , url = "square"
                              , width = 2
                              , height = 2
                              }
                            ]
                    in
                    Expect.equal
                        [ { thumbnailUrl = "square"
                          , url = "square"
                          , width = 2
                          , height = 2
                          }
                        ]
                    <|
                        filterImageFormat Landscape square
            ]
        ]
