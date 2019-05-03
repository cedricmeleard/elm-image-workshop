module Image exposing (Format(..), Image, filterImageFormat, imageListDecoder)

import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import Json.Decode.Pipeline exposing (required, requiredAt)


type alias Image =
    { thumbnailUrl : String
    , url : String
    , width : Int
    , height : Int
    }


type Format
    = Portrait
    | Landscape
    | Any


imageDecoder : Decoder Image
imageDecoder =
    Decode.succeed Image
        |> requiredAt [ "urls", "thumb" ] string
        |> requiredAt [ "urls", "regular" ] string
        |> requiredAt [ "width" ] int
        |> requiredAt [ "height" ] int


imageListDecoder : Decoder (List Image)
imageListDecoder =
    field "results" (list imageDecoder)


filterImageFormat : Format -> List Image -> List Image
filterImageFormat format imageList =
    case format of
        Portrait ->
            List.filter isPortrait imageList

        Landscape ->
            List.filter isLandscape imageList

        _ ->
            imageList


isPortrait : Image -> Bool
isPortrait image =
    image.width // image.height < 1


isLandscape : Image -> Bool
isLandscape image =
    image.width // image.height >= 1
