module Viewport exposing (Viewport, trimViewport)


type alias Viewport =
    { height : Float
    , width : Float
    }


trimViewport : Viewport -> Viewport
trimViewport v =
    let
        minSide =
            Basics.min v.height v.width
    in
    Viewport minSide minSide
