module Point exposing (Point, create, distance, subtract, toString)


type alias Point =
    { x : Float
    , y : Float
    }


create : Float -> Float -> Point
create x y =
    { x = x
    , y = y
    }


distance : Point -> Point -> Float
distance a b =
    sqrt ((a.x - b.x) ^ 2 + (a.y - b.y) ^ 2)


subtract : Point -> Point -> Point
subtract a b =
    create (a.x - b.x) (a.y - b.y)


add : Point -> Point -> Point
add a b =
    create (a.x + b.x) (a.y + b.y)


toString : Point -> String
toString p =
    String.concat [ String.fromFloat p.x, ",", String.fromFloat p.y ]
