module Crowd.Assets exposing (imageUrl, meetup, person, tag)

import Url
import Url.Builder as Url


person : String
person =
    imageUrl "Icon_Person.png"


tag : String
tag =
    imageUrl "Icon_Tag.png"


meetup : String
meetup =
    imageUrl "Icon_Meetup.png"


imageUrl : String -> String
imageUrl img =
    Url.absolute [ "lib", "images", img ] []
