module Main exposing (..)

{-| This is a skeleton for an interpreter application. For now it basically simply display what you type in.
You should just:
  - insert your code in the update and the viewTurtlePath functions,
  - surely add some field in the Model type.
You can of course add other types,
functions and modules but you shouldn't have to modify the code at other places -- if you think you have to modify
this code, reach your teacher out before doing this.
-}

import Browser
import Browser.Dom
import Collage exposing (Collage)
import Collage.Render
import Collage.Text
import Color
import Element exposing (Element, centerX, centerY, column, el, fill, focusStyle, height, padding, paddingEach, px, rgb, rgba, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Task
import Json.Decode exposing (string)
import Basics exposing (..)
import List
import Maybe exposing (Maybe(..))
import Result exposing (Result(..))
import String exposing (String)
import String exposing (split)




type alias Model =
    { commandInput : String
    , history : List String
    }



type Msg
    = CommandEntered String
    | CommandSubmitted 
    | NoOp


init : Model
init =
    { commandInput = ""
    , history = []
    }

lastElem : List a -> Maybe a         
lastElem =            
    List.foldl (Just >> always) Nothing  






update : Msg -> Model -> Model
update msg model =
    
    
    case msg of
        CommandEntered command ->
            
            { model | commandInput = command }
           
        CommandSubmitted->
        
            Debug.log(model.commandInput)

            -- TODO: Command parsing and applying goes here!!
            
          
            { model | commandInput = "", history = model.history ++ [ model.commandInput ]}
           
         
        NoOp ->
            model
   

    


view : Model -> Element Msg
view model =
    
    column [ width fill, height fill ]
        [ el [ width fill, height fill ]
            (el [ Border.width 2, padding 2, centerX, centerY, Background.color (Element.rgb255 52 101 164) ]
                (Element.html (Collage.Render.svgBox ( 800, 800 ) (viewTurtlePathForward ["forward" , "10"])))
            )
        , row [ width fill, height (px 50) ]
            [ el [] (text "Your command: ")
            , Input.text
                [ onEnter CommandSubmitted
                , Element.htmlAttribute (Html.Attributes.id "prompt")
                , width fill
                ]
                { onChange = CommandEntered
                , text = model.commandInput
                , placeholder = Nothing
                , label = Input.labelHidden "Enter the command"
                }
            ]
        ]

viewTurtlePathForward : List String -> Collage Msg
viewTurtlePathForward model =

    

    
    
    -- TODO: change this function! Here are some examples to draw basic shapes.
    -- Feel free to define some helper functions!
    -- Note that unfortunately, the Color.Color and Element.Color types doesn't match.
     Collage.group
        [ -- Drawing a bunch of segments tracing turtle movements.
          Collage.segment ( 0, 0 ) ( 50, 100 )
            |> Collage.traced (Collage.solid 1 (Collage.uniform Color.black))
        , Collage.segment ( 50,100  ) ( 100, 100 )
            |> Collage.traced (Collage.solid 1 (Collage.uniform Color.black))
        , Collage.image ( 60, 60 ) "https://elm-lang.org/images/turtle.gif"
            |> Collage.shift ( 100, 100 )

        -- You should have enough with the above examples, but if you need diving deeper in the `Collage` library
        -- here is the documentation link: https://package.elm-lang.org/packages/timjs/elm-collage/latest/
        ]
       

viewTurtlePath : Model-> Collage Msg
viewTurtlePath model =
    
    -- TODO: change this function! Here are some examples to draw basic shapes.
    -- Feel free to define some helper functions!
    -- Note that unfortunately, the Color.Color and Element.Color types doesn't match.
    Collage.group
        [ -- Drawing a bunch of segments tracing turtle movements.
          Collage.segment ( 0, 0 ) ( 50, 100 )
            |> Collage.traced (Collage.solid 1 (Collage.uniform Color.black))
        , Collage.segment ( 50,100  ) ( 100, 100 )
            |> Collage.traced (Collage.solid 1 (Collage.uniform Color.black))
        , Collage.image ( 60, 60 ) "https://elm-lang.org/images/turtle.gif"
            |> Collage.shift ( 100, 100 )

        -- You should have enough with the above examples, but if you need diving deeper in the `Collage` library
        -- here is the documentation link: https://package.elm-lang.org/packages/timjs/elm-collage/latest/
        ]
       


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                           
                            Decode.succeed msg
                           
                            

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


main =
 

    Browser.element
        { init =
            \() ->
                ( init
                , Browser.Dom.focus "prompt"
                    |> Task.attempt (always NoOp)
                )
        , view =
            \model ->
                Element.layout
                    [ width fill
                    , height fill
                    ]
                    (view model)
        , update =
            \msg model ->
                ( update msg model
                , if msg == CommandSubmitted then
                    Browser.Dom.focus "prompt"
                        |> Task.attempt (always NoOp)

                  else
                    Cmd.none
                 )  
        , subscriptions = always Sub.none
        }