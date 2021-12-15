module Main exposing (..)

{-| This is a skeleton for an interpreter application. For now it basically simply display what you type in.
You should just:

  - insert your code in the update and the viewTurtlePath functions,
  - surely add some field in the Model type.
    You can of course add other types,
    functions and modules but you shouldn't have to modify the code at other places -- if you think you have to modify
    this code, reach your teacher out before doing this.

-}

import Basics exposing (..)
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
import Html exposing (option)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (string)
import List
import List.Extra
import Maybe exposing (Maybe(..))
import Result exposing (Result(..))
import String exposing (String)
import Task


type alias Model =
    { commandInput : String
    , history : List String
    }

{-|  option de dessin donc:

        -penDown = Savoir si l'on dessine ou pas
        -color = Couleur du dessin
        -Angle = Angle du trait (tortue)
        -Position = position du point de dessin

-}

type alias DrawOptions =
    { penDown : Bool
    , color : Color.Color
    , angle : Float
    , position : ( Float, Float )
    }


type Msg
    = CommandEntered String
    | CommandSubmitted
    | NoOp



init : Model
init =
    { commandInput = "", history = [] }


lastElem : List a -> Maybe a
lastElem =
    List.foldl (Just >> always) Nothing


update : Msg -> Model -> Model
update msg model =
    case msg of
        CommandEntered command ->
            Debug.log command
                { model | commandInput = command }

        CommandSubmitted ->
            --Si la la commande entré est Clean  alors tout l'écan sera effacé
            if model.commandInput == "clean" then
                { model
                    | commandInput = ""
                    , history =
                    -- ajoute une commande "pen_up" au début de l'historique et une commande "pen_down" à la fin de l'historique. 
                    -- Il supprime également toutes les commandes "pen_up" ou "pen_down" car elles n'ont plus d'importance. Par conséquent, toutes les commandes d'historique sont exécutées sans dessin afin de nettoyer l'écran de toutes les lignes
                        "pen_up" :: List.filter (\el -> el /= "pen_down" && el /= "pen_up") model.history ++ [ "pen_down" ]
                }

            else
                { model | commandInput = "", history = model.history ++ [ model.commandInput ] }

        NoOp ->
            model


view : Model -> Element Msg
view model =
    column [ width fill, height fill ]
        [ el [ width fill, height fill ]
            (el [ Border.width 2, padding 2, centerX, centerY, Background.color (Element.rgb255 52 101 164) ]
                (Element.html (Collage.Render.svgBox ( 800, 800 ) (viewTurtlePath model)))
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

-- Convertir une Maybe Strign en Float
stringToFloat : Maybe String -> Float
stringToFloat str =
    case str of
        Just number ->
            case String.toFloat number of
                Just n ->
                    n
                
                Nothing ->
                    0

        Nothing ->
            0

-- Convertie la Maybe String en Color
parseColor : Maybe String -> Color.Color
parseColor str =
    case str of
        Just color ->
            --Change la couleur du trait selon la string Entré
            if color == "red" then
                Color.red

            else if color == "black" then
                Color.black

            else if color == "blue" then
                Color.blue

            else if color == "green" then
                Color.green

            else
                Color.black
        Nothing ->
            Color.black


newPosition : ( Float, Float ) -> Float -> List String -> ( Float, Float )
newPosition ( x, y ) angle commandParam =
    let
        length =
            --Prends la derniere string de la list et la converti en FLoat
            stringToFloat (List.Extra.last commandParam)
    in
    ( (cos angle * length) + x, (sin angle * length) + y )
    -- Créer une nouvelle position de la tortue Sellon son angle et sa position précedente

-- Prends en parametre la position actuelle (x,y) la nouvelle position (newX,neY) la couleur du stylo (Color) et l'état du stylo (pen_down or Up)
moveForward : ( Float, Float ) -> ( Float, Float ) -> Color.Color -> Bool -> Collage Msg 
moveForward ( x, y ) ( newX, newY ) color penDown =
    
    Collage.segment ( x, y ) ( newX, newY )
        |> (if penDown then
                Collage.traced (Collage.solid 1 (Collage.uniform color))
                --Si il est down alors il dessine derriere le deplacement de la tortue

            else
                --Sinon le trait est invisible
                Collage.traced Collage.invisible
           )


drawPath : List String -> DrawOptions -> List (Collage Msg)
drawPath history options =
    case history of
        x :: xs ->
            case String.words x of
                command ->
                    case List.head command of
                        -- Si la premiere string de la list est Forward
                        Just "forward" ->
                            --Appelle la fonction MoveForward
                            moveForward options.position (newPosition options.position options.angle command) options.color options.penDown
                                :: drawPath xs { options | position = newPosition options.position options.angle command } -- Nouvelle position de a tortue mis a jour

                        -- Si la premiere string de la list est turn_left
                        Just "turn_left" ->
                            -- Permets de modifié en degré (degrees) l'angle de la tortue vers la gauche
                            drawPath xs { options | angle =  degrees (stringToFloat (List.Extra.last command)) + options.angle }

                        -- Si la premiere string de la list est turn_right
                        Just "turn_right" ->
                            -- Permets de modifié en degré (degrees) l'angle de la tortue vers la droite *-1

                            drawPath xs { options | angle = degrees (stringToFloat (List.Extra.last command)) * -1 + options.angle }

                        -- Si la premiere string de la list est color
                        Just "color" ->
                            --Passe la valeur de de la derniere string de al list soit ("Red" ou "green" par exemple) dans Parse Color
                            -- Et passe la valeur dans la variable color
                            drawPath xs { options | color = parseColor (List.Extra.last command) }

                        -- Si la premiere string de la list est pen_down
                        Just "pen_down" ->
                            --Passe la valeur penDown a True 
                            drawPath xs { options | penDown = True }

                        -- Si la premiere string de la list est pen_up
                        Just "pen_up" ->
                            --Passe la valeur penDown a False
                            drawPath xs { options | penDown = False }

                        _ ->
                            drawPath xs options

        [] ->
            --Colle la tortue eavec une dimension 60,60 à la position et l'angle initial
            [ Collage.image ( 60, 60 ) "https://elm-lang.org/images/turtle.gif"
                |> Collage.shift options.position
                |> Collage.rotate options.angle
            ]


viewTurtlePath : Model -> Collage Msg
viewTurtlePath { history } =
    -- Element de base lors du lancement du projet s'incrémente en fonction des commandes entrés (history)
    Collage.group (drawPath history { penDown = True, angle = degrees 90, color = Color.black, position = ( 0, 0 ) })


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


main : Program () Model Msg
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
