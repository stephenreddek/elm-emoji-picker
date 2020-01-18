module EmojiPicker exposing
    ( Model, Msg(..)
    , PickerConfig, init, ViewConfig
    , view, update
    )

{-| This module provides a general-purpose emoji picker, with emojis
segregated by category. See [this file](https://github.com/Holmusk/elmoji/blob/master/examples/Main.elm)
for an example of how to use the picker in your application!


# Internals

@docs Model, Msg


# Config & Initialization

@docs PickerConfig, init, ViewConfig


# Functions to use in integration

@docs view, update

-}

import Dict exposing (Dict, get, isEmpty)
import Emojis
import Html exposing (Html, div, input, p, span, text)
import Html.Attributes exposing (class, hidden, style, title, value)
import Html.Events exposing (onInput, preventDefaultOn)
import Icons exposing (..)
import Json.Decode
import List.Extra
import String exposing (String)
import Types exposing (Category, Emoji)



----- MODEL -----


type alias SkinColor =
    String


{-| When initializing the emoji picker, you'll need to provide a few
configuration parameters.

`offsetX`: the horizontal offset from where the picker is declared
`offsetY`: the vertical offset from where the picker is declared
`closeOnSelect`: whether or not the close the picker after an emoji is selected
`frequentlyUsed`: A list of emojis to display in the "frequently used" section. They should be the native emojis (what gets output when selected).

-}
type alias PickerConfig =
    { offsetX : Float
    , offsetY : Float
    , closeOnSelect : Bool
    , frequentlyUsed : List String
    }


{-| When viewing the emoji picker, you'll need to provide these parameters.

`emojiDisplay`: A function use to display the emojis differently than native

-}
type alias ViewConfig msg =
    { emojiDisplay : Maybe (String -> Html msg) }


{-| The internal state of the emoji picker.

Note: the `skinColor` field is not in use in the current version,
but a future release may include a skin tone selector to switch between
emoji variants.

-}
type alias Model =
    { skinColor : SkinColor -- for future use (some emojis have variants)
    , activeTab : Tab -- for displaying emojis
    , hidden : Bool -- for toggling the emoji picker
    , offsetX : Float -- control the x-positon of the picker
    , offsetY : Float -- control the y-positon of the picker
    , closeOnSelect : Bool -- whether or not to close after an emoji is picked
    , frequentlyUsed : List Emoji
    }


type Tab
    = FrequentlyUsed
    | SearchResults String
    | ViewCategory Category


{-| This is the function to call to initialize the emoji picker.

    pickerConfig : PickerConfig
    pickerConfig =
        { offsetX = -271
        , offsetY = -410
        , closeOnSelect = True
        }

    emojiModel : EmojiPicker.Model
    emojiModel =
        EmojiPicker.init pickerConfig

-}
init : PickerConfig -> Model
init config =
    let
        frequentlyUsed =
            --TODO: how can we make this efficient?
            List.filterMap
                (\used ->
                    List.Extra.find (\( _, { native } ) -> native == used) Emojis.emojiList
                        |> Maybe.map Tuple.second
                )
                config.frequentlyUsed

        initialTab =
            if List.isEmpty frequentlyUsed then
                ViewCategory (Tuple.first Icons.people)

            else
                FrequentlyUsed
    in
    { skinColor = "none"
    , activeTab = initialTab
    , hidden = True
    , closeOnSelect = config.closeOnSelect
    , offsetX = config.offsetX
    , offsetY = config.offsetY
    , frequentlyUsed = frequentlyUsed
    }



---- UPDATE ----


{-| Most of the messages are handled internally, but there are a couple that will
be of interest to the parent module:

`Toggle`: Use this message in the parent to toggle the emoji picker on and off
`Select`: Catch this message in the parent to retrieve the selected emoji

-}
type Msg
    = NoOp
    | Toggle
    | ChooseSkinColor SkinColor
    | SelectCategory Category
    | SelectFrequentlyUsed
    | Select String
    | SearchUpdated String


{-| You'll need to catch the `Select` message in your parent module to
obtain the emoji from the picker. However, don't forget to propagate the
messages down to this function, because some internal states will probably
need to be updated.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Toggle ->
            ( { model | hidden = not model.hidden }, Cmd.none )

        -- currently this case is never called, but in the future we might
        -- add in a skin color selector for users to choose different emoji
        -- variants.
        ChooseSkinColor s ->
            ( { model | skinColor = s }, Cmd.none )

        SelectCategory selected ->
            ( { model | activeTab = ViewCategory selected }, Cmd.none )

        SelectFrequentlyUsed ->
            ( { model | activeTab = FrequentlyUsed }, Cmd.none )

        -- catch this in the parent. 's' is the emoji.
        Select s ->
            let
                newModel =
                    if model.closeOnSelect then
                        { model | hidden = not model.hidden }

                    else
                        model
            in
            ( newModel, Cmd.none )

        SearchUpdated search ->
            ( { model | activeTab = SearchResults search }, Cmd.none )



---- VIEW ----
{- used to get a skin variation of an emoji (if it exists).

   INPUTS
       color : emoji skin tone
       emoji : the emoji object
-}


selectSkinVariation : SkinColor -> Emoji -> String
selectSkinVariation color emoji =
    let
        isDictEmpty =
            isEmpty emoji.skinVariations
    in
    case ( color, isDictEmpty ) of
        ( "none", _ ) ->
            -- if there is no specified skin tone,
            emoji.native

        -- return the yellow version of the emoji
        ( skin, False ) ->
            -- if there is a specified skin tone, try to get it
            Maybe.withDefault emoji.native <| get skin emoji.skinVariations

        _ ->
            -- if the dict is empty, then no variations are available
            emoji.native



{- used to display a single emoji. it wraps the literal emoji string in a
   <span class="emoji"> so it can be styled.

   INPUTS
       color : emoji skin tone (from model.skinColor)
       emoji : the emoji to be displayed
-}


displayEmoji : ViewConfig Msg -> SkinColor -> Emoji -> Html Msg
displayEmoji config color emoji =
    let
        -- "native" is the literal emoji string
        native =
            selectSkinVariation color emoji

        display =
            config.emojiDisplay
                |> Maybe.map (\f -> f native)
                |> Maybe.withDefault (text native)
    in
    span
        [ class "emoji"
        , preventDefaultOn "click" (Json.Decode.succeed ( Select native, True ))
        , title emoji.name
        ]
        [ display ]



{- used to display emojis in a category.

   INPUTS
       version   : the browser's most recent supported version of emojis
                   this function filters out emojis that are unsupported,
                   since they will just display as boxes anyway.
       names     : from category.emojis. this function takes a list of
                   emoji names and gets them from the emoji dict.
       emojiDict : the place where all the emojis are stored. from the
                   Emojis module.
-}


getEmojisFromList : Int -> List String -> Dict String Emoji -> List Emoji
getEmojisFromList version names emojiDict =
    List.filterMap (\name -> get name emojiDict) names
        |> List.filter (\emoji -> emoji.version < version)



{- used to display an entire category.

   INPUTS
       pickerId  : from model.id (see `categoryMapping`)
       version   : the browser's most recent supported version of emojis
                   (see `getEmojisFromList`)
       emojiDict : the place where all the emojis are stored. from the
                   Emojis module.
       color     : from model.skinColor (see `displayEmoji`)
       cat       : category to be displayed
-}


displayCategory : ViewConfig Msg -> Int -> Dict String Emoji -> SkinColor -> Category -> Html Msg
displayCategory config version emojiDict color cat =
    let
        -- get the emojis from cat.emojis
        catEmojis =
            getEmojisFromList version cat.emojis emojiDict

        -- render them all
        renderedEmojis =
            List.map (displayEmoji config color) catEmojis
    in
    div [ class "category" ]
        ([ p [ class "category-title" ]
            [ text cat.name ]
         ]
            ++ renderedEmojis
        )



{- used to display a category icon at the bottom of the emoji picker

   INPUTS
       activeCat   : the active category (used to color the active icon blue)
       (cat, icon) : a tuple of (category, icon) from the Icons.elm file
-}


activeClass : String
activeClass =
    "path-active"


inactiveClass : String
inactiveClass =
    "path-inactive"


displayCategoryIcon : Tab -> ( Category, String -> Html Msg ) -> Html Msg
displayCategoryIcon activeTab ( cat, icon ) =
    let
        selectedCategory =
            case activeTab of
                ViewCategory category ->
                    Just category

                _ ->
                    Nothing

        updatedIcon =
            if Maybe.map .name selectedCategory == Just cat.name then
                icon activeClass

            else
                icon inactiveClass
    in
    span [ preventDefaultOn "click" (Json.Decode.succeed ( SelectCategory cat, True )) ]
        [ updatedIcon ]


{-| Use this function to instantiate the actual `Html msg` for the picker.
-}
view : ViewConfig Msg -> Model -> Html.Html Msg
view config model =
    let
        -- i've set the version to 10 here, since that seems to be the version that
        -- is most widely supported. however, in the ideal case, you'd set this
        -- dynamically based on what emoji version the user's browser supports.
        emojiVersion =
            10

        emojis =
            case model.activeTab of
                FrequentlyUsed ->
                    div [ class "category" ]
                        (p [ class "category-title" ]
                            [ text "Frequently Used" ]
                            :: List.map (displayEmoji config model.skinColor) model.frequentlyUsed
                        )

                SearchResults search ->
                    div [ class "category" ]
                        (p [ class "category-title" ]
                            [ text "Search Results" ]
                            :: List.filterMap
                                (\( emojiName, emoji ) ->
                                    if String.contains search emojiName then
                                        Just (displayEmoji config model.skinColor emoji)

                                    else
                                        Nothing
                                )
                                Emojis.emojiList
                        )

                ViewCategory category ->
                    displayCategory config
                        emojiVersion
                        Emojis.emojiDict
                        model.skinColor
                        category

        searchValue =
            case model.activeTab of
                SearchResults search ->
                    search

                _ ->
                    ""

        mainPanel =
            [ div [ class "icon-panel" ] icons
            , div [ class "search-bar" ]
                [ input [ value searchValue, onInput SearchUpdated ] []
                ]
            , div [ class "emojis-main" ]
                [ emojis ]
            ]

        frequentlyUsedIcon =
            span [ preventDefaultOn "click" (Json.Decode.succeed ( SelectFrequentlyUsed, True )) ]
                [ if model.activeTab == FrequentlyUsed then
                    Icons.frequentlyUsed activeClass

                  else
                    Icons.frequentlyUsed inactiveClass
                ]

        icons =
            frequentlyUsedIcon :: List.map (displayCategoryIcon model.activeTab) iconList
    in
    div [ class "elm-emoji-picker", hidden model.hidden ]
        [ div
            [ style "top" (String.fromFloat model.offsetY ++ "px")
            , style "left" (String.fromFloat model.offsetX ++ "px")
            , style "position" "absolute"
            , hidden model.hidden
            , class "emoji-picker"
            ]
            mainPanel
        , div
            [ class "emoji-modal-background"
            , preventDefaultOn "click" (Json.Decode.succeed ( Toggle, True ))
            ]
            []
        ]
