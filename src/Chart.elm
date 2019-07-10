module Chart exposing
    ( hBar, vBar, pie
    , title, colours, colors, addValueToLabel, updateStyles, dimensions
    , toHtml
    )

{-| This module comprises tools to create and modify a model of the data, labels and styling, and then the function `toHtml` renders the model using one of the provided views.


# Chart constructors

@docs hBar, vBar, pie


# Customisers

@docs title, colours, colors, addValueToLabel, updateStyles, dimensions


# Rendering

@docs toHtml

-}

import Basics.Extra exposing (flip, uncurry)
import ChartModel exposing (..)
import Dict exposing (Dict, get, update)
import Html exposing (Html, div, h3, span, text)
import Html.Attributes exposing (style)
import List exposing (filter, foldl, indexedMap, length, map, map2, maximum)
import Svg exposing (Svg, circle, svg)
import Svg.Attributes exposing (cx, cy, height, preserveAspectRatio, r, stroke, strokeDasharray, strokeDashoffset, viewBox, width)



-- API


{-| The horizontal bar chart results in a set of bars, one above the other, of lengths in proportion to the value. A label with the data value is printed in each bar.

    hBar data
        |> title "My Chart"
        |> toHtml

-}
hBar : List ( Float, String ) -> Model
hBar data =
    chartInit data BarHorizontal
        |> normalise
        |> addValueToLabel
        |> updateStyles "chart-container"
            [ ( "display", "block" )
            , ( "font", "10px sans-serif" )
            , ( "color", "white" )
            ]
        |> updateStyles "chart-elements"
            [ ( "background-color", "steelblue" )
            , ( "padding", "3px" )
            , ( "margin", "1px" )
            , ( "text-align", "right" )
            ]
        |> updateStyles "legend-labels"
            [ ( "display", "block" )
            ]


{-| The vertical bar chart results in a set of bars of lengths in proportion to the value. A label is printed below each bar.

    vBar data
        |> title "My Chart"
        |> toHtml

-}
vBar : List ( Float, String ) -> Model
vBar data =
    chartInit data BarVertical
        |> normalise
        |> updateStyles "chart-container"
            [ ( "flex-direction", "column" )
            ]
        |> updateStyles "chart"
            [ ( "display", "flex" )
            , ( "justify-content", "center" )
            , ( "align-items", "flex-end" )
            , ( "height", "300px" )
            ]
        |> updateStyles "chart-elements"
            [ ( "background-color", "steelblue" )
            , ( "padding", "3px" )
            , ( "margin", "1px" )
            , ( "width", "30px" )
            ]
        |> updateStyles "legend"
            [ ( "align-self", "center" )
            , ( "height", "70px" )
            ]
        |> updateStyles "legend-labels"
            [ ( "width", "100px" )
            , ( "text-align", "right" )
            , ( "overflow", "hidden" )
            , ( "white-space", "nowrap" )
            , ( "text-overflow", "ellipsis" )
            ]


{-| The pie chart results in a circle cut into coloured segments of size proportional to the data value.

    pie data
        |> toHtml

-}
pie : List ( Float, String ) -> Model
pie data =
    chartInit data Pie
        |> toPercent
        |> updateStyles "chart-container"
            [ ( "justify-content", "center" )
            , ( "align-items", "center" )
            , ( "flex-wrap", "wrap" )
            ]
        |> updateStyles "chart"
            [ ( "height", "200px" )
            , ( "transform", "rotate(-90deg)" )
            , ( "background", "grey" )
            , ( "border-radius", "50%" )
            ]
        |> updateStyles "chart-elements"
            [ ( "fill-opacity", "0" )
            , ( "stroke-width", "32" )
            ]
        |> updateStyles "legend"
            [ ( "flex-direction", "column" )
            , ( "justify-content", "center" )
            , ( "padding-left", "15px" )
            , ( "flex-basis", "67%" )
            , ( "flex-grow", "2" )
            , ( "max-width", "100%" )
            ]
        |> updateStyles "legend-labels"
            [ ( "white-space", "nowrap" )
            , ( "overflow", "hidden" )
            , ( "text-overflow", "ellipsis" )
            ]



-- UPDATE


{-| title adds a title to the model.

    -- e.g. build a chart from scratch
    chartInit vs ls BarHorizontal
        |> title "This will be the title"
        |> toHtml

-}
title : String -> Model -> Model
title newTitle model =
    { model | title = newTitle }


{-| colours replaces the default colours. Bar charts use just one colour, which will be the head of the list provided.

    vChart data
        |> colours [ "steelblue" ]
        |> toHtml

    pie data
        |> colours [ "steelblue", "#96A65B", "#D9A679", "#593F27", "#A63D33" ]
        |> toHtml

-}
colours : List String -> Model -> Model
colours newColours model =
    case newColours of
        [] ->
            model

        c :: cs ->
            case model.chartType of
                Pie ->
                    { model | colours = c :: cs }

                otherwise ->
                    updateStyles "chart" [ ( "background-color", c ) ] model


{-| colors supports alternative spelling of colours
-}
colors : List String -> Model -> Model
colors =
    colours


{-| addValueToLabel adds the data value of each item to the data label. This is applied by default in hBar.

    vBar data
        |> addValueToLabel
        |> toHtml

-}
addValueToLabel : Model -> Model
addValueToLabel model =
    { model
        | items = map (\item -> { item | label = item.label ++ " " ++ String.fromFloat item.value }) model.items
    }


{-| updateStyles replaces styles for a specified part of the chart. Charts have the following div structure

    .container
        .title
        .chart-container
            .chart      (container for the bars or pie segments)
                .chart-elements
            .legend     (also for the label container in a vertical bar chart)
                .legend-labels

    vChart vs ls
        |> updateStyles "chart" [ ( "color", "black" ) ]
        |> toHtml

-}
updateStyles : String -> List Style -> Model -> Model
updateStyles selector lst model =
    { model
        | styles =
            -- update selector (Maybe.map <| \curr -> foldl changeStyles curr lst) model.styles }
            update selector (Maybe.map <| flip (foldl changeStyles) lst) model.styles
    }


{-| sets the width and height of a chart

    vChart vs ls
        |> dimensions 400 300
        |> toHtml

-}
dimensions : Int -> Int -> Model -> Model
dimensions width height model =
    { model | width = width, height = height }



-- NOT exported


normalise : Model -> Model
normalise model =
    case maximum (map .value model.items) of
        Nothing ->
            model

        Just maxD ->
            { model
                | items = map (\item -> { item | normValue = item.value / maxD * 100 }) model.items
            }


toPercent : Model -> Model
toPercent model =
    let
        tot =
            List.sum (map .value model.items)

        items =
            map (\item -> { item | normValue = item.value / tot * 100 }) model.items
    in
    { model | items = items }



-- removes existing style setting (if any) and inserts new one


changeStyles : Style -> List Style -> List Style
changeStyles ( attr, val ) styles =
    ( attr, val ) :: filter (\( t, _ ) -> t /= attr) styles



-- VIEW


{-| toHtml is called last, and causes the chart data to be rendered to html.

    hBar data
        |> toHtml

-}
toHtml : Model -> Html a
toHtml model =
    let
        get_ sel =
            Maybe.withDefault [] (get sel model.styles)
    in
    div (List.map (uncurry style) <| get_ "container")
        [ h3 (List.map (uncurry style) <| get_ "title") [ text model.title ]
        , div (List.map (uncurry style) <| get_ "chart-container") <|
            -- chart-elements, axis, legend-labels,...
            case model.chartType of
                BarHorizontal ->
                    viewBarHorizontal model

                BarVertical ->
                    viewBarVertical model

                Pie ->
                    viewPie model
        ]


viewBarHorizontal : Model -> List (Html a)
viewBarHorizontal model =
    let
        get_ sel =
            Maybe.withDefault [] (get sel model.styles)

        colour =
            Maybe.withDefault "steelblue" (List.head model.colours)

        elements =
            map
                (\{ normValue, label } ->
                    div
                        (style "width"
                            (String.fromFloat normValue
                                ++ "%"
                            )
                            :: style "background-color" colour
                            :: List.map (uncurry style) (get_ "chart-elements")
                        )
                        [ span (List.map (uncurry style) <| get_ "legend-labels")
                            [ text label ]
                        ]
                )
                model.items
    in
    elements



-- V E R T I C A L


viewBarVertical : Model -> List (Html a)
viewBarVertical model =
    let
        get_ sel =
            Maybe.withDefault [] (get sel model.styles)

        elements =
            map
                (\{ normValue } -> div (style "height" (String.fromFloat normValue ++ "%") :: List.map (uncurry style) (get_ "chart-elements")) [])
                model.items

        rotateLabel lenData idx =
            let
                labelWidth =
                    60

                offset =
                    case modBy lenData 2 == 0 of
                        True ->
                            (lenData // 2 - idx - 1) * labelWidth + 20

                        -- 6 chart-elements, 2&3 are the middle
                        False ->
                            (lenData // 2 - idx) * labelWidth - 10

                -- 5 chart-elements, 2 is the middle
            in
            style "transform" <| "translateX( " ++ String.fromInt offset ++ "px) translateY(30px) rotate(-45deg)"

        labels =
            indexedMap
                (\idx item ->
                    div
                        (rotateLabel (length model.items) idx
                            :: List.map (uncurry style) (get_ "legend-labels")
                        )
                        [ text (.label item) ]
                )
                model.items
    in
    [ div (List.map (uncurry style) <| get_ "chart") elements
    , div (List.map (uncurry style) <| get_ "legend") labels
    ]



-- P I E   V I E W


viewPie : Model -> List (Html a)
viewPie model =
    let
        -- elem : Float -> Float -> String -> Svg msg
        elem off ang col =
            circle
                [ r "25%"
                , cx "50%"
                , cy "50%"
                , stroke col
                , strokeDashoffset (String.fromFloat off)
                , strokeDasharray <| String.fromFloat ang ++ " 100"
                , style "fill" "none"
                , style "stroke-width" "32"
                ]
                []

        go val ( accOff, cols, accElems ) =
            case cols of
                c :: cs ->
                    let
                        ang =
                            val.normValue
                    in
                    ( accOff - ang
                    , if List.isEmpty cs then
                        model.colours

                      else
                        cs
                    , elem accOff ang c :: accElems
                    )

                [] ->
                    ( accOff, cols, accElems )

        ( _, _, elems ) =
            List.foldl go ( 0, model.colours, [] ) model.items

        legend items =
            List.map2
                (\{ label } col ->
                    div (List.map (uncurry style) <| get_ "legend-labels")
                        [ span
                            [ style "background-color" col
                            , style "display" "inline-block"
                            , style "height" "20px"
                            , style "width" "20px"
                            , style "margin-right" "5px"
                            ]
                            [ text " " ]
                        , Html.text label
                        ]
                )
                items
                model.colours

        get_ sel =
            get sel model.styles
                |> Maybe.withDefault []
    in
    [ Svg.svg
        -- chart
        (List.map (uncurry style) (get_ "chart")
            ++ [ viewBox "0 0 63.66198 63.66198"
               , preserveAspectRatio "xMidYMid slice"
               ]
        )
        elems
    , div
        -- legend
        (List.map (uncurry style) <| get_ "legend")
        (legend model.items)
    ]
