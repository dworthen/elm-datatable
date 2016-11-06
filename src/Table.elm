module Table exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events as Events
import Dict
import Regex


-- MODEL


type SortOrder
    = Asc
    | Desc


type alias State =
    { sortBy : String
    , sortOrder : SortOrder
    , filters : List ( String, String )
    , hiddenColumns : List String
    }


type alias Column data msg =
    { name : String
    , toString : data -> String
    , formatter : data -> Html msg
    }


initializeState : String -> State
initializeState sortBy =
    State sortBy Asc [] []


intColumn : String -> (data -> Int) -> Maybe (data -> Html msg) -> Column data msg
intColumn name toVal formatter =
    let
        toStr =
            toString << toVal

        toText =
            text << toStr

        htmlFormatter =
            Maybe.withDefault (\a -> toText a) formatter
    in
        Column name toStr htmlFormatter


floatColumn : String -> (data -> Float) -> Maybe (data -> Html msg) -> Column data msg
floatColumn name toVal formatter =
    let
        toStr =
            toString << toVal

        toText =
            text << toStr

        htmlFormatter =
            Maybe.withDefault (\a -> toText a) formatter
    in
        Column name toStr htmlFormatter


stringColumn : String -> (data -> String) -> Maybe (data -> Html msg) -> Column data msg
stringColumn name toVal formatter =
    let
        toText =
            text << toVal

        htmlFormatter =
            Maybe.withDefault (\a -> toText a) formatter
    in
        Column name toVal htmlFormatter



-- VIEW


type alias ViewConfig data msg =
    { columns : List (Column data msg)
    , canHide : Bool
    , canSort : Bool
    , canFilter : Bool
    , toMsg : State -> msg
    }


view : ViewConfig data msg -> State -> List data -> Html msg
view config state data =
    table []
        [ tableHeadView config state
        , tableBodyView config state data
        ]


tableBodyView : ViewConfig data msg -> State -> List data -> Html msg
tableBodyView { columns } { hiddenColumns, sortBy, sortOrder, filters } data =
    let
        visibleColumns =
            removeHiddenColumns hiddenColumns columns

        filteredRows =
            filterRows filters columns data

        rows =
            filteredRows
                |> List.map
                    (\row ->
                        tr []
                            (visibleColumns
                                |> List.map
                                    (\col ->
                                        td []
                                            [ col.formatter row ]
                                    )
                            )
                    )
    in
        tbody [] rows



-- Going to need state and viewConfig


tableHeadView : ViewConfig data msg -> State -> Html msg
tableHeadView { columns, canHide, canSort, canFilter } { hiddenColumns, sortBy, sortOrder } =
    let
        closeButton =
            if canHide then
                i [ class "fa fa-close" ] [ text "X" ]
            else
                text ""

        sortButton col =
            let
                asc =
                    col.name == sortBy && sortOrder == Asc

                desc =
                    col.name == sortBy && sortOrder == Desc

                classes =
                    [ ( "fa", True )
                    , ( "fa-sort-asc", asc )
                    , ( "fa-sort-desc", desc )
                    , ( "fa-sort", not (asc || desc) )
                    ]
            in
                if canSort then
                    i [ classList classes ] [ text "Sort" ]
                else
                    text ""

        inputBox =
            if canFilter then
                input [] []
            else
                text ""

        toTh col =
            th []
                [ closeButton
                , text col.name
                , sortButton col
                , br [] []
                , inputBox
                ]

        ths =
            columns
                |> removeHiddenColumns hiddenColumns
                |> List.map toTh
    in
        thead []
            [ tr [] ths ]


removeHiddenColumns : List String -> List (Column data msg) -> List (Column data msg)
removeHiddenColumns hiddenColumns columns =
    let
        showNotHiddenColumn col =
            hiddenColumns
                |> List.member col.name
                |> not
    in
        columns
            |> List.filter showNotHiddenColumn


filterRows : List ( String, String ) -> List (Column data msg) -> List data -> List data
filterRows filters columns data =
    let
        columnDict =
            columns
                |> List.map (\col -> ( col.name, col ))
                |> Dict.fromList

        getToString name =
            case Dict.get name columnDict of
                Nothing ->
                    (\_ -> "")

                Just col ->
                    col.toString

        passFilter row ( filterName, filterText ) =
            let
                toString =
                    getToString filterName

                rowValue =
                    toString row

                spaceRegEx =
                    Regex.regex " "

                regEx =
                    filterText
                        |> Regex.escape
                        |> Regex.replace Regex.All spaceRegEx (\_ -> ".*?")
                        |> Regex.regex
                        |> Regex.caseInsensitive
            in
                Regex.contains regEx rowValue
    in
        data
            |> List.filter (\row -> List.all (passFilter row) filters)
