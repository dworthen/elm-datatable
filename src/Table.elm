module Table exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events as Events
import Json.Decode as Json
import Json.Encode as JSEncode
import String
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
    , canHide : (Bool, String)
    , canSort : (Bool, String)
    , canFilter : (Bool, String)
    , toMsg : State -> msg
    }


view : ViewConfig data msg -> State -> List data -> Html msg
view config state data =
    table [ class "datatable" ]
        [ tableHeadView config state
        , tableBodyView config state data
        ]


tableBodyView : ViewConfig data msg -> State -> List data -> Html msg
tableBodyView { columns } { hiddenColumns, sortBy, sortOrder, filters } data =
    let
        visibleColumns =
            removeHiddenColumns hiddenColumns columns

        rows =
            data
                |> filterRows filters columns
                |> sortRows sortBy sortOrder columns

        displayCell row col =
            td [] [ col.formatter row ]

        displayRow row =
            tr [] <| List.map (displayCell row) visibleColumns

        tableRows =
            rows
                |> List.map displayRow
    in
        tbody [] tableRows


onClose : String -> (State -> msg) -> State -> Attribute msg
onClose hiddenColumn toMsg state =
    let
        newColumns =
            state.hiddenColumns ++ [ hiddenColumn ]

        newState =
            { state | hiddenColumns = newColumns }
    in
        Events.on "click" <| Json.map toMsg <| Json.succeed newState


onFilter : String -> (State -> msg) -> State -> Attribute msg
onFilter colName toMsg state =
    let
        filterDict =
            Dict.fromList state.filters

        newFilters search =
            Dict.insert colName search filterDict
                |> Dict.toList

        newState search =
            { state | filters = (newFilters search) }

        newToMsg =
            toMsg << newState
    in
        Events.on "input" <| Json.map newToMsg Events.targetValue


onSort : String -> (State -> msg) -> State -> Attribute msg
onSort columnName toMsg state =
    let
        { sortBy, sortOrder } =
            state

        newSortOrder =
            if sortBy == columnName then
                case sortOrder of
                    Asc ->
                        Desc

                    Desc ->
                        Asc
            else
                Asc

        newState =
            { state | sortOrder = newSortOrder, sortBy = columnName }
    in
        Events.on "click" <| Json.map toMsg <| Json.succeed newState


tableHeadView : ViewConfig data msg -> State -> Html msg
tableHeadView { columns, canHide, canSort, canFilter, toMsg } state =
    let
        { hiddenColumns, sortBy, sortOrder } =
            state

        closeButton col =
            if fst canHide then
                span 
                    [ class "column-close"
                    , onClose col.name toMsg state
                    ]
                    [ i [ class "column-close-icon fa fa-close" ] [ ]
                    , span 
                        [ class "column-close-text"
                        , property "innerHTML" (JSEncode.string <| snd canHide) 
                        ] 
                        [ ]
                    ]
            else
                text ""

        sortButton col =
            let
                asc =
                    col.name == sortBy && sortOrder == Asc

                desc =
                    col.name == sortBy && sortOrder == Desc

                classes =
                    [ ( "column-sort-icon", True )
                    , ( "fa", True )
                    , ( "fa-sort-asc", asc )
                    , ( "fa-sort-desc", desc )
                    , ( "fa-sort", not (asc || desc) )
                    ]
            in
                if fst canSort then
                    span 
                        [ class "column-sort" 
                        , onSort col.name toMsg state
                        ]
                        [ i [ classList classes ] [ ]
                        , span 
                            [ class "column-sort-text"
                            , property "innerHTML" (JSEncode.string <| snd canSort)
                            ]
                            [ ]
                        ]
                else
                    text ""

        inputBox col =
            if fst canFilter then
                input 
                    [ class "column-filter"
                    , onFilter col.name toMsg state 
                    , placeholder (snd canFilter)
                    ] 
                    []
            else
                text ""

        toTh col =
            th []
                [ closeButton col
                , span [ class "column-title" ] [ text col.name ]
                , sortButton col
                , br [] []
                , inputBox col
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


sortRows : String -> SortOrder -> List (Column data msg) -> List data -> List data
sortRows sortBy sortOrder columns =
    let
        colToString =
            getColumnToString sortBy columns

        sort =
            List.sortBy colToString
    in
        case sortOrder of
            Asc ->
                sort

            Desc ->
                List.reverse << sort


columnsToDictionary : List (Column data msg) -> Dict.Dict String (Column data msg)
columnsToDictionary columns =
    columns
        |> List.map (\col -> ( col.name, col ))
        |> Dict.fromList


getColumnToString : String -> List (Column data msg) -> (data -> String)
getColumnToString name columns =
    let
        columnsDict =
            columnsToDictionary columns
    in
        case Dict.get name (columnsDict) of
            Nothing ->
                (\_ -> "")

            Just col ->
                col.toString


filterRows : List ( String, String ) -> List (Column data msg) -> List data -> List data
filterRows filters columns data =
    let
        passFilter row ( filterName, filterText ) =
            let
                toString =
                    getColumnToString filterName columns

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
