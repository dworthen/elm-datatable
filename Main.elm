module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Table as Table
import String
import Dict exposing (Dict(..))
import Array


-- MAIN


main =
    App.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { name : String
    , tableState : Table.State
    , presidents : List Person
    , presidentsTuples : List (PersonTuples String Int)
    , presidentsList : List PersonList
    }


model : Model
model =
    let
        tableState = Table.State "Name" Table.Asc [] []
    in
        Model "Derek" tableState presidents presidentsTuples presidentsList


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = Something
    | UpdateTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        UpdateTableState newTableState ->
            ( { model | tableState = newTableState }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Table.view tableViewConfig model.tableState model.presidents
        , Table.view tableViewConfigTuples model.tableState model.presidentsTuples
        , Table.view tableViewConfigList model.tableState model.presidentsList 
        ]



-- PEOPLE


tableViewConfig : Table.ViewConfig Person Msg
tableViewConfig =
    { columns =
        [ Table.intColumn "Year" .year Nothing
        , Table.stringColumn "Name" .name <| Just (\c -> strong [] [ text c.name ])
        , Table.stringColumn "City" .city Nothing
        , Table.stringColumn "State" .state Nothing
        ]
    , canHide = (True, "Close Me")
    , canSort = (True, "Sort")
    , canFilter = (True, "Search")
    , toMsg = UpdateTableState
    }


type alias Person =
    { name : String
    , year : Int
    , city : String
    , state : String
    }


presidents : List Person
presidents =
    [ Person "George Washington" 1732 "Westmoreland County" "Virginia"
    , Person "John Adams" 1735 "Braintree" "Massachusetts"
    , Person "Thomas Jefferson" 1743 "Shadwell" "Virginia"
    , Person "James Madison" 1751 "Port Conway" "Virginia"
    , Person "James Monroe" 1758 "Monroe Hall" "Virginia"
    , Person "Andrew Jackson" 1767 "Waxhaws Region" "South/North Carolina"
    , Person "John Quincy Adams" 1767 "Braintree" "Massachusetts"
    , Person "William Henry Harrison" 1773 "Charles City County" "Virginia"
    , Person "Martin Van Buren" 1782 "Kinderhook" "New York"
    , Person "Zachary Taylor" 1784 "Barboursville" "Virginia"
    , Person "John Tyler" 1790 "Charles City County" "Virginia"
    , Person "James Buchanan" 1791 "Cove Gap" "Pennsylvania"
    , Person "James K. Polk" 1795 "Pineville" "North Carolina"
    , Person "Millard Fillmore" 1800 "Summerhill" "New York"
    , Person "Franklin Pierce" 1804 "Hillsborough" "New Hampshire"
    , Person "Andrew Johnson" 1808 "Raleigh" "North Carolina"
    , Person "Abraham Lincoln" 1809 "Sinking spring" "Kentucky"
    , Person "Ulysses S. Grant" 1822 "Point Pleasant" "Ohio"
    , Person "Rutherford B. Hayes" 1822 "Delaware" "Ohio"
    , Person "Chester A. Arthur" 1829 "Fairfield" "Vermont"
    , Person "James A. Garfield" 1831 "Moreland Hills" "Ohio"
    , Person "Benjamin Harrison" 1833 "North Bend" "Ohio"
    , Person "Grover Cleveland" 1837 "Caldwell" "New Jersey"
    , Person "William McKinley" 1843 "Niles" "Ohio"
    , Person "Woodrow Wilson" 1856 "Staunton" "Virginia"
    , Person "William Howard Taft" 1857 "Cincinnati" "Ohio"
    , Person "Theodore Roosevelt" 1858 "New York City" "New York"
    , Person "Warren G. Harding" 1865 "Blooming Grove" "Ohio"
    , Person "Calvin Coolidge" 1872 "Plymouth" "Vermont"
    , Person "Herbert Hoover" 1874 "West Branch" "Iowa"
    , Person "Franklin D. Roosevelt" 1882 "Hyde Park" "New York"
    , Person "Harry S. Truman" 1884 "Lamar" "Missouri"
    , Person "Dwight D. Eisenhower" 1890 "Denison" "Texas"
    , Person "Lyndon B. Johnson" 1908 "Stonewall" "Texas"
    , Person "Ronald Reagan" 1911 "Tampico" "Illinois"
    , Person "Richard M. Nixon" 1913 "Yorba Linda" "California"
    , Person "Gerald R. Ford" 1913 "Omaha" "Nebraska"
    , Person "John F. Kennedy" 1917 "Brookline" "Massachusetts"
    , Person "George H. W. Bush" 1924 "Milton" "Massachusetts"
    , Person "Jimmy Carter" 1924 "Plains" "Georgia"
    , Person "George W. Bush" 1946 "New Haven" "Connecticut"
    , Person "Bill Clinton" 1946 "Hope" "Arkansas"
    , Person "Barack Obama" 1961 "Honolulu" "Hawaii"
    ]


tableViewConfigTuples : Table.ViewConfig (PersonTuples String Int) Msg
tableViewConfigTuples =
    { columns =
        [ Table.intColumn "Year" (snd) Nothing
        , Table.stringColumn "Name" (fst) Nothing 
        ]
    , canHide = (True, "Close Me")
    , canSort = (True, "Sort")
    , canFilter = (True, "Search")
    , toMsg = UpdateTableState
    }


type alias PersonTuples a b =
    (a, b)


presidentsTuples : List (PersonTuples String Int)
presidentsTuples =
    [ ("George Washington", 1732)
    , ("John Adams", 1735)
    , ("Thomas Jefferson", 1743)
    , ("Ander Jackson", 1767)
    ]


tableViewConfigList : Table.ViewConfig PersonList Msg
tableViewConfigList =
    { columns =
        [ Table.stringColumn "Year" (getListItem 1) Nothing
        , Table.stringColumn "Name" (getListItem 0) <| Just (\c -> strong [] [ text <| getListItem 0 c ]) 
        ]
    , canHide = (True, "Close Me")
    , canSort = (True, "Sort")
    , canFilter = (True, "Search")
    , toMsg = UpdateTableState
    }


type alias PersonList =
    List String


getListItem : Int -> List String -> String
getListItem ind items =
    items
        |> Array.fromList
        |> Array.get ind
        |> Maybe.withDefault ""


presidentsList : List PersonList
presidentsList =
    [ ["George Washington", "1732"]
    , ["John Adams", "1735"]
    , ["Thomas Jefferson", "1743"]
    , ["Ander Jackson", "1767"]
    , ["William Henry Harrison", "1773"]
    ]
