module Main exposing (main)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Browser


type alias Todo =
    { id: Int
    ,isDone : Bool
    ,text : String
    }

type alias Model = 
    { current: Int 
    ,text : String
    ,xs : List Todo
    }

init : Model
init = {current=0,text="",xs=[]}

type Event = AddTodo Todo | UpdateTodo Todo | RemoveTodo Todo | ChageStateInput Int String

update : Event -> Model -> Model
update msg state =
    case msg of
        AddTodo todo ->
            {state | current = state.current + 1, xs = todo :: state.xs }
        RemoveTodo todo ->
            {state | xs = state.xs |> List.filter (\y -> y.id /= todo.id) }
        UpdateTodo todo -> 
            {state | xs = state.xs |> List.map (\y -> if y.id == todo.id then {y | text = todo.text, isDone= todo.isDone} else y) }
        ChageStateInput id newI -> 
            {current = state.current, text = newI, xs = state.xs }
        

todoElement : Int -> Bool -> String -> Html.Html Event
todoElement id isDone text = Html.li [] [
                Html.input [Html.Attributes.type_ "checkbox", Html.Attributes.checked isDone, Html.Events.onCheck(\x -> UpdateTodo {id=id, isDone= x , text= text})] [ ],
                Html.a [Html.Attributes.style "text-decoration" (if isDone then "line-through" else "none")] [ Html.text text],
                Html.button [ Html.Events.onClick (RemoveTodo {id=id, isDone=isDone, text=text}) ] [Html.text "Remove"]
                ]

view : Model -> Html.Html Event
view model =
    Html.div[] [
        Html.input [ Html.Attributes.placeholder "Enter your todo",  Html.Attributes.value model.text, Html.Events.onInput (\x -> ChageStateInput model.current x) ] [],
        Html.button [ Html.Attributes.value "Add", Html.Events.onClick (AddTodo {id=model.current, isDone=False, text=model.text}) ] [ Html.text "Add"],
        Html.ul [] (model.xs |> List.map (\{id, isDone,text} -> todoElement id isDone text))
    ]
    
    
main =
  Browser.sandbox { init = init, update = update, view = view }
