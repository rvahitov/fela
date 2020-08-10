module Fela.Domain.Models.DictionaryModel

open Fela.Common

module Name =

    type T = string

    let value (t : T) = string t

    let ofString (s : string) : T =
        Assert.stringNotEmpty "s" s
        s

type Name = Name.T


module DisplayName =
    type T = string

    let value (t : T) = string t

    let ofString (s : string) : T =
        Assert.stringNotEmpty "s" s
        s

type DisplayName = DisplayName.T

module Item =
    type ItemKey = string

    type ItemValue = string

    type T =
        private { Key : ItemKey
                  Value : ItemValue }

    let key item = item.Key
    let value item = item.Value

    let Create (key : ItemKey, value : ItemValue) =
        Assert.stringNotEmpty "key" key
        Assert.stringNotEmpty "value" value
        { Key = key
          Value = value }

type Item = Item.T

module Dictionary =
    type T =
        { Name : Name
          DisplayName : DisplayName
          Items : Item [] }

    let create name displayName items =
        { Name = name
          DisplayName = displayName
          Items = items }

type Dictionary = Dictionary.T

type DictionaryEvent =
    | DisplayNameChanged of Name * DisplayName * System.DateTimeOffset
    | ItemsAdded of Name * Item list * System.DateTimeOffset
    | ItemsRemoved of Name * Item list * System.DateTimeOffset

open System.Threading.Tasks

type IDictionaryManager =
    abstract GetByName : Name -> Task<Dictionary option>
    abstract SetDisplayName : (Name * DisplayName) -> Task<IExecutionResult>
    abstract AddItems : (Name * Item list) -> Task<IExecutionResult>
    abstract RemoveItems : (Name * Item list) -> Task<IExecutionResult>

module Aggregates =
    open Akkling
    open Akkling.Persistence

    module State =
        type T =
            { displayName : DisplayName
              items : Map<Name, Item> }

        let initial (name : Name) =
            { displayName =
                  name
                  |> Name.value
                  |> DisplayName.ofString
              items = Map.empty }

        let setDisplayName displayName state = { state with displayName = displayName }
        let addItems items state =
            { state with items = items |> List.fold (fun m item -> m |> Map.add (Item.key item) item) state.items }

        let removeItems items state =
            { state with
                  items =
                      items
                      |> List.map Item.key
                      |> List.fold (fun m key -> m |> Map.remove key) state.items }

        let filterItemsForAdd items state =
            let filter item =
                let key = item |> Item.key in not <| Map.containsKey key state.items
            items |> List.filter filter

        let filterItemsForRemove items state =
            let filter item =
                let key = item |> Item.key in Map.containsKey key state.items
            items |> List.filter filter

    type Message =
        | SetDisplayName of Name * DisplayName
        | AddItems of Name * Item list
        | RemoveItems of Name * Item list
        | GetDictionary of Name
        | Event of DictionaryEvent

    let createAggregate name factory =
        let aggregate (ctx : Eventsourced<Message>) =
            let rec loop state =
                actor {
                    match! ctx.Receive() with
                    | Event e ->
                        if not <| ctx.IsRecovering() then
                            ctx.Sender() <! ExecutionResult.Success()
                            ctx.System.EventStream.Publish e
                        match e with
                        | DisplayNameChanged (_, dn, _) -> return! loop (state |> State.setDisplayName dn)
                        | ItemsAdded (_, items, _) -> return! loop (state |> State.addItems items)
                        | ItemsRemoved (_, items, _) -> return! loop (state |> State.removeItems items)
                    | SetDisplayName (n, dn) ->
                        if state.displayName <> dn then
                            return Persist(Event(DisplayNameChanged(n, dn, System.DateTimeOffset.Now)))
                        else
                            ctx.Sender() <! ExecutionResult.Success()
                            return! loop state
                    | AddItems (n, items) ->
                        match state |> State.filterItemsForAdd items with
                        | [] ->
                            ctx.Sender() <! ExecutionResult.Success()
                            return! loop state
                        | lst -> return Persist(Event(ItemsAdded(n, lst, System.DateTimeOffset.Now)))
                    | RemoveItems (n, items) ->
                        match state |> State.filterItemsForRemove items with
                        | [] ->
                            ctx.Sender() <! ExecutionResult.Success()
                            return! loop state
                        | lst -> return Persist(Event(ItemsRemoved(n, lst, System.DateTimeOffset.Now)))
                    | GetDictionary n ->
                        let items =
                            state.items
                            |> Map.toArray
                            |> Array.map snd
                        ctx.Sender() <! Some(Dictionary.create n state.displayName items)
                        return! loop state
                }
            loop (State.initial name)
        spawn factory (name |> Name.value) <| propsPersist aggregate
