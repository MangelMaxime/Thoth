# FormBuilder

<article class="message is-warning">
  <div class="message-body">

This library is marked in **Alpha** stage but is **already used in production**.

I released it in **Alpha** so we can work as a community on improving it and still be able to introduce changes if needed.

  </div>
</article>

## Introduction

When working with forms in an Elmish application, we end up writing a lot of lines. I explained the situation in [my keynote at FableConf 2018](https://www.youtube.com/watch?v=Ry4qQxU0380).

The conclusion was: that to [manage a basic form](https://slides.com/mangelmaxime/fableconf_2018_keynote/live#/2/1) we need to write at least **23 lines of code per field** and have a lot of **duplication**.

This library is trying to solve that problem.

## Demo

<div class="columns">
    <div class="column is-8 is-offset-2">
        <div id="form_demo"></div>
    </div>
</div>

<div class="has-text-centered">

*[View the code](https://github.com/MangelMaxime/Thoth/blob/master/demos/Thoth.Elmish.Demo/src/FormBuilder.fs)*
</div>

<script type="text/javascript" src="../demos/vendors.js"></script>
<script type="text/javascript" src="../demos/demo.js"></script>
<script type="text/javascript">
    Demos.FormBuilder("form_demo");
</script>

## How to use ?

### Installation

Add the `Thoth.Elmish.FormBuilder` dependency in your [Paket](https://fsprojects.github.io/Paket/) files: `paket add Thoth.Elmish.FormBuilder --project <your project>`.

If you are trying this library for the first time you probably want to add `Thoth.Elmish.FormBuilder.BasicFields` too. It provides some ready to use fields.

In order to use the default view of `Thoth.Elmish.FormBuilder.BasicFields`, you need to include [Bulma](http://bulma.io/) in your project.

### BasicFields - Basic usage

<div class="message is-info">
<div class="message-header">
Information
</div>
<div class="message-body">

In this part, we are going to use `Thoth.Elmish.FormBuilder.BasicFields` in order to have ready to use fields.

Later, we will learn how to build custom fields.
</div>
</div>

---------------

1. Open the library modules

```fsharp
open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
```

---------------

2. Register the message dedicated to the `FormBuilder`

```fsharp
type Msg =
    | OnFormMsg of FormBuilder.Types.Msg
    // ...
```

---------------

3. Store the `FormBuilder` instance in your model

```fsharp
type Model =
    { FormState : FormBuilder.Types.State
      // ...
    }
```

---------------

4. Create your form using the builder API

```fsharp
let (formState, formConfig) =
    Form<Msg>
        .Create(OnFormMsg)
        .AddField(
            BasicInput
                .Create("name")
                .WithLabel("Name")
                .IsRequired()
                .WithDefaultView()
        )
        .AddField(
            BasicSelect
                .Create("favLang")
                .WithLabel("Favorite language")
                .WithValuesFromServer(getLanguages)
                .WithPlaceholder("")
                .IsRequired("I know it's hard but you need to choose")
                .WithDefaultView()
        )
        // When you are done with adding fields, you need to call `.Build()`
        .Build()
```

<div class="message is-warning">
<div class="message-body">

Each field needs to have a unique `name`. The name is used to link the `label` with its form elements. And it will also be used as the key for the JSON.

If you don't set a unique `name` per field, you will see this message in the console:

```fsharp
Each field needs to have a unique name. I found the following duplicate name:

- name
- description
```

</div>
</div>

---------------


5. Initialize the `FormBuilder` in your init function

<div class="message is-warning">
<div class="message-body">
<span>
<span class="icon has-text-warning is-medium"><i class="fa fa-lg fa-warning"></i></span>Never store <code>formConfig</code> in your model
<span>
</div>
</div>

```fsharp
let private init _ =
    let (formState, formCmds) = Form.init formConfig formState
    { FormState = formState }, Cmd.map OnFormMsg formCmds
```

---------------

6. Handle `OnFormMsg` in your update function

```fsharp
let private update msg model =
    match msg with
    | OnFormMsg msg ->
        let (formState, formCmd) = Form.update formConfig msg model.FormState
        { model with FormState = formState }, Cmd.map OnFormMsg formCmd
    // ...
```

---------------

7. Render your form in your view function

```fsharp
let private formActions (formState : FormBuilder.Types.State) dispatch =
    div [ ]
        [ button [ OnClick (fun _ ->
                    dispatch Submit
                   ) ]
            [ str "Submit" ] ]

let private view model dispatch =
    Form.render
        { Config = formConfig
          State = model.FormState
          Dispatch = dispatch
          ActionsArea = (formActions model.FormState dispatch)
          Loader = Form.DefaultLoader }
```

### BasicFields - Custom views

If you are not using [Bulma](http://bulma.io/) in your project, `Thoth.Elmish.FormBuilder.BasicFields` provides a `WithCustomView` API allowing you to customize the field view.

Example:

```fsharp
.AddField(
    BasicInput
        .Create("name")
        .WithLabel("Name")
        .IsRequired()
        .WithCustomView(fun (state : Types.FieldState) (dispatch : Types.IFieldMsg -> unit) ->
            let state : Input.State = state :?> Input.State

            // You can write your view here
            input [ Value  state.Value
                    OnChange (fun ev -> ev.Value |> Input.ChangeValue |> dispatch ) ]
        )
)
```

### Server side validation

In order to support server side validation, the library defines the type `ErrorDef`.

```fsharp
type ErrorDef =
    { Text : string
      Key : string }
```

- `Text` is the error message to display
- `Key` is the name of the field related to the error.

<article class="message is-info">
  <div class="message-body">

I included the `Decoder` and `Encoder` definitions for use in your Fable client.

If you need it on the server, you will need to copy the type definition for now.

  </div>
</article>

When you receive a `ErrorDef list` from your server, you can call `Form.setErrors` to display them in the form.

Example:

```fsharp
| CreationResponse.Errors errors ->
    let newFormState =
        model.State
        |> Form.setLoading false
        |> Form.setErrors formConfig errors
    { model with State = newFormState }, Cmd.none
```

### Create a custom field

#### Prelude

In this section, you will learn:

- how to create a custom fields
- the convention I use when designing a field, I encourage you to follow them 😊
- general comments on why I structure my code in a specific way

You will see usage of boxing `box` and casting `:?>`. If you want to learn more about that after reading this section you can take a look at the [F.A.Q.](#can-we-avoid-boxing-casting)

#### File structure

When designing a field I encourage you to follow this structure:

```fsharp
namespace MyCustomFieldLibrary

[<RequireQualifiedAccess>]
module MyField =

    // Here goes the logic for your field

type MyField private (state : MyField.State) =
    // Here goes the Fluent API that will be exposed and used to register a field in a Form

    static member Create(name : string) =
        // ...
```

By using this architecture, you can then use your API like this:

```fsharp
module MyApp.PageA

open Thoth.Elmish.FormBuilder
open MyCustomFieldLibrary

let formState, formConfig =
    Form<Msg>
    .Create(OnFormMsg)
        .AddField(
            MyField
                .Create("name")
                // ...
        )
```

The benefits are:
- Each field consists of a **single file**
- By using **1 open statement** you get access to all your fields API

*This is the structure used in [Thoth.Elmish.FormBuilder.BasicFields](https://github.com/MangelMaxime/Thoth/tree/master/src/Thoth.Elmish.FormBuilder.BasicFields)*

#### Implement your field logic and config contract

Designing a custom field is similar to designing an Elmish component.

Here is the contract that all fields need to implement. Don't worry, we are going to go step by step.

```fsharp
/// Contract for registering fields in the `Config`
type FieldConfig =
    { View : FieldState -> (IFieldMsg -> unit) -> React.ReactElement
      Update : FieldMsg -> FieldState -> FieldState * (string -> Cmd<Msg>)
      Init : FieldState -> FieldState * (string -> Cmd<Msg>)
      Validate : FieldState -> FieldState
      IsValid : FieldState -> bool
      ToJson : FieldState -> string * Encode.Value
      SetError : FieldState -> string -> FieldState }
```

As an example of a custom field, we will re-implement a basic `Input`.

---------------

1. `State` and `Validator` types

*`State` is similar to `Model` in Elmish terms*

**Every** field **needs to have** a `Name` property. This will be used later to identify each field uniquely and to generate the JSON representation of the field.

```fsharp
type State =
    { Label : string
      Value : string
      Type : string
      Placeholder : string option
      Validators : Validator list
      ValidationState : ValidationState
      Name : string }

and Validator = State -> ValidationState
```

---------------

2. `Msg` type

As in Elmish, your fields are going to react to `Msg`. But you need to interface with `IFieldMsg`.

```fsharp
type Msg =
    | ChangeValue of string
    interface IFieldMsg
```

---------------

3. `init` function

This function will be called when initializing your forms.

For example, if your field needs to fetch data from the server you can trigger the request here. [See the select field for an example](https://github.com/MangelMaxime/Thoth/blob/master/src/Thoth.Elmish.FormBuilder.BasicFields/Select.fs)

```fsharp
let private init (state : FieldState) =
    state, FormCmd.none
```

---------------

4. `validate` and `setError` function

If you used the same names for `ValidationState` and `Validators` properties, you can copy/paste these functions in all your field definitions.

*I didn't find a way to make it generic for any field*

```fsharp
let private validate (state : FieldState) =
    let state : State = state :?> State
    let rec applyValidators (validators : Validator list) (state : State) =
        match validators with
            | validator::rest ->
                match validator state with
                | Valid -> applyValidators rest state
                | Invalid msg ->
                    { state with ValidationState = Invalid msg }
            | [] -> state

    applyValidators state.Validators { state with ValidationState = Valid } |> box

let private setError (state : FieldState) (message : string)=
    let state : State = state :?> State
    { state with ValidationState = Invalid message } |> box
```

---------------

5. `isValid` function

This function will be called to check if your field is in a valid state or not.

```fsharp
let private isValid (state : FieldState) =
    let state : State = state :?> State
    state.ValidationState = Valid
```

---------------

6. `toJson` function

This function will be called by the form in order to generate the JSON representation of your field.

```fsharp
let private toJson (state : FieldState) =
    let state : State = state :?> State
    state.Name, Encode.string state.Value
```

---------------

7. `update` function

Similar to Elmish, this is called for updating your `State` when receiving a `Msg` for this field.

```fsharp
let private update (msg : FieldMsg) (state : FieldState) =
    // Cast the received message into it's real type
    let msg = msg :?> Msg
    // Cast the received state into it's real type
    let state = state :?> State

    match msg with
    | ChangeValue newValue ->
        { state with Value = newValue }
        |> validate
        // We need to box the returned state
        |> box, FormCmd.none
```

**Notes**

- You need to call `validate` youself after updating your model. This is required because not every field message needs to trigger a validation.
- Instead of using the `Cmd` module from Elmish, you needs to use `FormCmd`. This module implements the same API as the `Cmd` module.

---------------

8. `view` function

```fsharp
let private view (state : FieldState) (dispatch : IFieldMsg -> unit) =
    let state : State = state :?> State
    let className =
        if isValid state then
            "input"
        else
            "input is-danger"

    div [ Class "field" ]
        [ label [ Class "label"
                  HtmlFor state.Name ]
            [ str state.Label ]
          div [ Class "control" ]
            [ input [ Value state.Value
                      Placeholder (state.Placeholder |> Option.defaultValue "")
                      Id state.Name
                      Class className
                      OnChange (fun ev ->
                        ChangeValue ev.Value |> dispatch
                      ) ] ]
          span [ Class "help is-danger" ]
            [ str state.ValidationState.Text ] ]
```

---------------

9. Expose your `config`

```fsharp
let config : FieldConfig =
    { View = view
      Update = update
      Init = init
      Validate = validate
      IsValid = isValid
      ToJson = toJson
      SetError = setError }
```

#### Expose a fluent API

See the [F.A.Q.](#why-use-a-fluent-api) for why I chose to expose a fluent API.

1. In order to design an immutable fluent API, you need to mark your `constructor` as `private`.

```fsharp
type BasicInput private (state : Input.State) =
```

---------------

2. Expose a `static member Create(name : string)`

<span>
<span class="icon has-text-info"><i class="fa fa-lg fa-info"></i></span>Each field should have a <code>name</code> property as recommended in HTML5. This name will be used to identify the field for dispatching the messages in your form.
<span>

```fsharp
static member Create(name : string) =
    BasicInput
        { Label = ""
          Value = ""
          Type = "text"
          Placeholder = None
          Validators = [ ]
          ValidationState = Valid
          Name = name }
```

---------------

3. Create a member to return a `FieldBuilder`

```fsharp
member __.WithDefaultView () : FieldBuilder =
    { Type = "basic-input"
      State = state
      Name = state.Name
      Config = Input.config }
```

**Notes**

- The `Type` value needs to be a unique name to identify your field type. For example, `basic-input`, `fulma-input`, `my-lib-special-dropdown`, etc.
- The `Config` properties refer to the exposed config you wrote earlier.

---------------

4. Create on member per property you want to customize

Here are some examples:

```fsharp
member __.WithLabel (label : string) =
    BasicInput { state with Label = label }

member __.WithPlaceholder (placeholder : string) =
    BasicInput { state with Placeholder = Some placeholder }

member __.IsRequired (?msg : String) =
    let msg = defaultArg msg "This field is required"

    let validator (state : Input.State) =
        if String.IsNullOrWhiteSpace state.Value then
            Invalid msg
        else
            Valid

    BasicInput { state with Validators = state.Validators @ [ validator ] }

member __.AddValidator (validator) =
    BasicInput { state with Validators = state.Validators @ [ validator ] }
```


<article class="message is-success has-text-centered">
<div class="message-header">
🎉 Congrats 🎉
</div>
<div class="message-body">

You now have a **working field** with a **flexible API** exposed

</div>
</article>

## API

### FormBuilder.Types

| Types | Description |
|---|---|
| `ErrorDef` | Error representation to support server side validation |
| `ValidationState` | Used to describe if a field is `Valid` or `Invalid` with the message to display |
| `IFieldMsg` | Interface to be implemented by any field `Msg` |
| `FieldState` | Type alias for the field `State`, should be casted |
| `FieldMsg` | Type alias for the field `Msg`, should be casted |
| `Field` | Record to register a field in a `Form` instance |
| `Msg` | Internal `Msg` used by the Form library |
| `State` | Track current state of the Form |
| `FieldConfig` | Contract for registering fields in the `Config` |
| `Config` | Configuration for the Form |

### FormBuilder.Form

| Types | Description |
|---|---|
| `Form.init` | `init` function to call from your `init` to initialize the form |
| `Form.update` | `update` function to call when you received a message for the form |
| `Form.render` | Render the form in your view |
| `Form.valide` | Validate the model and check if it's valid |
| `Form.toJson` | Generate a JSON representation from the current state |
| `Form.setLoading` | Set the loading state of the form |
| `Form.isLoading` | Check if the form is loading |
| `Form.setErrors` | Set error for each field based on a `ErrorDef list` |

### FormBuilder.FormCmd

| Types | Description |
|---|---|
| `FormCmd.none` | None - no commands, also known as `[]` |
| `FormCmd.ofMsg` | Command to issue a specific message |
| `FormCmd.map` | When emitting the message, map to another type |
| `FormCmd.batch` | Aggregate multiple commands |
| `FormCmd.ofAsync` | Command that will evaluate an async block and map the result into success or error (of exception) |
| `FormCmd.ofFunc` | Command to evaluate a simple function and map the result into success or error (of exception) |
| `FormCmd.performFunc` | Command to evaluate a simple function and map the success to a message discarding any possible error |
| `FormCmd.attemptFunc` | Command to evaluate a simple function and map the error (in case of exception) |
| `FormCmd.ofPromise` | Command to call `promise` block and map the results |

## F.A.Q.

### Can we avoid boxing / casting ?

This library is using boxing / casting a lot in order to allow us to store different types in a common list. I tried to use interface for `FieldConfig` in order to have something like:

```fsharp
type FieldConfig<'State, 'Msg> =
    abstract member View : 'State * ('Msg -> unit) -> obj
    abstract member Update : 'Msg * 'State -> 'State * (string -> Cmd<'Msg>)
    abstract member Init : 'State -> 'State * (string -> Cmd<'Msg>)
    abstract member Validate : 'State -> 'State
    abstract member IsValid : 'State -> bool
    abstract member ToJson : 'State -> string * Encode.Value
    abstract member SetError : 'State * string -> 'State
```

But then I didn't find a way to store all the fields `FieldConfig<'State, 'Msg>` in a list inside `Config<'AppMsg>`.

If you find a way to either hide the boxing / casting things from the user view or to make everything strongly typed please open an issue to discuss it.

### Why use a fluent API ?

When writing this library I explored several ways for building the DSL. Here is my analysis:

<table>
    <thead>
        <tr>
            <th style="text-align:center"></th>
            <th style="text-align:center">
                <span>
                    Computation Expression
                </span>
            </th>
            <th style="text-align:center">
                <span>
                    Pipeline
                </span>
            </th>
            <th style="text-align:center">
                <span>
                    Fluent
                </span>
            </th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td style="text-align:center">
                Easy to create
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-danger"></i>
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-success"></i>
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-warning"></i>
            </td>
        </tr>
        <tr>
            <td style="text-align:center">
                Easy to extend
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-danger"></i>
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-success"></i>
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-success"></i>
            </td>
        </tr>
        <tr>
            <td style="text-align:center">
                Terse
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-warning"></i>
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-danger"></i>
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-success"></i>
            </td>
        </tr>
        <tr>
            <td style="text-align:center">
                Discoverability
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-warning"></i>
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-success"></i>
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-success"></i>
            </td>
        </tr>
        <tr>
            <td style="text-align:center">
                <span>
                    Naturally follows indentation
                    <span class="icon tooltip is-tooltip-multiline has-text-grey-light" data-tooltip="Evaluation is based on how easy it is to distinguish the different blocks">
                        <i class="fa fa-question-circle"></i>
                    </span>
                </span>
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-success"></i>
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-warning"></i>
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-success"></i>
            </td>
        </tr>
        <tr>
            <td style="text-align:center">
                <span>
                    Allow optional arguments
                    <span class="icon tooltip is-tooltip-multiline has-text-grey-light" data-tooltip="This is useful for the validators. For example, you can make a 'custom error message' optional">
                        <i class="fa fa-question-circle"></i>
                    </span>
                </span>
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-danger"></i>
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-danger"></i>
            </td>
            <td style="text-align:center">
                <i class="fa fa-circle has-text-success"></i>
            </td>
        </tr>
    </tbody>
</table>

### Am I forced to use Bulma/Fulma ?

No, I used `Bulma` in `Thoth.Elmish.FormBuilder.BasicFields` because it was easier for me as I already know this framework. You can use `WithCustomView` to customize the views.

```fsharp
BasicInput
    .Create("condition")
    // Here you can customize the view function
    .WithCustomView(fun state dispatch ->
        let state = state :?> Checkbox.State

        div [ ]
            [ label [ Class "my-custom-label" ]
                [ str state.Label ]
              input [ Class "my-custom-input"
                      // others properties
                    ] ]
    )
```

### Will there be a Fulma based library ?

Yes, I am already working on it but it's not ready yet for a public release, because I want to support all/most of Bulma features and it takes time to design.

### Is there any CSS included ?

`Thoth.Elmish.FormBuilder` has been designed to be really thin and not tied to a specific CSS framework.

The only special case is if you use the `DefaultLoader`. Then the library will inject **7 lines** of CSS in your `document`.

But if you use a `CustomLoader` then no CSS is injected.
