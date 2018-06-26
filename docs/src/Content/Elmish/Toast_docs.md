# Toast

<article class="message is-warning">
    <div class="message-body">
This library is a <strong>Work In Progress</strong> so changes can still occur.

When using this library with HMR, the view can flicker a bit especially when you are changing the custom view implementation. If you refresh your tab the problem should be gone.

Because you don't use HMR in production, there is no problem to use this library.
    </div>
</article>

This library aims to provide you a nice way to inform the user that something occured. By attaching itself at the top level of your application toasts can be persistant between page navigation.

It also provide a very basic input system. Do not abuse of it, a toast should be about notifying the user not asking him an action.

## Demo

You can see a demo on [this page](https://mangelmaxime.github.io/Thoth/elmish/toast_demo.html).

## How to use ?

1. Add the `Thoth.Elmish.Toast` dependency in your [Paket](https://fsprojects.github.io/Paket/) files: `paket add Thoth.Elmish.Toast --project <your project>`

2. Attach the toast system to your Elmish program

```fs
open Elmish
open Thoth.Elmish

Program.mkProgram init update view
|> Program.withToast Toast.render
|> Program.run
```

3. You can now send `Toast` from any command in your program.

```fs
let update msg model =
    match msg with
    | DemoInfo ->
        model, Toast.message "I am toast of type Info"
                |> Toast.title "Info"
                |> Toast.info
```

## Customize the view

This library include a default `render` function. Like that you can experiment with the library and see if it fit your needs.

<span class="icon is-medium has-text-info"><i class="fa fa-2x fa-exclamation-triangle"></i></span> We strongly encourage you to implement your own render. <span class="icon is-medium has-text-info"><i class="fa fa-2x fa-exclamation-triangle"></i></span>

Examples:

| Name | F# code | CSS code |
|---|---|---|
| Default | [F# code](https://github.com/MangelMaxime/Thoth/blob/master/src/Thoth.Elmish.Toast/Toast.fs#L442-L481) | [CSS code](https://github.com/MangelMaxime/Thoth/blob/master/src/Thoth.Elmish.Toast/css/toast-minimal.css) |
| Fulma | [F# code](https://github.com/MangelMaxime/Thoth/blob/master/demos/Thoth.Elmish.Demo/src/Toast.fs#L24-L68) | [CSS code](https://github.com/MangelMaxime/Thoth/blob/master/demos/Thoth.Elmish.Demo/src/scss/toast.scss) |

## API

### Usage

Any time you want to send a toast you need to follow this rules:

1. Create it and set it's content using `Toast.message`
2. Pipe any number of [Builder](#builder) you want
3. Use one of the [Trigger](#trigger) to set it's [Status](#status) and send it

### Status

```fs
type Status =
    | Success
    | Warning
    | Error
    | Info
```

### Position

```fs
type Position =
    | BottomRight
    | BottomLeft
    | BottomCenter
    | TopRight
    | TopLeft
    | TopCenter
```

### Builder

| Function | Description |
|---|---|
| `Toast.message` | Create a toast and set the message content |
| `Toast.title` | Set the title content |
| `Toast.position` | Set the position |
| `Toast.addInput` | Add an input to the toast |
| `Toast.icon` | Set the icon |
| `Toast.timeout` | Set the timeout in seconds |
| `Toast.noTimeout` | No timeout, make sure to add close button or dismin on click |
| `Toast.dismissOnClick` | Allow user to dismis the toast by cliking on it |
| `Toast.withCloseButton` | Add a close button |

### Trigger

| Function | Description |
|---|---|
| `Toast.success` | Send the toast marked with Success status |
| `Toast.warning` | Send the toast marked with Warning status |
| `Toast.error` | Send the toast marked with Error status |
| `Toast.info` | Send the toast marked with Info status |
