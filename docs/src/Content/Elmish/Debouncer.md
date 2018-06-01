# Debouncer

## Kezako ?

> Debouncing enforces that a function not be called again until a certain amount of time has passed without it being called. As in "execute this function only if 100 milliseconds have passed without it being called."
>
> Perhaps a function is called 1,000 times in a quick burst, dispersed over 3 seconds, then stops being called. If you have debounced it at 100 milliseconds, the function will only fire once, at 3.1 seconds, once the burst is over. Each time the function is called during the burst it resets the debouncing timer.
>
> [Source](https://css-tricks.com/the-difference-between-throttling-and-debouncing/)

## Demo

The following demo works like that:

1. Ask the user to type something
2. User is typing
3. When user **stop** typing, disable the input and inform the user.
4. After a **short period** of time, **reset** the demo.

<div class="columns">
    <div class="column is-4 is-offset-4">
        <div id="debouncer_demo"></div>
    </div>
</div>


<script type="text/javascript" src="/demos/demo.js"></script>
<script type="text/javascript">
    Demos.DebouncerDemo("debouncer_demo");
</script>

## How to use ?

