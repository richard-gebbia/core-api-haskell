# core-api

The Core API is an experiment in software architecture. Like other architecture
APIs, its noble goal is to improve software quality by allowing its users to
more succintly and more empirically describe their code's goals.

The idea for the Core API arose from a hypothesis I had: as software engineers,
we only ever really want our code to do one of two things.

1. Transform data from one form to another
2. Move data from one "place" (read: region of byte storage) to another, at a 
   specific point in time

(Note: I could be totally wrong about this, but I honestly can't think of 
something I normally would want my code to do that isn't (or can't be 
restructured as) one of these two things.)

My ultimate goal with the Core API is to test this hypothesis, by presenting an
interface that allows the user to directly address those two concerns and 
nothing more.

## How do we write programs then?

You can theoretically write entire specifications for programs along the lines
of:

When {some event} happens, grab the data from {some places}, transform it by 
{some transformation}, and put the transformed data into {some other places}.

Here are some examples:

- **A video-game**: When 1/60th of a second (or whatever your desired frame rate 
  is) has passed, grab the data from the input devices and the current game 
  state, transform it by the game loop, and put the transformed data into the 
  current game state and the screen.

- **A web server**: When a client makes a request, grab the data from that 
  request (and probably some other data from a database), transform it into an 
  HTTP response, and put the transformed data into a port to send to the 
  requesting client.

- **An automaton/robot**: When the environment triggers the robot's sensors, 
  grab the data from the sensors, transform it, and put the transformed data 
  into the output ports of the chip (so that the robot could do useful things 
  like move or produce sound or whatever).

And there are many more that quite neatly fit this description. These examples
are here to hopefully convey the breadth of programs that this model can 
produce.

The Core API has as direct a translation of this model as I could make in 
Haskell (and hopefully more languages, as well) (update: there's a Clojure 
version, as well).

## No really, how do we write programs with this?

The most important function in the Core API is `onEvent`, which has the 
following signature:

    onEvent :: ctx -> Event ctx -> (a -> b) -> InPlace a -> OutPlace b -> IO ()
    onEvent context event xform inPlace outPlace = ...

- **context**: Not all hardware is built the same, and it's very likely that 
  the I/O ports on a robot will be very different from the I/O ports on a 
  personal computer. The "context" is `onEvent`'s gateway to all the I/O 
  "places" that can be read from or written to, as well as events that can be
  subscribed to.
- **event**: This is the event to subscribe to. An event is similar 
  to the concept from event-driven OOP, except that it *only* exists to 
  represent *a point in time*, so it cannot "carry" any data with it.
- **xform**: This is a function that gets called when the event specified by
  `event` occurs. A the time of the `event`, a value of type `a` will be read
  from the given `inPlace`, and that `a` will be given to `xform`. The result of
  `xform` will be given to `outPlace`.
- **inPlace**: This is the "place" to grab data from at the time of the `event`.
- **outPlace**: This is the "place" to put data into after it has been taken
  from `inPlace` and transformed by `xform`.

Let's look at a basic use case for this function:

```haskell
main = do
  context <- initContext
  onEvent context userEntersInput (\_ -> "Hello!") fromNowhere stdout
```

I'd like to stray from the specifics of `context` for right now (it will be
covered later). For our purposes now, just consider that not all hardware will 
have a place called `stdout` to put data into.

You can read this as:

When the event `userEntersInput` happens, grab `()` from input places 
(`fromNowhere` simply makes a `()` out of thin air), transform that `()` by the 
function `(\_ -> "Hello!")`, and put the result into `stdout`.

Let's look at a slightly more complex example:

```haskell
echoUserInput :: String -> String
echoUserInput = id

main = do
  context <- initContext
  onEvent context userEntersInput echoUserInput stdin stdout
```

There are a couple of things that are different between this example and the 
previous one:
- The `xform` function now actually uses its one parameter.
- The `inPlace` parameter to `onEvent` is now `stdin`.
- The `xform` function "grabs" the data inside the place named `stdin` and uses 
  it in the body of the function. 

You can read this as:

When the user inputs a line into stdin, grab the data from stdin, transform it
by the function `echoUserInput`, and put the result into `stdout`.

## The Place API

Ideally, a platform's "context" would just contain low-level "places". For 
simple, bare platforms, that could mean just straight-up I/O ports for the 
places.

While these would be nice for platform creators to provide, generally 
programmers don't want to deal directly with I/O ports. When I say "put the
string 'Hello' into stdout," that could mean a bunch of individual places get
written to. Conversely, when I say "grab the mouse position", that means
that I actually want to read data from more than one place.

The *Place API* exists to do just that. It contains two types: `InPlace` and 
`OutPlace`.

### `InPlace`s

`InPlace`s are instances of `Applicative`, so you can build "bigger" (or "more
abstract") `InPlace`s using the standard `Applicative` builder pattern.

Here's an example: let's say at a given point in time you want to get both the 
current time and the mouse position. You have two `InPlace`s: 

```haskell
currentTimePlace :: InPlace UTCTime
mousePosPlace :: InPlace Vec2
```

You can make a data structure:

```haskell
data TimeAndMousePos = TimeAndMousePos
    { currentTime :: UTCTime
    , mousePos :: Vec2
    }
```

And with that data structure you can make a new `InPlace`:

```haskell
timeAndMousePos :: InPlace TimeAndMousePos
timeAndMousePos = TimeAndMousePos
    <$> currentTimePlace
    <*> mousePosPlace
```

### `OutPlace`s

On the other side of the coin, we have `OutPlace`s, which are `Contravariant`
and can therefore not be made an instance of `Applicative` to use the same
builder pattern as with `InPlace`s. Instead we have `spread`, which takes a list
of ways to put a "big" (read: user-defined) structure into a "small" 
(read: built-in) `OutPlace`, and then it creates a new `OutPlace` that writes a "big"
structure in all of the provided, "small" ways. If that doesn't make any sense 
and you now think I'm on some kind of heavy medication, here's the type and 
example usage.

With this "big" structure:

```haskell
data Tee = Tee
    { teeFile :: File
    , teeStdout :: Stdout
    }
```

you can make an `OutPlace Tee` with the following:

```haskell
spread
    [ teeFile >$< fileNamed "foo.txt" -- fileNamed "foo.txt" :: OutPlace File
    , teeStdout >$< stdout            -- stdout :: OutPlace Stdout
    ]
```

The function `(>$<)` is the contravariant version of `fmap`. Where `fmap` has
the type: 

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

`(>$<)` has the type:

```haskell
(>$<) :: Contravariant f => (a -> b) -> f b -> f a
```

If you're not sure what "contravariant" means, don't worry. For the purposes of
this library, it just means that you have to use `spread` instead of 
`Applicative`.

Hopefully, this professional dataflow diagram will make it easier to visualize.
I paid someone $5 US to make this for me so it better work.

```
Input                                                       Output
in-place A                                             out-place F
          \                                           ^
           \                                         /
            v                                       /
in-place B---> in-place D --xform--> out-place E ----> out-place G
            ^                                       \
           /                                         \
          /                                           v
in-place C                                             out-place H

=============>                                      =============>
applicative                                                 spread
```

You probably still think I'm on heavy medication at this point, and you may be
right ;).

## Still to do
If you got this far down actually reading everything, then congratulations. I am
fully aware that this README is probably not in a perfect state and that there
are likely parts that are confusing or overly complicated. Please feel free to
tell/ask me about anything in the repo and/or README. I would sincerely 
appreciate suggestions.

This repo is primarily here as an idea. It is in no way production-ready, mainly
because I haven't filled out any real context to use. The events and places I
use in the repo were created out of necessity to build the example code. If I
feel confident that the API works and is useful, I will certainly consider 
building a "standard context".

Here are some other things I'd like to get done:
- standard means for the user to generate and apply their own context
- ways to easily combine or add functionality to event handlers
- WAYYYYYY more examples
- build a timeout scheduler
- port the API to another language, probably 1/2 curly brace languages

## License

Copyright © 2020 Richard Gebbia

Distributed under the BSD3 license.
