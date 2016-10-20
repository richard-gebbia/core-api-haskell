-----------------------------------------------------------------------------
-- |
-- Module      :  CoreAPI
-- Copyright   :  (c) Richard Gebbia, 2016
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  richard.gebbia@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- An experimental library that, when combined with a suitable "context" 
-- library, allows the user to almost completely eschew IO in favor of pure
-- functions.
--
-----------------------------------------------------------------------------

module Core (
    -- * The Core API
    -- $core_api_intro

    -- * Places
    Place,
    initPlace,

    -- ** InPlaces
    InPlace,
    fromNowhere,
    asInPlace,

    -- ** OutPlaces
    OutPlace,
    toNowhere,
    asOutPlace,
    spread,
    (>$<),
    pair,
    maybeOut,

    -- * The Good Stuff
    Event,
    Context(..),
    onStart,
    onEvent,
    exitable
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Functor.Contravariant hiding ((>$<))

{- $core_api_intro

The primary motivation behind the Core API is that nearly every program can be
described in the following way:

\"When \<some event\> happens, take data from \<some place(s) A\>, transform it 
by \<some pure function f\>, and put the transformed data into 
\<some place(s) B\>.\"

Some examples:

/A video game/: When 1/60th of a second (or whatever your desired frame rate is) 
has passed, grab the data from the input devices and the current game state, 
transform it by the game loop, and put the transformed data into the current 
game state and the screen.

/A web server/: When a client makes a request, grab the data from that request 
(and probably some other data from a database), transform it into an HTTP 
response, and put the transformed data into a port to send to the requesting 
client.

/An automaton or robot/: When the environment triggers the robot's sensors, grab 
the data from the sensors, transform it, and put the transformed data into the 
output ports of the chip (so that the robot could do useful things like move or 
produce sound or whatever).

The goal for this library is to encapsulate this pattern so that the programmer
can focus primarily on transforming data using pure functions and therefore less
on IO.

-}

-- Place

{-|
A /place/ can be thought of as any region where bytes are stored. This can be
system memory, a register, or an IO pin. 'Place's are unique from other 
constructs in Haskell in that they can have different values at different times.
When we talk about \"side effects\" in code, what we're really talking about is 
either taking data from places or putting data into places.

If you're familiar with Reactive Programming, you can think of a place as either
a source or a sink depending on how it's being used.
-}
type Place a = MVar a

{-| 
Creates a new 'Place' that can be used either as an 'InPlace' with 'asInPlace'
or as an 'OutPlace' with 'asOutPlace'. The 'Place' starts out with the data that
you give it. This will only change if it\'s treated as an 'OutPlace' and has 
data written to it.

A common pattern for programs is to hold some state between events and to use
that state for making decisions later. For instance, let\'s say we\'re making a 
simple command-line program that spews a list of insults at the user in order 
and pauses after each one to allow the user to rate how offended they are.  
We\'d want to both know what insult the user is on and lob the next one after 
the user rates one. The way to do this would be to make a place using 
'initPlace', where you\'d read from it to determine what insult the silly user
is on, and you\'d write to it to move on once the user has been thoroughly 
offended.

Something like:

@
'onEvent' ctx userEntersInput spewInsult ratingAndWhich outputAndWhich

spewInsult :: (Rating, Int) -\> (String, Int)
spewInsult (rating, which) = 
    (... figure out what insult to show ... , which + 1)
@

-}
initPlace :: a -> IO (Place a)
initPlace = newMVar

-- InPlaces

{-| 
An 'InPlace' is simply a place that we can take data from. The current time,
the keyboard state, and microphone input are examples of data that we can get 
from input places. 'onEvent' will take the data from the 'InPlace' given at the
time of the event and put it through the given @xform@.

'InPlace's are instances of @Applicative@ so you can construct \"bigger\" 
'InPlace's out of \"smaller\" ones. As an example, let\'s say at a given point 
in time you want to get both the current time and the mouse position. You have
two 'InPlace's: 

@
currentTimePlace :: 'InPlace' UTCTime
mousePosPlace :: 'InPlace' Vec2
@

You can make a data structure:

@
data TimeAndMousePos = TimeAndMousePos
    { currentTime :: UTCTime
    , mousePos :: Vec2
    }
@

And with that data structure you can make a new 'InPlace':

@
timeAndMousePos :: 'InPlace' TimeAndMousePos
timeAndMousePos = TimeAndMousePos
    \<$\> currentTimePlace
    \<*\> mousePosPlace
@
-}
newtype InPlace a = InPlace { getPlaceContents :: () -> IO a }

instance Functor InPlace where
    fmap f ipa = InPlace (\_ -> (getPlaceContents ipa) () >>= return . f)

instance Applicative InPlace where
    pure x = InPlace (\_ -> pure x)
    
    ipf <*> ipa = InPlace (\_ -> do
        f <- (getPlaceContents ipf) ()
        a <- (getPlaceContents ipa) ()
        return $ f a)

{-| 
A trivial 'InPlace' that will only ever have @()@ when it\'s read.
-}
fromNowhere :: InPlace ()
fromNowhere = pure ()


{-| 
Given a 'Place' made with 'initPlace', this will allow that 'Place' to be 
treated as an 'InPlace' so that its data can be read.
-}
asInPlace :: Place a -> InPlace a
asInPlace mvar =
    InPlace (\_ -> readMVar mvar)


-- OutPlaces

{-| 
An 'OutPlace' is simply a 'Place' that we can put data into. The screen and 
speakers are examples of output devices that would have 'OutPlace' interfaces
in code. 'onEvent' will put the result of its @xform@ parameter into the 
supplied 'OutPlace'.

Unlike 'InPlace's, 'OutPlace's are not instances of @Applicative@. It does,
however, have the 'spread' function, which will allow you to make \"bigger\"
'OutPlace's from \"smaller\" ones in a similar fashion.
-}
newtype OutPlace a = OutPlace { render :: a -> IO () }

instance Contravariant OutPlace where
    contramap g fb = OutPlace (\a -> (render fb) (g a))


{-| 
A trivial 'OutPlace' that simply ignores whatever is put into it.
-}
toNowhere :: OutPlace a
toNowhere = OutPlace (\_ -> return ())


{-| 
Given a 'Place' made with 'initPlace', this will allow that 'Place' to be 
treated as an 'OutPlace' so that it can be written to.
-}
asOutPlace :: Place a -> OutPlace a
asOutPlace mvar = 
    let 
        putIntoMVar :: Place a -> a -> IO ()
        putIntoMVar mvar x = 
            let
                put :: Bool -> Place a -> a -> IO ()
                put isEmpty mvar x =
                    if isEmpty then
                        putMVar mvar x
                    else
                        void $ swapMVar mvar x
            in do
                isEmpty <- isEmptyMVar mvar
                put isEmpty mvar x
    in
    OutPlace $ putIntoMVar mvar


{-| 
Build bigger 'OutPlace's from smaller ones using this function. Let\'s say you
have the following data structure.

> data JimmyBuffett = JimmyBuffett
>     { jimmy :: Jimmy
>     , buffett :: Buffett
>     }

You can make an @OutPlace JimmyBuffett@ with the following:

@
'spread'
    [ jimmy '>$<' margaritaville      -- margaritaville :: OutPlace Jimmy
    , buffett '>$<' paradise          -- paradise :: OutPlace Buffett
    ]
@
-}
spread :: [OutPlace a] -> OutPlace a
spread opas = 
    OutPlace (\a -> sequence_ $ map (($ a) . render) opas)

{-|
The '>$<' operator is the infix version of @contramap@ ('OutPlace's are 
instances of @Contravariant@ rather than @Functor@), similar to how @\<$\>@ is
the infix version of @fmap@. It's exposed in this library so you don\'t have to 
pull in another dependency.
-}
(>$<) :: (a -> b) -> OutPlace b -> OutPlace a
(>$<) = contramap

{-|
Combine two 'OutPlace's into one that takes a pair.
-}
pair :: (OutPlace a, OutPlace b) -> OutPlace (a, b)
pair (opa, opb) = 
    OutPlace (\(a, b) -> do 
        (render opa) a
        (render opb) b)

{-|
Most raw 'OutPlace's won\'t take @Maybe@s, but you might conditionally want to 
write to them.
-}
maybeOut :: OutPlace a -> OutPlace (Maybe a)
maybeOut opa = 
    OutPlace $ maybe ((render toNowhere) ()) (render opa)


-- Events

{-|
An 'Event' is intended to represent a set of points in time. Unlike standard 
\"Event-oriented programming,\" an 'Event' in this context doesn\'t carry any
data with it. An example of an 'Event' would be the points in time when the user
clicks the mouse or every 16ms since the start of the program.
-}
newtype Event ctx = Event (ctx -> IO ())


eventHandler :: (a -> b) -> InPlace a -> OutPlace b -> IO ()
eventHandler xform inPlace outPlace = do
    a <- (getPlaceContents inPlace) ()
    b <- return $ xform a
    (render outPlace) b


{-|
Causes a particular movement of bytes to happen when the program starts.
-}
onStart = eventHandler


{-|
Probably the most important function in this library, 'onEvent' allows you to
hook up a movement of bytes to occur at particular points in time.

As an example, say you want to make an "importantizer" program, which takes its 
input and makes it IMPORTANT. The way that you would do that is something like:

@
'onEvent' ctx userEntersInput toUpper stdin stdout
@

Let\'s break this down parameter by parameter:

    - @ctx@ is a \"context\", a collection of places and events. Not all 
    computers are built alike. PCs, for instance, generally have keyboards and 
    mice and speakers, but robots, mobile phones, and IoT devices probably
    won\'t have the same events and I\/O places. 

    - @userEntersInput@ is an 'Event', representing the points in time when a 
    user enters a line into the terminal. Internally, it's a function that takes
    a context, which is why one is supplied as the first argument to 'onEvent'.

    - @toUpper@ is the transformation of data that occurs before outputting.
    Data will come in through @stdin@, and before it is moved to @stdout@, it\'s
    transformed by @toUpper@. Uppercase always makes text more important.

    - @stdin@ is the 'InPlace' where the data came from. In this instance, it 
    probably came from a line in the terminal. The actual value of @stdin@
    likely came from a function on @ctx@ because the concept of 
    \"terminal input\" will be foreign to devices that don\'t have terminals.

    - @stdout@ is the 'OutPlace' to where the data will be sent. In this 
    instance, it will probably go to the terminal. The actual value of @stdout@
    likely came from a function on @ctx@ because the concept of 
    \"terminal output\" will be foreign to devices that don\'t have terminals.

You can read this line of code as: \"When the user enters input into the 
terminal, call @toUpper@ on it and send it to the terminal output.\"
-}
onEvent :: ctx -> Event ctx -> (a -> b) -> InPlace a -> OutPlace b -> IO ()
onEvent context (Event waitForEvent) xform inPlace outPlace =
    void $ forkIO $ forever $ do
        waitForEvent context
        eventHandler xform inPlace outPlace
        yield


-- Outside Context

{-|
A \"context\" is a collection of 'Event's, 'InPlace's and 'OutPlace's. It is
usually required to be in the @IO@ monad to get access to one, since 'Event's
and 'Place's are all side-effects. The general way to use this library at the is
at the top level of your program like so:

@
main = do
    -- get the context
    ctx \<- 'initContext'

    -- curry onEvent so we can elide the \"ctx\" parameter
    let onEvent\' = 'onEvent' ctx

        -- get stdin and stdout (this is still in the \"let\" block)
        stdin\' = stdin ctx
        stdout\' = stdout ctx

    -- actual non-accidental details (i.e. the program) go here
    onEvent\' userEntersInput toUpper stdin\' stdout\'
@
-}
class Context ctx where
    initContext :: IO ctx


{-|
As a consequence of the Core API\'s design, 'onEvent' will run your program 
forever. If you\'d like your program to quit at some point, you\'ll have to tell
it to do so manually. You can use 'exitable' to make what would be your @main@ a 
function that takes an extra 'OutPlace' parameter that, when written to (it 
takes @()@), will quit.

Example usage:

@
myProgram :: OutPlace () -\> IO ()
myProgram exitPlace = do
    ctx \<- 'initContext'
    'onEvent' ctx someEvent someTransform someInPlace ('pair' (someOutPlace, 'maybeOut' exit))

main = 'exitable' myProgram
@

In this example, @someTransform@ will have to take whatever comes into it and
transform it into something of type @(SomeType, Maybe ())@, where @someOutPlace@
is of type @OutPlace SomeType@.
-}
exitable :: (OutPlace () -> IO ()) -> IO ()
exitable action = do
    exitMVar <- newEmptyMVar
    action $ asOutPlace exitMVar
    takeMVar exitMVar
