# Obelisk Routing

## Motivation

The `obelisk-route` package is designed to help with managing the paths and parameters for
routing in your application.

This is distinct from packages like `servant` that are designed for specifying endpoints for a
REST API. Remember that when it comes to handling the routes on the backend, whatever handler
you're using for the routes will need to handle the methods, and headers etc.

This may seem like an oversight in `obelisk-route` if you're used to routing systems where you
have to specify all of these things. But this package is designed and built to comply with three
core guarantees:

* Every route value can be encoded to a URL

Every route that you declare in your types will produce a valid URL, i.e. encoding cannot fail.

* Encoding roundtrips, which also implies that there's no ambiguity in the routes

```haskell
decode . encode = pure
```

Route declared using `obelisk-route` are bidirectional, meaning that any route that can be encode
to types and matched on can also be encoded as a url *without losing information*. This also
provdies a test case that you can apply to any routes that you create. If they fail this test
then they are not valid routes.

* Common modifications made to routes, whereever possible, will be caught by the compiler.

When you make common modifications to routes, such as adding or removing a route, you get
"incomplete case" and similar warnings everywhere you need to update your application. Changing
the type of a route will be a complete build failure until each use-case is fixed. This makes it
*much* harder to miss things when you add routes.

Additionally `obelisk-route` is required by design to render a page on the backend exactly as it
would appear when rendered on the frontend. This would be difficult to impossible to enforce if
the routing system worked differently in the two contexts.

When we looked at existing routing solutions none of them provided all of these guarantees, and
some didn't provide any. Being able to rely on these guarantees is the primary goal behind the
creation of `obelisk-route`.

This package will ask a lot of you compared to other routing solutions. We can't prevent
arbitrary shadowing so you will need to mindful of that. However this package offers a lot in
return and the guarantees provided by the design, semantics, and types of this package will more
than make up for it.

## Simple route with one parameter

Consider the following `Frontend` route that we want our application to be able to handle:

```
/user/42
```

It has a static portion "user" and some number that we want to verify and have available on our
page for us to use. We'll briefly discuss some of the types that are involved and then go through
the process of adding this route to our application.

## Building blocks

There are a few pieces required to build routes in Obelisk and there is plenty to dig into if you
are curious. It may seem daunting and there is certainly a lot of power on offer. However for our
purposes we can survive with a surface level understanding.

Below we will cover a few of the types that we need to interact with to add routes to our
application. Again you don't have to have a deep understanding of these types, a high level view
will suffice for now.

#### `Encoder`

An `Encoder` may describe how an individual segment of a route is to be encoded. Such as how to
read the number from a route like `/user/33`. Or how several routes are encoded, such as an
`Encoder` for all of the sub-routes pertaining to 'blog posts', such as:

- /blog/:postId:/edit
- /blog/:postId:/metrics
- etc...

In particular note that `Encoder`s may be composed, thereby creating more elaborate routing
structures from simpler pieces. It also means that scary looking routing hierarchies can be
partitioned and made easier to work with.

#### `SegmentResult`

This is the building block of individual routes, it has a list-like structure with a
recursive component, and a terminating case.

The recursive case is the `PathSegment Text (Encoder check parse a PageName)` constructor that
encodes a literal segment in the path, followed by one or more `Encoder`s.

The terminating case is the `PathEnd (Encoder check parse a (Map Text (Maybe Text)))` constructor.
It is used to indicate that there is only this single segment in that route, or to terminate a longer route.

#### `BackendRoute` & `FrontendRoute`

An Obelisk application begins with two distinct sets of routes for backend and frontend. These
are defined as GADTs called `BackendRoute` and `FrontendRoute`, respectively. These are provided
for you to start you off.

For more info on GADTs check out the following links:
* [Haskell Wiki](https://wiki.haskell.org/Generalised_algebraic_datatype)
* [Haskellforall](http://www.haskellforall.com/2012/06/gadts.html)

In a fresh Obelisk application these live in the `Common.Route` module.

`BackendRoute` is for serving static things or deferring to other backend endpoints. Such as:
- "static/files/img/:imgName:"
- "api/..." where the "..." portion is managed by a REST endpoint package of your choice.

These backend routes can run their own code or hand off to a different request handling package
to a finer grained set of endpoints. But should none of them match the frontend of the
application is served and the frontend router will continue parsing the route.

`FrontendRoute` is for managing the frontend routing of your application. Such as:
- "blog/:userId:/:postId:"
- "dashboard?optionA=true".

Building routes for either one is the same process. Keeping the types separate enforces the
distinction at compile time and helps to prevent errors.

#### Obelisk live development environment

For best results, work through this tutorial using a freshly created Obelisk application. To do
so, ensure the `ob` command is installed and run the following:

```shell
$ mkdir ob-routes-tutorial
$ cd ob-routes-tutorial
$ ob init
```

After `ob init` has done its thing, use the live development environment provided by `ob`:

```shell
$ ob run
```

### Defining our route as a type

When we define this route, we will need to tell Obelisk what want to do with that number. We
_could_ leave it as `Text`, but we know it needs to be a number. We could encode to a bare `Int`,
but we're responsible Haskellers so we will define a `newtype` in `Common.Api` to wrap the number
for greater type safety and to be more descriptive:

```haskell
newtype UserId = UserId { unUserId :: Int }
```

To create the route itself, we add a constructor to the `FrontendRoute` GADT in `Common.Route`.

The purpose of this constructor is to give us something to match on when we're deciding which
code to run. It will also be used to help build type-safe links within our application, as well
as declare what the types are that will be pulled from the route.

Add the following constructor to `FrontendRoute` in `Common.Route`:

```haskell
  FrontendRoute_User :: FrontendRoute UserId
```

> The naming of the route 'FrontendRoute_User' is an Obelisk convention of including the type
> name in individual constructor names. This helps disambiguate the code at the 'cost' of a few
> extra keystrokes. As an example:
>
> ```haskell
> data Foo
>   = Foo_ConstructorA
>   | Foo_ConstructorB
> ```

### Building our route from segments

Next we need to build the `SegmentResult` that will tell Obelisk how this route is to be encoded
in either direction.

If you're following along with `ob run` running then after you add this constructor (and save the
file) you will see incompleteness warnings appear in the feedback.

Broadly speaking, the Obelisk router breaks down incoming routes into segments that are given to
our `Encoder` to find a match. We define these segments using the `SegmentResult` type and add
them to the case expression in `Common.Route` to be matched against.

Add our new constructor to the following function in `Common.Route` as another pattern in the
`case` expression for the frontend routes:

```haskell
fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty)
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      -- Add the new constructor here
  )
```

Our route has multiple segments so we need to use the `PathSegment` constructor:

```haskell
  | PathSegment Text (Encoder check parse a PageName)
```

We declare the first segment by using the literal value "user" as the first argument to the
constructor that we added to the `case` expression:

```haskell
  FrontendRoute_User -> PathSegment "user" _todo
```

We can now move onto building the `Encoder` for our `UserId`.

#### Wait a minute, shouldn't I be matching on the route on the left?

In many routing systems you match directly on the route input and break it down as required. This
process can be fragile and has limitations. Additionally you are left on your own when comes to
_creating_ links in your application. Because there is no way to relate the structure of a route
to anything, you have to manually build up routes again, and if those routes change it can be
onerous to find and fix all the constructed links.

Obelisk routes are bidirectional, which means the `Encoder` that you create works as both a
pattern match for incoming routes. As well as using the GADT constructors as a type safe
mechanism for _creating_ links in your application. It's a compile error to try to use route
constructors that don't exist, and if you change the type of a route the application will not
build until you fix that change every where it appears.

This design also enables the composition of `Encoder`s which is difficult and much more prone to
error if you start by matching on the route itself.

----

### UserId Encoder

Because the Obelisk routes are bidirectional, we need describe how to encode our type as a url
segment at the same time as describing how to encode the url segment as our type.

In other words, we need to tell Obelisk how to create a `UserId` from `Text` and back again. To
do this we're going to write two functions and put them together into a `Prism'`.

A `Prism'` is a condensed way of describing how to go from one type to another and _maybe_ back
the other way.

The types of the two functions are:

```haskell
_ :: UserId -> Text
-- and
_ :: Text -> Either Text UserId -- because not all Text is a valid UserId
```

The two functions are combined into a `Prism'` using the `prism` function. That function and the
`Prism'` type will need to be imported from the `Control.Lens` module.

```haskell
import Control.Lens (Prism', prism)
```

Turning our `UserId` into `Text` is unremarkable so let's do that one first. Define the prism in
the `Common.Api` module, next to our `UserId` newtype definition:

```haskell
userIdTextPrism :: Prism' Text UserId
userIdTextPrism = prism toText _fromText
  where
    -- Unwrap the UserId, use 'show' on the 'Int' value, and then turn that into 'Text'
    toText = T.pack . show . unUserId
```

Next to encode `Text` from the route input as a `UserId`. This means we have to check that this
input is valid. Our requirement is that the input is a number greater than zero:

```haskell
userIdTextPrism :: Prism' Text UserId
userIdTextPrism = prism toText fromText
  where
    -- Unwrap the UserId, use 'show' on the 'Int' value, and then turn that into 'Text'
    toText :: UserId -> Text
    toText = Text.pack . show . unUserId

    fromText :: Text -> Either Text UserId
    fromText t = case readMaybe $ Text.unpack x of
      -- Match when we have successfully read an Int value
      -- and use a 'guard' to ensure it is greater than zero.
      Just i | i > 0 -> Right $ UserId i
      -- For everything else we return a Left value to indicate a failure.
      _ -> Left x
```

We pass our `Prism'` to the aptly named `reviewEncoder` function from `Obelisk.Route` to create
the `Encoder`. In doing so we avoid the boring bits of having to chop up the route ourselves:

```haskell
reviewEncoder userIdTextPrism :: Encoder parse check UserId Text
```

Add this to the end of our `case` expression in `Common.Route`:

```haskell
FrontendRoute_User -> PathSegment "user" $ _todo . reviewEncoder userIdTextPrism
```

Next is to tell Obelisk that we have only a single path segment and once it has matched the
static part and the `UserId` this route is complete. This is done by composing our `UserId`
encoder with the `singlePathSegmentEncoder` using `(.)` imported from `Control.Category`:

#### A more general composition

Importantly the composition operator we need is the more general version from the
`Control.Category` module. This is because the one from `Prelude` is specialised to the function
arrow type `(->)`, whereas the one in `Control.Category` uses the `Category` typeclass:

```haskell
-- (.) from Prelude
(.) :: (b -> c) -> (a -> b) -> a -> c

-- (.) from Control.Category
(.) :: Category cat => cat b c -> cat a b -> cat a c
```

Ensure that `(.)` is imported correctly at the top of `Common.Route`:

 ```haskell
import Prelude hiding ((.))
import Control.Category
```

----

Our addition to the `case` expression should look like this:

```haskell
  FrontendRoute_User -> PathSegment "user" $ singlePathSegmentEncoder . reviewEncoder userIdTextPrism
```

It should be a rare event that you will need to write your own `Encoder` function like
`reviewEncoder` or `singlePathSegmentEncoder` directly. The provided `Encoder`s in `Obelisk.Route`
are designed to be composed and combined to build more complicated encoders and should be
sufficient for most requirements. If you find this is not the case and there is a pattern that
keeps appearing and is not accounted for, then that is probably a bug and we'd like to know about
it!

#### Aside: 'Wrapped' typeclass and Template Haskell.

If you're comfortable with Template Haskell, you can use the `makeWrapped` splice from
`Control.Lens.TH` to derive the `Wrapped` typeclass. This would let you avoid having to write a
`Prism'`, and you could use the `unwrappedEncoder` to handle wrapping up the `Int` value produced
by using the `readShowEncoder`. That encoder relies on the `Read` and `Show` instances for a
given type and may not be appropriate for a URL:

```haskell
PathSegment "user" $ readShowEncoder . unwrappedEncoder
```

This is handy if you already have your newtypes defined and don't want to hand write `Prism'`s.
This uses the `_Wrapped` prism from the `lens` library and then the encoder that uses the `Read`
and `Show` instances for `Int`.

----

## Using routes

Now we can update the Frontend of our application to do something with this routing information.

We're going to make the rest of these changes in the main `Frontend.hs` in the `frontend`
package. If you're following along with a freshly minted Obelisk application then it should look
something like this:

```haskell
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Welcome to Obelisk!"
      el "p" $ text $ T.pack commonStuff

      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

      elAttr "img" ("src" =: static @"obelisk.jpg") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s
      return ()
  }
```

We're going to modify the function at `_frontend_body` to show different content for the main or
user page, and then link between them.

Notice that part of the type of `frontend` is `R FrontendRoute`. That is the same `FrontendRoute`
type that we added our user route to. This tells us which routes are available for us to match on
in this function. We can't accidentally match on a route that is not part of that type, it would
be a compile error.

At the top of our `_frontend_body` function, before the `do`, we're going to insert the
`subRoute_` function and turn this entire function into a `case` expression that shows different
content based on the current route. The existing content will be moved into the branch for
`FrontendRoute_Main` and it will look a bit like this:

```haskell
  , _frontend_body = subRoute_ $ \r -> case r of
    FrontendRoute_Main -> do
      el "h1" $ text "Welcome to Obelisk!"
      {- The initial body content goes here -}
```

Alternatively you can turn on the `{-# LANGUAGE LambdaCase #-}` language extension and avoid
needing to bind the `r` variable:

```haskell
{-# LANGUAGE LambdaCase #-} -- put this at the top of the file
...
  , _frontend_body = subRoute_ $ \case
    FrontendRoute_Main -> do
      el "h1" $ text "Welcome to Obelisk!"
      {- The initial body content goes here -}
```

As another branch in `case` expression, add our `FrontendRoute_User` constructor:

```haskell
  , _frontend_body = subRoute_ $ \case
    FrontendRoute_User -> _todo
    FrontendRoute_Main -> do
```

Now to add some content.

Using the `askRoute` function we can access a `Dynamic` of our `UserId`. This is because the type
that we had in our `FrontendRoute` GADT type is stored in a `ReaderT` that changes based on which
route we're currently matching on.

Were we to use `askRoute` in the `Frontend_Main` branch of our `case` expression, the resulting
`Dynamic` would contain a value of unit `()` because that is the type of the `Frontend_Main`
route.

Update the `FrontendRoute_User` branch of our `case` expression to the following:

```haskell
  , _frontend_body = subRoute_ $ \case
    FrontendRoute_User -> do
      text "We're on the user page! But which user?"
      dUserId <- askRoute
      dyn_ $ ffor dUserId $ \(UserId uidVal) ->
        text "This user : " <> T.pack (show uidVal)

    FrontendRoute_Main -> do
      ...
```

### Building links

We have separate content based on the route, but we don't have any links yet!

With the `routeLink` function we can create type safe links to different parts of our
application. We will first link to the user page from the main page. Somewhere in the body of the
`FrontendRoute_Main` content, add the following:

```haskell
routeLink (FrontendRoute_User :/ User 42) $
  text "Visit the page of User # 42"
```

We define the link we want by using the constructor from our `FrontendRoute` type and using the
function `(:/)` from `Obelisk.Route`, and we provide the required input of a `UserId` to satisfy
the type of `FrontendRoute_User`. Then we create a child widget with some content that when
clicked will take us to the user page.

Similarily, we can create a link on the user page to take us to the main page:

```haskell
routeLink (FrontendRoute_Main :/ ()) $
  text "To the main page!"
```

You should now have an Obelisk application with some rudimentary routing and enough of a start to
begin building your application with type safe routes! Hooray!!

The next topic we will cover is adding groups of sub-routes, as well as routes with multiple parameters.