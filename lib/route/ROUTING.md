# Obelisk Routing

## Motivation

The `obelisk-route` package is designed to help with managing the paths and parameters for routing
in your application.

A POST request makes no sense for the frontend of an application so we do not support
creating this type of route. As it wwould be difficult to impossible to enforce if the routing
system worked differently on the backend vs the frontend

Most importantly it has been designed and built to provide the following guarantees.

* **Every route value can be encoded to a URL**

Every route that you declare in your types will produce a valid URL, i.e. encoding cannot fail.

* **Encoding round-trips, which also implies that there's no ambiguity in the routes**

```haskell
decode . encode = pure
```

Route declared using `obelisk-route` are bidirectional, meaning that any route that can be encode
to types and matched on can also be encoded as a url *without losing information*. This also
provdies a test case that you can apply to any routes that you create. If they fail this test
then they are not valid routes.

* **Common modifications made to routes, whereever possible, will be caught by the compiler.**

When you make common modifications to routes, such as adding or removing a route, you get
"incomplete case" and similar warnings everywhere you need to update your application. Changing
the type of a route will be a complete build failure until each use-case is fixed. This makes it
*much* harder to miss things when you add routes.

It may seem odd that `obelisk-route` does not provide a way to specify the HTTP Method for a given
route. This is due to the requirement that this package may be used when rendering a page on the
backend **or** the frontend. It would be difficult to impossible to provide these guarantees if the
routing system worked differently on the backend vs the frontend.

When we looked at existing routing solutions none of them provided all of these guarantees and some
didn't provide any. Being able to rely on these guarantees is the primary goal behind the creation
of `obelisk-route`.

This package will ask a lot of you when compared to some other routing solutions. We can't prevent
arbitrary shadowing of routes for example, so you will need to mindful. However this package offers
an awful lot in return. We think that the guarantees provided by the design, semantics, and types of
this package will more than make up for it.

## Thinking in routes

If you approach this package as a collection of 'encoders' and 'decoders' for turning strings into
things and things into strings. Then more often than not, you will find yourself fighting the API
and things will be needlessly difficult.

Instead, shift your thinking to view this package as a collection of the smallest possible
abstract blocks that you _compose_ to build a declaration of your routing structure. By composing
and building routes from pieces that are so small that they are "obviously correct", you can be
confident that the composition of those individual pieces is also correct.

As we build up some examples in this guide, we will demonstrate this capability and how
`obelisk-route` likely already has all the tools you need to construct a definition of your
routing structure. As well as cover what to do when it doesn't, and what you need to know to
ensure that you build correct and composable pieces.

## Simple route with one parameter

The first example we will use is the following `Frontend` route:

```
/user/42
```

It has a static portion "user" and some number that we want to verify and have available on our
page for us to use. We'll briefly discuss some of the types that are involved and then go through
the process of adding this route to our application.

## Building blocks

There are a few pieces required to build routes in Obelisk and there is plenty to dig into if you
are curious. For the time being we can survive with a surface level understanding.

#### `Encoder`

This is the primary building block for any route definition. It looks a lot worse than it is:

```haskell
-- The 'check' and 'parse' type variables will be covered later
data Encoder check parse a b
```

You can think of an `Encoder` as a thing of type `a -> b`. Indeed, `Encoder`s may be composed using
`(.)` from `Control.Category`, as `Encoder` is an instance of `Category`. An `Encoder` may describe
an individual segment such as:

```haskell
Encoder check parse a Text
```

Or a collection of routes:

```haskell
Encoder check parse a (R MyBlogRoutes)
```

#### `SegmentResult`

This type is for describing a segment within a larger `Encoder`. An `Encoder` may be built without
ever using this type, but it makes some parts easier to manage.

It has a list-like structure with a recursive component and a terminating case. These can be, but
aren't always required, to combined `Encoder`s together to describe segments of a route.

The recursive case is the `PathSegment Text (Encoder check parse a PageName)` constructor that
encodes a literal segment in the path, followed by one or more `Encoder`s.

The terminating case is the `PathEnd (Encoder check parse a (Map Text (Maybe Text)))` constructor.
It is used to indicate that there is only this single segment in that route, or to terminate a
longer parent route.

#### `BackendRoute` & `FrontendRoute`

An Obelisk application begins with two distinct sets of routes for backend and frontend. These
are defined as GADTs called `BackendRoute` and `FrontendRoute`, respectively.

For more info on GADTs check out the following links:
* [Haskell Wiki](https://wiki.haskell.org/Generalised_algebraic_datatype)
* [What I Wish I Knew When Learning Haskell - GADTs](http://dev.stephendiehl.com/hask/#gadts)
* [Haskellforall](http://www.haskellforall.com/2012/06/gadts.html)

In a freshly generated Obelisk application these live in the `Common.Route` module.

`BackendRoute` is for serving static things or deferring to other backend endpoints. Such as:
- "static/files/img/:imgName:"
- "api/..." where the "..." portion is managed by a REST endpoint package of your choice.

These backend routes can run their own code or hand off to a different request handling package,
such as `servant`, for a finer grained set of REST endpoints. Should none of them match the
frontend of the application is served and the frontend router will continue parsing the route.

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

Obelisk can also provide a local [Hoogle](https://hoogle.haskell.org) server that lets you browse
and search the types and documentation for all of the dependencies in your entire Obelisk
application. To start the Hoogle server, in a spare terminal run the following command from the root
of your Obelisk application:

```shell
$ ob hoogle
```

You can then access your local Hoogle from your web browser at `http://localhost:8080`, or by
instructing an editor plugin to use that address.

### Defining our route as a type

To create the route itself, we will add a constructor to the `FrontendRoute` GADT in `Common.Route`.

The purpose of this constructor is to give a name and a value to a particular route. It provides a
constructor to match on when we're deciding what code to run. It will also be used to help build
type-safe links within our application, as well as declare what types are expected from the route.

Recall that our example route is `/user/:someUserId:` where the `:someUserId:` will be provided to
our application. We don't want to use a bare `Int` because we're responsible Haskellers. So we will
create a `newtype` to wrap it up. Add the following `newtype` to the `Common.Api` module:

```haskell
newtype UserId = UserId { unUserId :: Int }
```

We will also leverage the `Wrapped` typeclass from `lens` to simplify the use of this type in our
application. We won't have to implement this ourselves, as GHC is more than capable of writing this
code for us using Template Haskell. Import the `makeWrapped` function from `Control.Lens` and add
the following line immediately after your `UserId` definition:

```haskell
makeWrapped ''UserId
```

Before this builds you will need to turn on the following language extensions for GHC. These are
required by the code that the Template Haskell produces and ensure that the types are enforced
correctly for your `UserId` type. The extension `{-# LANGUAGE TemplateHaskell #-}` is required to
allow the code to generated at all. Add the following at the top of the file, above the module
definition:

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
```

For more information on Haskell language extensions, here are some excellent resources:
- [TypeClasses - Introduction to GHC language extensions](https://typeclasses.com/extensions-intro)
- [What I Wish I Knew When Learning Haskell - Language Extensions](http://dev.stephendiehl.com/hask/#language-extensions)

Part of what this provides is a generalised `Prism` from `lens` called `_Wrapped` that can act as a
generalised constructor or de-constructor. More interesting to us however is the instances and
associated types that are created by the template Haskell. We will see these in use shortly.

Now that we have something more useful to represent our `Int`, we can define the constructor for our
new route. Add the following constructor to `FrontendRoute` in `Common.Route`:

```haskell
  FrontendRoute_User :: FrontendRoute UserId
```

This constructor indicates that we have a `FrontendRoute` that when successfully loaded, will
provide the current `UserId` input on that route.

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

If you're following along with `ob run` then after you added the new constructor and saved the
file. New incompleteness warnings will have appeared in the output of `ob run`. This is GHC and the
type system helping us out by pointing out the next steps for us now that we've extended our
`FrontendRoutes` type.

The next step is to fill in the definition of our new route. The primary function that contains
these definitions is the `fullRouteEncoder` function, located in `Common.Route`.

Add our new constructor as another pattern in the `case` expression for the frontend routes. For now
add a type-hole on the right hand side of our new `case` expression and save the file. The `ob run`
output should update and tell you the type of the expression that it is expecting:

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
      FrontendRoute_User -> _todo
  )
```

The output will be:

```shell
    • Found hole:
        _todo :: SegmentResult (Either Text) (Either Text) UserId
      Or perhaps ‘_todo’ is mis-spelled, or not in scope
```

This is telling us that we have to construct a `SegmentResult` to be paired with our
`FrontendRoute_User` constructor to form part of the complete `Encoder` for `FrontendRoutes`.

Looking back at the `SegmentResult` type, it has two constructors. The one that we need is the
`PathSegment` constructor:

```haskell
  | PathSegment Text (Encoder check parse a PageName)
```

This is because our route definition has multiple parts, one of which is static. Use the literal
value "user" as the first argument to the constructor, leaving a typed hole as the second
argument. The `case` expression should now look like this:

```haskell
  FrontendRoute_User -> PathSegment "user" _todo
```

The output from `ob run` will update and look something like this:

```shell
    • Found hole:
        _todo :: Encoder (Either Text) (Either Text) UserId PageName
      Or perhaps ‘_todo’ is mis-spelled, or not in scope
```

Remembering that we can squint a bit and view an `Encoder` as a function from `a -> b`, this makes
the next typed hold that we have to fill, have a shape similar to `UserId -> PageName`. Our next task
will be to build that `Encoder` and fill that hole.

#### Wait a minute, shouldn't I be matching on the route on the left?

In many routing systems you match directly on the route input and break it down as required. This
process can be fragile and has limitations. Additionally you are left on your own when comes to
_creating_ links in your application. Because there is no way to relate the structure of a route to
anything, you have to manually build up routes again. If any of those routes change it can be a
tedious and error-prone process to find and fix all the constructed links.

Obelisk routes are bidirectional, which means the `Encoder` that you create works as both a 'pattern
match' for incoming routes. As well as using the GADT constructors as a type safe mechanism for
_creating_ links in your application. It's a compile error to try to use route constructors that
don't exist, and if you change the type of a route the application will not build until you fix that
change every where it appears.

----

### Completing our first encoder

Within the `Obelisk.Route` module are many pre-built `Encoders`. There are two that are of interest
to us, we will look at their types first so we can think about how we will use them.

The first is:

```haskell
unwrappedEncoder
  :: ( Wrapped a
     , Applicative check
     , Applicative parse
     )
  => Encoder check parse a (Unwrapped a)
```

The `Wrapped` typeclass constraint and the associated type`(Unwrapped a)` ensure that regardless of
which type our `a` is when it is wrapped, the type of `Unwrapped a` is the correct wrapped type. In
our case:

```haskell
(UserId 42) :: Wrapped UserId
--
42 :: Unwrapped UserId ~ Int
```

The second function is:

```haskell
readShowEncoder
  :: ( MonadError Text parse
     , Read a
     , Show a
     , Applicative check
     )
  => Encoder check parse a PageName
```

This `Encoder` is capable of dealing with any type that has instances of `Read` and `Show`. However
it is only a legal `Encoder` is these instances are inverses of one another. That is if they satisfy
this test:

```haskell
forall t. read (show t) === t
```

Recall that our typed hole tells us we need to construct a value of the following type:

```haskell
_todo :: Encoder (Either Text) (Either Text) UserId PageName
```

#### Aside `PageName`

This type is a combination of the URL and a query string and is the target type of a _complete_
route `Encoder`. Individual `Encoder`s will work with different types, but often the final type of a
chain of `Encoder`s will be a `PageName`. However it is unlikely you will ever need to construct
this type yourself as `obelisk-route` will do that for you.

----

We can safely simplify these types a bit for the sake of understanding:

```haskell
unwrappedEncoder :: Wrapped a => a -> (Unwrapped a)
-- &
readShowEncoder :: (Read a, Show a) => a -> PageName
```

To simplify things, let's specialise the type of `unwrappedEncoder` to our `UserId` type. We know we
have a `Wrapped` instance `UserId`, so what we're doing is replacing the `a` with `UserId`. We also
know that the unwrapped type of a `UserId` is `Int`, so `Unwrapped UserId` becomes `Int`:

```haskell
unwrappedEncoder :: UserId -> Int
```

This `Encoder` does the simplest useful thing while being obviously correct. Courtesy of the
`Wrapped` typeclass this is also generalised so that we don't even have to write the code that wraps
and unwraps our `UserId` to its inner type.

Update our `case` expression and compose `unwrappedEncoder` with a typed hole and save the file. The
`ob run` will update and tell us the next step.

#### Aside: a more general composition

The composition operator we need is the more general version from the `Control.Category`
module. This is because the one from `Prelude` is specialised to the function arrow type `(->)`,
whereas the one in `Control.Category` uses the `Category` typeclass:

```haskell
-- (.) from Prelude
(.) :: (b -> c) -> (a -> b) -> a -> c

-- (.) from Control.Category
(.) :: Category cat => cat b c -> cat a b -> cat a c
```

The `Encoder` type is an instance of `Category` so we need to use the more general function. Ensure
that `(.)` is imported correctly at the top of `Common.Route`:

 ```haskell
import Prelude hiding ((.))
import Control.Category
```

----

Our updated `case` branch with `unwrappedEncoder` composed with our typed hole `_todo`:

```haskell
  FrontendRoute_User -> PathSegment "user" $ _todo . unwrappedEncoder
```

With the file saved, the `ob run` output will tell us the shape of typed hole:

```shell
    • Found hole:
        _todo :: Encoder (Either Text) (Either Text) Int PageName
```

The last hole to fill is now an `Encoder` from `Int -> PageName`. Conveniently `Int` has an instance
of both `Read` and `Show`, satisfying both constraints of the `readShowEncoder` becoming:

```haskell
readShowEncoder :: Int -> PageName
```

Which is exactly what we wanted! So we're able to replace our typed hole `_todo` with
`readShowEncoder` and we've created our first complete route `Encoder`. Yay. Our `case` expression
should now look like this:

```haskell
  FrontendRoute_User -> PathSegment "user" $ readShowEncoder . unwrappedEncoder
```

It should be a rare event that you will need to write your own `Encoder` function like
`unwrappedEncoder` directly. The provided `Encoder`s in `Obelisk.Route` are designed to be composed
and combined to build more complicated encoders and should be sufficient for most requirements. If
you find this is not the case and there is a pattern that keeps appearing and is not accounted for,
then that is probably a bug and we'd like to know about it!

As this continues we will demonstrate more complicated routes and how to define them. We will
discover more of the functionality provided within `obelisk-route`. But for now we need to integrate
our routes into the application and see how to make decisions and access the values from the route.

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

Using the `askRoute` function from the `Obelisk.Route.Frontend` module we can access a `Dynamic` of
our `UserId`. This is because the type that we had in our `FrontendRoute` GADT type is stored in a
`ReaderT` that changes based on which route we're currently matching on.

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
        text $ "This user : " <> T.pack (show uidVal)

    FrontendRoute_Main -> do
      ...
```

### Building links

We have separate content based on the route, but we don't have any links yet!

With the `routeLink` function, also from the `Obelisk.Route.Frontend` module, we can create type
safe links to different parts of our application. We will first link to the user page from the main
page. Somewhere in the body of the `FrontendRoute_Main` content, add the following:

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
