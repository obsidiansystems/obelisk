# Obelisk Routing

## Motivation

The `obelisk-route` package is designed to help with managing the paths and parameters for routing
in your application.

Most importantly it has been designed and built to provide the following guarantees:

* **Every route value can be encoded to a URL**

Every route that you declare in your types will produce a valid URL; that is, encoding cannot fail.

* **When you decode something you've encoded, you'll always get the same value back**

```haskell
decode . encode = pure
```

Routes declared using `obelisk-route` are bidirectional, meaning that any route that can be encoded
to types and matched on can also be encoded as a url *without losing information*.

* **Common modifications made to routes, where ever possible, will be caught by the compiler.**

When you make common modifications to routes, or a route is added or removed, there will be
"incomplete case" and similar warnings wherever you need to update your application. The type
system also makes it *much* easier to catch those easy-to-forget issues, such as updating nav-bars,
or that breadcrumb widget that needs a human-readable description.

* **Behaviour of routes is identical for Backend and Frontend**

This package doesn't have a distinction between backend or frontend routes, these are only different
types so their behaviour is identical. This guarantees that rendering completed during backend
hydration will be indistinguishable from rendering completed on the frontend.

It may seem odd that `obelisk-route` does not provide a way to specify the HTTP Method for a given
route. This is due to the requirement that this package may be used when rendering a page on the
backend **or** the frontend. A POST request makes no sense for the frontend of an application so we
do not support creating this type of route. As it would be difficult to impossible to enforce if
the routing system worked differently on the backend vs the frontend.

## The way of the Obelisk Route

The way to approach building routes with `obelisk-route` is to focus on your Route data structure.

First, make sure it matches your application's structure: with one Route value for each logical
page the user might want to visit.  Only once that is clear, move on to thinking about how those
logical pages ought to appear in the user's address bar.  For instance, you might want each user's
profile to show up at `/user/$userid`.

Finally, consider how you want to handle URLs with slight differences; an extra trailing slash
different capitalization, or URLs whose rendering format has changed. By doing things in this
order, you design your routes around your application, rather than designing your application
around your routes.

We then write the definition of the route using `Encoder`s. These may be thought of as small pure
functions that _compose_ together using `(.)` to build the concrete definition of your routing
structure. By composing and building routes from pieces that are so small that they are "obviously
correct", you can be confident that the composition of those individual pieces is also correct.

As we build up some examples in this guide, we will demonstrate how to think about your abstract
routes as concrete definitions using `Encoder`s as pure functions. You will see that `obelisk-route`
already has most of the tools you need to construct that concrete definition. We'll also cover what
to do when it doesn't, and what you need to know to ensure that you build correct and composable
pieces.

#### Obelisk live development environment

For best results, work through this document using a freshly created Obelisk application. Refer
to the Obelisk documentation for [Developing an Obelisk project](https://github.com/obsidiansystems/obelisk#developing-an-obelisk-project).

## The first route type

The first route we will create will be for the main landing page of our application. There is only
one possible route associated with this page, so the value for our route will have only one possible
instantiation.

When it comes to how that route will present itself to the user in the address bar, it will be our
main page and at the root of all things so it must be `/`.

The routes for new a Obelisk application are in the `Common.Route` module. We will follow this
convention and create our type there:

```haskell
data MyRoute = MyRoute_Main
  deriving (Show, Eq, Ord, Generic)
```

The constructor for our main page is `MyRoute_Main` and no parameters because there is only one
way to create this value. This value will be used in our `case` expressions to decide what code
to run, and when we're creating links to this page. Also add the deriving clause as we will need
these instances as our routes grow.

> The naming of the route MyRoute_Main is an Obelisk convention of including the type
> name in individual constructor names. This helps disambiguate the code at the 'cost' of a few
> extra keystrokes. As an example:
>
> ```haskell
> data Foo
>   = Foo_ConstructorA
>   | Foo_ConstructorB
> ```

## First concrete route definition

We have the logical definition of our route in the `MyRoute_Main` constructor, and the next step
is to create the concrete definition of that route for our application. We will do this by
building our first `Encoder`. Below the `MyRoute` type, create a function with the following type
signature:

```haskell
myRouteEncoder
  :: ( Applicative check
     , MonadError Text parse
     )
  => Encoder check parse () PageName
myRouteEncoder =
  undefined
```

You will need the constraints to satisfy the typechecker for `check` and `parse`, but they're not
important to what we're doing at the moment.

#### The `Encoder` type

This is the primary building block for any route definition:

```haskell
-- The 'check' and 'parse' type variables will be covered later
data Encoder check parse a b
```

You can think of an `Encoder check parse a b` as a thing of type `a -> b`. Indeed, `Encoder`s may be
treated as you would any pure function, including composing them using `(.)`. Because `Encoder` has
an instance of `Category`.

However the `(.)` operator from `Prelude` is specialised to the function arrow type `(->)` so we
must import the more general version from the `Control.Category` module, because it uses the
`Category` typeclass constraint:

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

If you use the `(.)` from `Prelude` to compose `Encoder`s then you will end up with type errors
similar to the following:

```shell
    • Couldn't match expected type ‘b0 -> c0’
                  with actual type ‘Encoder check0 parse0 [Text] PageName’
    • In the first argument of ‘(.)’, namely ‘pathOnlyEncoder’
```

You can see that it expected something of type `b0 -> c0` whereas we're wanting to compose
`Encoder`s, although we can treat `Encoder` as pure functions, the types are distinct.

----

Within the `Obelisk.Route` module are many pre-built `Encoders`, for our purposes we need to find
one that can be used to satisfy the `() -> PageName` shaped hole and ensure the route structure
we require. The `Encoder` that meets this requirement is the `unitEncoder`, which has the
following type:

```haskell
-- Constraints elided for brevity
unitEncoder :: (...) => r -> Encoder check parse () r
```

The `()` is equivalent to our current `MyRoute` type, because there is only possible way to
construct a value of type `MyRoute`. Also when this route is encoded as a URL there should be
only one possible output.

Update `myRouteEncoder` to use this `Encoder` and place a 'typed hole' as the argument:

```haskell
myRouteEncoder
  :: ( Applicative check
     , MonadError Text parse
     )
  => Encoder check parse () PageName
myRouteEncoder =
  unitEncoder
```

We need to provide an input to `unitEncoder` and that input has of type `PageName`. The
`PageName` type is a combination of the URL and a query string and is the target type of a
_complete_ route `Encoder`. Individual `Encoder`s will work with different types, but often the
final type of a chain of `Encoder`s will be a `PageName`. It is unlikely you will ever need to
construct this type yourself as `obelisk-route` has functions that will handle that for you.

For our purposes, the `PageName` needs to be completely empty as our route `MyRoute_Main`
corresponds to the `/` URL. To create an empty `PageName` we can lean on `Monoid` and use
`mempty`. The `Encoder` definition now looks like:

```haskell
myRouteEncoder
  :: ( Applicative check
     , MonadError Text parse
     )
  => Encoder check parse () PageName
myRouteEncoder =
  unitEncoder mempty
```

Note that our type `MyRoute` does not appear in this signature, this is because we have only one
possible way to construct a value of type `MyRoute` and only one possible URL. Until there are
more routes the `()` or unit type is equivalent to our `MyRoute` type. When we add more routes
then this will change accordingly.

## Adding a 404 page

Next we're going to add a 404 page to our list of routes. This route will be similar to our main
route in that there will be only one possible instantiation of this route. The representation in
the address bar will of course be different. But similarily there will only be one possible
route, in this case the expected route will be `/missing`.

To represent this we will extend the `MyRoute` type with another constructor:

```haskell
data MyRoute
  = MyRoute_Main
  | MyRoute_Missing
  deriving (Show, Eq, Ord, Generic)
```

Now that we have the logical definition of our 404 route we will extend our `Encoder` to account
for the new constructor. Note that now we have more than one constructor for our `MyRoute` type
so the type signature for our `Encoder` has to change as `()` is no longer correct.

The `Encoder` type is now:

```haskell
myRouteEncoder
  :: ( Applicative check
     , MonadError Text parse
     , MonadError Text check
     )
  => Encoder check parse MyRoute PageName
myRouteEncoder =
```

The `Encoder` must now distinguish between routes whilst maintaining all of the required
guarantees. We will use the `enumEncoder` to help us build our `Encoder`. Lets have a look at its
type:

```haskell
enumEncoder
  :: forall parse check p r
  . ( Universe p
    ... -- some constraints elided
    )
  => (p -> r)
  -> Encoder check parse p r
```

Whereas the `unitEncoder` had a static value input for the `r` value, the `enumEncoder` needs a
function: `p -> r` to decide how to encode the different values of the enumeration. To further
explain this, replace `unitEncoder` with `enumEncoder` and `mempty` with a 'typed hole': `_todo`,
then save the file.

### Aside: Typed holes

That `_todo` is called a 'type hole' and they allow you ask GHC "what should be the type of the
thing at `_todo`". They are very useful for debugging and incremental development. They may be used
any where you can use a function or a value. For more information on type holes, we recommend the following:

- [What I Wish I Knew When Learning Haskell - Type Holes](http://dev.stephendiehl.com/hask/#type-holes)
- [GHC Manual - Typed Holes](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/typed-holes.html)

These are a _very_ useful tool for building & debugging your program. But do not succumb to 'type
tetris'. This is when you find yourself replacing typed holes with the first thing that satisfies
the type checker, but doesn't necessarily result in a useful or correct program.

----

If your following along with either `ob run` or [`ghcid`](https://github.com/ndmitchell/ghcid)
the output will now contain two errors. We will deal with them in turn:

The first is related to the `Universe` constraint:

```shell
    • Could not deduce (Universe MyRoute)
        arising from a use of ‘enumEncoder’
      from the context: (Applicative check, MonadError Text parse)
        bound by the type signature for:
        ...
```

The `enumEncoder` uses the `Universe` instance to ensure coverage over every possible value for
type `p`. We'll leverage `Generic`s to make GHC do this for us. Add the following instance of
`Universe` for `MyRoute`:

```haskell
instance Universe MyRoute where
  universe = universeGeneric
```

Save the file and the next error is feedback from the 'typed hole':

```shell
    • Found hole: _todo :: MyRoute -> PageName
      Or perhaps ‘_todo’ is mis-spelled, or not in scope
    • In the first argument of ‘enumEncoder’, namely ‘_todo’
      In the expression: enumEncoder _todo
```

This is the specialised type for the `p -> r` function for `enumEncoder`. We have to write the function that describes how to create the `PageName` for each value of our sum type. Remove the typed hole and replace it with a function that contains a case expression:

```haskell
myRouteEncoder
  :: ( Applicative check
     , MonadError Text parse
     , MonadError Text check
     )
  => Encoder check parse MyRoute PageName
myRouteEncoder = enumEncoder $ \myRoute -> case myRoute of
```

A slightly nicer way of writing this is to turn on the [`LambdaCase`](http://dev.stephendiehl.com/hask/#lambdacase) [language extension](http://dev.stephendiehl.com/hask/#language-extensions). Which is exactly the same as what we have but tidy:

```haskell
myRouteEncoder = enumEncoder $ \case
  MyRoute_Main -> _mainTodo
  MyRoute_Missing -> _missingTodo
```

Now to fill in the missing pieces. The `PageName` for the main page is the same default value we
provided before: `mempty` as it has no path and no query parameters. The path for the
`MyRoute_Missing` page will be placed at `/missing` so as not to overlap with the main page. To
do that we construct the corresponding `PageName` value. Where `PageName` is a tuple of the list
of path segements (`[Text]`) and a map of the query parameters (`Map Text Text`):

```haskell
myRouteEncoder = enumEncoder $ \case
  MyRoute_Main -> mempty
  MyRoute_Missing -> (["missing"], mempty)
```

The missing page has only a single path segment and no query parameters so we create a singleton
list and an empty map. Now we have a concrete definition of both of the `MyRoute` routes.

Were we to add another route to our data `MyRoute` data structure, a contact page for example.
After we added the constructor to the `MyRoute` type the compiler would provide a non-exhaustive
pattern match warning until we updated this `case` expression to handle the new route.

The warning would be similar to this:

```shell
common/src/Common/Route.hs:(77,36)-(79,42): warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative: Patterns not matched: MyRoute_Contact
   |
77 | myRouteMainEncoder = enumEncoder $ \case
   |                                    ^^^^^...
```

## Nested Routes

Applications often have a hierarchy to keep functionality organised. You are able to identify a
request as belonging to a particular category and hand it off accordingly, the thing you've handed
the request to is then responsible for handling the finer details. The end result being that the
whole system may be quite complex but the individual components are easier to create, maintain, and
more obviously correct due to their constrained scope.

This package lets you create modular and nested routes that enable them to match the hierarchy of
your application rather than dictating it. We will add two sets of nested routes; one to represent
sending routes to a backend API, and the other to represent the frontend of our application.

These routes will be similar to the routes we've already created to avoid overloading on new
concepts. Each set of routes will have one single possible page per route. Add the new routes above
the `MyRoute` type in the `Common.Route` module:

The backend routes:

```haskell
data ApiRoute :: * -> * where
  ApiRoute_Status :: ApiRoute () -- what is the current status of our api.
  ApiRoute_Version :: ApiRoute () -- what is the current running version of our api.
  ApiRoute_Uptime :: ApiRoute () -- how long has our api been up and running.
```

The frontend routes:

```haskell
data AppRoute :: * -> * where
  AppRoute_Register :: AppRoute () -- user registration page
  AppRoute_Contact :: AppRoute () -- display of contact information
  AppRoute_About :: AppRoute () -- about us
```

You'll notice that these routes appear to be _more_ complex than the `MyRoute` type. The reason for
that is we need more flexibility in the types to be able to combine the different route
structures. These are [Generalised Algebraic Data Types](http://dev.stephendiehl.com/hask/#gadts),
or GADTs. The extra flexibility that we need is the additional type argument, indicated in the first
line of the type definition. This allows us to chain the routes together and ensure that the types
are correct.

If you've not encountered GADTs before, or you're a bit rusty, check out the following links for more information:
* [What I Wish I Knew When Learning Haskell - GADTs](http://dev.stephendiehl.com/hask/#gadts)
* [Haskellforall](http://www.haskellforall.com/2012/06/gadts.html)
* [Haskell Wiki](https://wiki.haskell.org/Generalised_algebraic_datatype)

----

Build the `Encoder` for the `ApiRoute` type, starting as always, with the type signature:

```haskell
apiRouteEncoder
  :: ( MonadError Text check
     , MonadError Text parse
     )
  => Encoder check parse (R ApiRoute) PageName
```

Similar to our first `Encoder` this will contain the concrete definitions of the routes for the
`ApiRoute` type. Unlike the `MyRoute` encoder however, we must use the `R` type to wrap our route
type due to the extra type argument that definition of `ApiRoute` requires.

### Aside: The `R` type

The `R` type is an alias that wraps a set of routes, such as `ApiRoute`. When writing the GADTs for
routes, the type of the routes requires a type argument which is then fixed for each individual
constructor in that GADT.

See the first line of the definition of the `ApiRoute` type:

```haskell
data ApiRoute :: * -> * where
```

That `* -> *` type means that the type of `ApiRoute` needs to be applied to another type before GHC
considers it to be a complete type. When we define a constructor, we provide this type argument:

```haskell
  ApiRoute_Status :: ApiRoute ()
```

When we need to refer to _all_ of the routes in `ApiRoute`, we can't provide any single type
argument, because all of the routes would then be restricted to that one type.

To solve this, we use the `R` type alias to wrap the type constructor for the GADT with a [`DSum`
(dependent sum)][dsumtype]. This then defers the resolution of that type argument, and enables us to
refer to an entire set of routes in a type signature without sacrificing flexibility.

[dsumtype]: https://hackage.haskell.org/package/dependent-sum-0.7.1.0/docs/Data-Dependent-Sum.html#t:DSum

----

Because we're defining an `Encoder` for a type of `(R p)` we will use the `pathComponentEncoder`
function. This `Encoder` is purpose built for these types of route. Additionally, the type that forms part of
the interface, `SegmentResult`, allows more complex route definitions. This function works
similarly to the `enumEncoder` which we know already:

```haskell
apiRouteEncoder
  :: ( MonadError Text check
     , MonadError Text parse
     )
  => Encoder check parse (R ApiRoute) PageName
apiRouteEncoder = pathComponentEncoder $ \case
  ApiRoute_Status -> _statusTodo
  ApiRoute_Version -> _versionTodo
  ApiRoute_Uptime -> _updateTodo
```

Instead of `PageName`, we now have to return a `SegmentResult` on the right hand side of the `case` expression.

### Aside: `SegmentResult`

This type is part of the interface to the `pathComponentEncoder` function. Because routes may be
nested in the definition of other routes. All of the routes related to pull requests nested under
the route for a repository, for example. We need a way to describe whether a route may have more
segments or if we've reached the end.

To do this, the `SegmentResult` has two constructors, one recursive allowing for more structure, and
the other terminating, indicating that this path is complete. Refer to the Haddock documentation for
more detail information.

The recursive constructor has the following type:

```haskell
PathSegment Text (Encoder check parse a PageName)
```

This lets us specify a fixed component of a path, and then we must also provide an `Encoder` to turn
this value (the one on the left hand side of the case expression) into the remainder of the route.

The other constructor:

```haskell
PathEnd (Encoder check parse a (Map Text (Maybe Text)))
```

Indicates that the path is complete and requires an `Encoder` to turn this value into query parameters.

----

The value we're matching on is `ApiRoute_Status` and it is similar to the `MyRoute_Missing` route
from earlier. The constructor has no arguments so there is only one possible instantiation, and we
have a static text component, "status", to differentiate it from the `/` path.

```haskell
  ApiRoute_Status -> PathSegment "status" $ unitEncoder mempty
```

MyRoute_App AppRoutes ~ /app/{register, contact, about}
Needs: R, pathComponentEncoder, SegmentResult

Aside: This lets you treat paths modularly


#############################################################################################################
new content ends here
#############################################################################################################

#############################################################################################################
old content that is being restructured starts here
#############################################################################################################

## Simple route with one parameter

We're going to add a homepage for every user, which is it be parameterised on their user
ID. Expressed abstractly it might look something like this:

```haskell
data MyRoute
  = MyRoute_Main
  | MyRoute_UserHome Text
```

### Defining our route as a type

To create the route itself, we will add a constructor to the `FrontendRoute` GADT in `Common.Route`.

Recall that our abstract route definition was a homepage for every user, parameterised over their
user ID. Now we begin to make this into a concrete definition.

Add the following constructor to `FrontendRoute` in `Common.Route`:

```haskell
  FrontendRoute_User :: FrontendRoute Int
```

This constructor indicates that we have a `FrontendRoute` that when successfully loaded, will
provide the current user ID `Int` that was input on that route.

#### The `Encoder` type

This is the primary building block for any route definition:

```haskell
-- The 'check' and 'parse' type variables will be covered later
data Encoder check parse a b
```

You can think of an `Encoder check parse a b` as a thing of type `a -> b`. Indeed, `Encoder`s may be
treated as you would any pure function, including composing them using `(.)`. Because `Encoder` has
an instance of `Category`.

However the `(.)` operator from `Prelude` is specialised to the function arrow type `(->)` so we
must import the more general version from the `Control.Category` module, because it uses the
`Category` typeclass constraint:

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

If you use the `(.)` from `Prelude` to compose `Encoder`s then you will end up with type errors
similar to the following:

```shell
    • Couldn't match expected type ‘b0 -> c0’
                  with actual type ‘Encoder check0 parse0 [Text] PageName’
    • In the first argument of ‘(.)’, namely ‘pathOnlyEncoder’
```

You can see that it expected something of type `b0 -> c0` whereas we're wanting to compose
`Encoder`s, although we can treat `Encoder` as pure functions, the types are distinct.

----

Looking back at the type hole from GHC:


```shell
    • Found hole:
        _todo :: Encoder (Either Text) (Either Text) Int PageName
      Or perhaps ‘_todo’ is mis-spelled, or not in scope
```

Recall that we can treat an `Encoder check parse a b` as a function from `a -> b`, this makes the
next typed hold that we have to fill have a type similar to `Int -> PageName`. Our next task will
be to build that `Encoder`.

#### Wait a minute, shouldn't I be matching on the route on the left?

In many routing systems you match directly on the route input and break it down as required. This
process can be fragile and has limitations. Additionally you are left on your own when comes to
creating links in your application. Because there is no way to relate the structure of a route to
anything, you have to manually build up routes again. If any of those routes change it can be a
tedious and error-prone process to find and fix all the constructed links.

Obelisk routes are bidirectional, which means the `Encoder` that you create works as both a 'pattern
match' for incoming routes. As well as using the GADT constructors as a type safe mechanism for
_creating_ links in your application. It is a compile error to try to use route constructors that
don't exist, and if you change the type of a route the application will not build until you fix that
change every where it appears.

----

### Completing our first encoder

Within the `Obelisk.Route` module are many pre-built `Encoders`, for our purposes we need to find
one that can be used to satisfy the `Int -> PageName` shaped hole. The `unsafeShowEncoder` matches
our requirements:

```haskell
unsafeShowEncoder
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
_todo :: Encoder (Either Text) (Either Text) Int PageName
```

We can safely simplify the type of `unsafeShowEncoder` for the sake of understanding:

```haskell
unsafeShowEncoder :: (Read a, Show a) => a -> PageName
```

Conveniently `Int` has an instance of both `Read` and `Show`, but more importantly these instances
are inverses of one another. This matters because a guarantee of `obelisk-route` is that going to
and from a URL **does not lose information**. If applying `show` to a value of `Int` and then
applying `read` to the resulting `String` did not produce the exact same `Int` value that we started
with, our `Encoder` would not be valid as it could not satisfy that guarantee.

Replacing the constraints with our type, `unsafeShowEncoder` becomes:

```haskell
unsafeShowEncoder :: Int -> PageName
```

This is the type what we wanted! So we're able to replace our typed hole `_todo` with
`unsafeShowEncoder` and we've created our first complete route `Encoder`. Yay. This `Encoder` does
the simplest useful thing while being obviously correct.

Our `case` expression should now look like this:

```haskell
  FrontendRoute_User -> PathSegment "user" unsafeShowEncoder
```

Once you've updated our `case` expression to replace the typed hole with this `Encoder`, save the
file. The `ob run` output will update and tell us the next step.

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

We're going to make the rest of these changes in the main `Frontend.hs` module in the `frontend`
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

We'll modify the function at `_frontend_body` to show different content for the main or user page,
and then link between them.

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
the `Int` that represents the user ID. This is because the type that we had in our `FrontendRoute`
GADT type is stored in a `ReaderT` that changes based on which route we're currently matching on.

Were we to use `askRoute` in the `Frontend_Main` branch of our `case` expression, the resulting
`Dynamic` would contain a value of unit `()` because that is the type of the `Frontend_Main` route.

Update the `FrontendRoute_User` branch of our `case` expression to the following:

```haskell
  , _frontend_body = subRoute_ $ \case
    FrontendRoute_User -> do
      text "We're on the user page! But which user?"
      dUserId <- askRoute
      dyn_ $ ffor dUserId $ \uidVal ->
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
routeLink (FrontendRoute_User :/ 42) $
  text "Visit the page of User # 42"
```

We define the link we want by using the constructor from our `FrontendRoute` type and using the
function `(:/)` from `Obelisk.Route`, and we provide the required input of an `Int` to satisfy the
type of `FrontendRoute_User`. Then we create a child widget with some content that when clicked will
take us to the user page.

Similarly, we can create a link on the user page to take us to the main page:

```haskell
routeLink (FrontendRoute_Main :/ ()) $
  text "To the main page!"
```

You should now have an Obelisk application with some rudimentary routing and enough of a start to
begin building your application with type safe routes! Hooray!!
