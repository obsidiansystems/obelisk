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
types so their behaviour is identical. This guarantees that routes generated as part of a backend
static rendering process will be indistinguishable from routes generated on the frontend.

It may seem odd that `obelisk-route` does not provide a way to specify the HTTP Method for a given
route. This is due to the requirement that this package may be used when rendering a page on the
backend **or** the frontend. A POST request makes no sense for the frontend of an application so we
do not support creating this type of route. As it would be difficult to impossible to enforce if
the routing system worked differently on the backend vs the frontend.

## Obelisk route mental model

The way to approach building routes with `obelisk-route` is to focus on your Route data structure.

First, make sure it matches your application's structure: with one Route value for each logical
page the user might want to visit.  Only once that is clear, move on to thinking about how those
logical pages ought to appear in the user's address bar.  For instance, you might want each user's
profile to show up at `/user/$userid`.

Finally, consider how you want to handle URLs with slight differences; an extra trailing slash,
different capitalization, or URLs whose rendering format has changed. By doing things in this order,
you design your routes around your application, rather than designing your application around your
routes.

We then write the definition of the route using `Encoder`s. These may be thought of as small pure
functions that _compose_ together using `(.)` to build the concrete definition of your routing
structure. By composing and building routes from pieces that are so small that they are "obviously
correct", you can be confident that the composition of those individual pieces is also correct.

As we build up some examples in this guide, we will demonstrate how to think about your abstract
routes as concrete definitions using `Encoder`s. You will see that `obelisk-route` already has most
of the tools you need to construct that concrete definition. We'll also cover what to do when it
doesn't, and what you need to know to ensure that you build correct and composable pieces.

#### Obelisk live development environment

For a hands-on experience, work through the section you're interested in using a freshly created Obelisk application. Refer
to the Obelisk documentation for [Developing an Obelisk project](https://github.com/obsidiansystems/obelisk#developing-an-obelisk-project).

## Your route as a type

This route will be the main landing page of our application. There is only one possible route
associated with this page, so the value for our route must have only one possible instantiation.

When it comes to how that route will present itself to the user in the address bar, it will be our
main page and at the root of all things so it must be `/`.

The routes for a Obelisk application often live in `Common.Route` and nearby modules. We will follow
this convention and create our type there:

```haskell
data MyRoute = MyRoute_Main
  deriving (Show, Eq, Ord, Enum, Bounded)
```

The constructor for our main page is `MyRoute_Main` with no parameters. This value will be used in
our `case` expressions to decide what code to run and when we're creating links to this page. Be
sure to include the deriving clause as we will need these instances.

> The naming of the route MyRoute_Main is an Obelisk convention of including the type name in
> individual constructor names. This helps disambiguate the code at the 'cost' of a few extra
> keystrokes. As an example:
>
> ```haskell
> data Foo
>   = Foo_ConstructorA
>   | Foo_ConstructorB
> ```

We have the logical definition of the route as the `MyRoute_Main` constructor, and the next step is
to create the concrete definition of that route for our application. We will do this by building our
first `Encoder`. Below the `MyRoute` type, create a function with the following type signature:

```haskell
myRouteEncoder
  :: ( Applicative check
     , MonadError Text parse
     )
  => Encoder check parse MyRoute PageName
myRouteEncoder =
  undefined
```

You will need the constraints to satisfy the typechecker for the type variables; `check`, and
`parse`. They're not important to what we're doing now.

#### The `Encoder` type

This is the primary building block for any route definition:

```haskell
data Encoder check parse a b
```

An `Encoder check parse a b` may be thought of as small pure functions of type: `a -> b`. Thinking
of them like this may provide some intuition as `Encoder`s may be composed using `(.)`, and in some
ways treated as if they were nothing but pure functions. In practice they are more complex than
this, but it is a good place to start.

The `check` and `parse` types are related to the functions that the `Encoder` uses to encode or
validate route input. When using existing `Encoder`s these type variables are often able to be
ignored. They are more relevant when you're building your own `Encoder`s.

They are also bidirectional so the one `Encoder` will handle going from `a` to `b` and
back. Importantly, a correctly built `Encoder` will not lose information, regardless of which
direction the information is travelling:

```haskell
forall a. decode (encode a) == pure a
```

----

Within the `Obelisk.Route` module are many pre-built `Encoder`s, the one we will use is the `enumEncoder`:

```haskell
enumEncoder
  :: forall parse check p r.
     ( Universe p
     , Show p
     , Ord p
     , Ord r
     , MonadError Text parse
     , MonadError Text check
     , Show r
     )
  => (p -> r)
  -> Encoder check parse p r
```

Update `myRouteEncoder` to use this `Encoder` and place a 'typed hole' as the argument:

```haskell
myRouteEncoder
  :: ( Applicative check
     , MonadError Text parse
     , MonadError Text check
     )
  => Encoder check parse MyRoute PageName
myRouteEncoder = enumEncoder _todo
```
### Aside: Typed holes

That `_todo` is called a 'typed hole' and they allow you ask GHC "what should be the type of the
thing at `_todo`". They are very useful for debugging and incremental development. They may be used
anywhere you can use a function or a value. For more information on typed holes, see the following links:

- [What I Wish I Knew When Learning Haskell - Type Holes](http://dev.stephendiehl.com/hask/#type-holes)
- [GHC Manual - Typed Holes](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/typed-holes.html)

They are a _very_ useful tool for building & debugging your program. But do not succumb to 'type
tetris'. This is when you find yourself replacing typed holes with the first thing that satisfies
the type checker, but doesn't necessarily result in a useful or correct program.

Typed holes don't have to be called `_todo`, you can give them any valid Haskell identifier name, as
long as they start with `_` and don't clash with anything other names.

**NB:** Typed holes still rely on GHC type checking and inspecting your codebase. So if you've a large
codebase then resolving the typed hole may be slow.

----

If you're following along with either `ob run`, [`ghcid`](https://github.com/ndmitchell/ghcid), or
building as you go, the output will now contain two errors relating to our use of `enumEncoder`.

The first is related to the `Universe` constraint from the [`universe` package](https://hackage.haskell.org/package/universe):

```shell
    • Could not deduce (Universe MyRoute)
        arising from a use of ‘enumEncoder’
      from the context: (Applicative check, MonadError Text parse)
        bound by the type signature for:
        ...
```

The `enumEncoder` uses the `Universe` typeclass to ensure coverage for every possible value for type
`p`. We need an instance of this typeclass for our `MyRoute` type in order to use `enumEncoder`, we
don't have to do much as there is a default implementation for any type that has instances for
`Enum` and `Bounded`, which we have already derived.

You may need to import the `Data.Universe` module if you haven't already.

Then declare an instance for our type like so:

```haskell
instance Universe MyRoute
```

Save the file and the next error is feedback from the 'typed hole':

```shell
    • Found hole: _todo :: MyRoute -> PageName
      Or perhaps ‘_todo’ is mis-spelled, or not in scope
    • In the first argument of ‘enumEncoder’, namely ‘_todo’
      In the expression: enumEncoder _todo
```

This is the specialised type for the `p -> r` function for `enumEncoder`. We have to write the
function that describes how to create the `PageName` for each value of our sum type. We might only have a
single constructor at the moment but replace the `_todo` typed hole with a `case` expression to
match on the `MyRoute` input, leaving a typed hole on the right hand side of the `->`:

```haskell
myRouteEncoder
  :: ( Applicative check
     , MonadError Text parse
     , MonadError Text check
     )
  => Encoder check parse MyRoute PageName
myRouteEncoder = enumEncoder $ \myRoute -> case myRoute of
  MyRoute_Main -> _todo
```

A slightly nicer way of writing a `case` expression when you're matching on a single input to a
lambda is to turn on the [`LambdaCase`](http://dev.stephendiehl.com/hask/#lambdacase) [language
extension](http://dev.stephendiehl.com/hask/#language-extensions). This will work exactly the same
as a normal case expression, but it is a bit tidier:

```haskell
myRouteEncoder
  :: ( Applicative check
     , MonadError Text parse
     , MonadError Text check
     )
  => Encoder check parse MyRoute PageName
myRouteEncoder = enumEncoder $ \case
  MyRoute_Main -> _todo
```

This new typed hole will inform us that the return type of this `case` expression needs to be
something of type `PageName`.

The `PageName` type is an alias for a tuple of URL and any query parameters:

- The URL as a list of the individual route segments: `[Text]`.
- Query parameters represented using `Map Text (Maybe Text)`.

It is the target type of a _complete_ route `Encoder`.

The URL of our main pain is `/` which is represented by an empty list and because we don't have any
query parameters, that will be an empty map.

Fill in the typed hole with an empty `PageName` tuple:

```haskell
myRouteEncoder
  :: ( Applicative check
     , MonadError Text parse
     , MonadError Text check
     )
  => Encoder check parse MyRoute PageName
myRouteEncoder = enumEncoder $ \case
  MyRoute_Main -> ([], mempty)
```

NB: `Map` has an instance of `Monoid`, so we can use `mempty` to create an empty `Map` for us.

We now have an `Encoder` that represents the concrete definition of our route structure. The single
page with only one possible constructor `MyRoute_Main` and it's representation in the address bar:
`([], mempty)`.

## Adding another page

Next we're going to add a privacy policy page to our list of routes. This route will be similar to
our main route in that there will be only one possible instantiation of this route. The
representation in the address bar will be different, in this case the expected route will be
`/privacy`.

To represent this we will extend the `MyRoute` type with another constructor:

```haskell
data MyRoute
  = MyRoute_Main
  | MyRoute_PrivacyPolicy
  deriving (Show, Eq, Ord, Enum, Bounded)
```

Because we have added another route and we're using a `case` expression in the `enumEncoder`, the
compiler will start displaying warnings about 'non-exhaustive' pattern matches. These may even be
regarded as errors depending on your compiler options. The compiler output will look a bit like this:

```shell
common/src/Common/Route.hs:(77,36)-(79,42): warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative: Patterns not matched: MyRoute_PrivacyPolicy
   |
77 | myRouteEncoder = enumEncoder $ \case
   |                                ^^^^^...
```

With the logical definition of our privacy policy route created, we will alter our `Encoder` to
account for the new constructor. We will add to the `case` expression to match on the different
constructors and let us the define the correct `PageName` for the new page:

```haskell
myRouteEncoder = enumEncoder $ \case
  MyRoute_Main -> ([], mempty)
  MyRoute_PrivacyPolicy -> _privacyTodo
```

The path for the `MyRoute_PrivacyPolicy` page is `/privacy`. To construct the corresponding
`PageName` value we use a single element list as the path has only a single segment: `"privacy"`.

There are still no query parameters so that part of the `PageName` is still an empty `Map`:

```haskell
myRouteEncoder = enumEncoder $ \case
  MyRoute_Main -> ([], mempty)
  MyRoute_PrivacyPolicy -> (["privacy"], mempty)
```

#### Wait a minute, shouldn't I be matching on the route on the left?

In many routing systems you match directly on the route input and break it down as required. However
this often means that you are left on your own when comes to creating links for these routes because
there is no way to relate the structure of a route to anything. If any of those routes change it can
be a tedious and error-prone process to find and fix all the constructed links.

Obelisk routes are bidirectional, which means the `Encoder` that you create also works as a 'pattern
match' for incoming routes. The route types operate as a type safe mechanism for _creating_ links in
your application. It is a compile error to try to use route constructors that don't exist, and if
you change the type of a route the application will not build until you fix that change every where
it appears.

----

## Nested Routes

Applications often have a hierarchy to keep things organised. A request is identified as belonging
to a particular sub-category and handed off accordingly, the sub-category is then responsible for
handling the finer details. The end result being that the whole system may be quite complex but the
individual components are easier to create, maintain, and more obviously correct due to their
constrained scope.

This section will have a top level `Encoder` with two sets of nested routes; one for a backend API,
and another for the frontend of our application. The nested routes will be kept simple as the focus
is on how to nest them. This hypothetical system is comprised of two subsystems, with the following
functionality:

The backend offers three different functions:

- check the status of the system
- request the current version of the system
- request the current uptime duration

The frontend has three pages:

- user registration page
- 'contact us' page
- 'about us' page

Lets think about what the abstract definition of these routes might look like. We have six unique
bits of functionality that we want to account for, so we could have a single structure that has
everything in one big list. But that doesn't reflect that there are two logically separate systems,
and the routes must conform to our needs, not the other way around.

To anyone using our system however there is no such separation, so we need a way to decide how to
delegate properly. For that we need another top level structure that is able to decide if something
is a backend or frontend request. This top level shouldn't do anything but decide which system to
forward the request to, but in doing so it would present the appearance of this 'single' system
_and_ allow us to maintain the logical distinction and keep things organised.

So we know that at a high level we need the following abstract route structures:

- `ApiRoute` for the backend
- `AppRoute` for the frontend
- `TopLevel` to delegate

Based on the earlier descriptions of the different backend and frontend functionality we can divide
them up into the following data structures:

```haskell
data ApiRoute
  = ApiRoute_Status
  | ApiRoute_Version
  | ApiRoute_Uptime
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Universe ApiRoute

data AppRoute
  = AppRoute_Register
  | AppRoute_Contact
  | AppRoute_About
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Universe AppRoute
```

As for the representation of these routes as paths, the backend api routes are:

- `/status`
- `/version`
- `/uptime`

The frontend routes are:

- `/register`
- `/contact`
- `/about`

We'll define the `Encoder`s for these later using `enumEncoder`s as we have done in earlier
sections. The focus here is on the `TopLevel` structure to perform the delegation and maintain
the organisation we require.

We want a structure that will allow us to organise the two sub-systems into a hierarchy, keeping
their respective paths isolated from one another and ensure that neither sub-system, nor this top
level, have or need any special awareness of each other.

To that end we will create a type that will allow us to 'chain' together the different structures
while enabling us to handle them separately. In earlier sections the route had a single possible
instantiation, but a route for the `TopLevel` could be _any_ one of the sub-routes.

To achieve the desired modularity we use the type of the sub-route as an argument to this top-level
route constructor.

We will build this route in `Common.Route` using a [Generalised Algebraic Data Type](http://dev.stephendiehl.com/hask/#gadts), or 'GADT'. Part of the reason for this is that it more providese more detailed type information for greater type-safety that aid in providing the guarantees that this package is built upon.

If you've not encountered GADTs before, or you're a bit rusty, check out the following links for more information:
* [What I Wish I Knew When Learning Haskell - GADTs](http://dev.stephendiehl.com/hask/#gadts)
* [Haskellforall](http://www.haskellforall.com/2012/06/gadts.html)
* [Haskell Wiki](https://wiki.haskell.org/Generalised_algebraic_datatype)

It's not necessary to have a deep understanding of GADTs to use them with `obelisk-route`, we'll
provide enough of information to be able to get by.

----

In the `Common.Route` module add the following line:

```haskell
data TopLevel :: * -> * where
```

This is the declaration of our `TopLevel` type, the `* -> *` annotation indicates that this type
requires an additional type argument. This is the part of the abstract route definition that will be
determined once we know which constructor will be used:

```haskell
data TopLevel :: * -> * where
  TopLevel_API :: TopLevel ApiRoute
  TopLevel_APP :: TopLevel AppRoute

-- Template Haskell to generate required instances.
deriveRouteComponent ''TopLevel
```

These are our constructors for the routes. As in earlier sections, these following the naming
convention of including the type name. On the right hand side of the `::` we provide explicit type
for each constructor.

Now that we have the logical structure of our route defined, we will start writing the `Encoder` to
build our concrete definition. Each constructor now represents either all possible routes to either
`ApiRoute` or `AppRoute` with the logical distinction between the two.

The following paths will be defined in our `Encoder` and as with our constructors the `Encoder` for
the `TopLevel` is only aware of and responsible for enough of the structure to handle deciding which
sub-structure provide the `Encoder` for. This `Encoder` has zero knowledge of any of the sub-routes
and only handles the minimum to perform its task:

- `/api/...` ~ backend
- `/app/...` ~ frontend

With those paths in mind we can start constructing our `Encoder`:

```haskell
topLevelRouteEncoder
  :: ( MonadError Text check
     , MonadError Text parse
     )
  => Encoder check parse (R TopLevel) PageName
topLevelRouteEncoder = _todo
```

This similar to earlier `Encoder`s with the main difference being the use of the `R` type to wrap
our `TopLevel` type.

### Aside: `R`

When referring to route types in type signatures when they are defined as 'GADT's, we need to ensure
that the route type remains polymorphic. Using the route type `TopLevel a` as an example, each
constructor may specify a different type in place of that `a`. Consequently we can't expose that `a`
in the type signature because the type system will demand that `a` have a single concrete type.

To keep the polymorphism that we need, we wrap the route type using [`DSum`](https://hackage.haskell.org/package/dependent-sum/docs/Data-Dependent-Sum.html#t:DSum) from the 'dependent-sum' package. This uses [existential quantification](http://dev.stephendiehl.com/hask/#existential-quantification) to prevent the type variable from our route type appearing in the type signature, preserving polymorphism without any loss of type safety.

Using the full `DSum TopLevel Identity` type everytime we wanted to refer to our route type would be
tedious and noisy so we use the type alias `R` as shorthand:

```haskell
type R f = DSum f Identity
```

---

The `Encoder` we will use is `pathComponentEncoder`, which works almost exactly
like `enumEncoder` with the ability to leverage the extra type information carried by our GADT.

```haskell
pathComponentEncoder
  :: forall check parse p.
     ( Universe (Some p)
     , GShow p
     , GCompare p
     , MonadError Text check
     , MonadError Text parse )
  => (forall a. p a -> SegmentResult check parse a)
  -> Encoder check parse (R p) PageName
```

Where the type variable `p` is replaced by `TopLevel` and the Template Haskell we added after our
`TopLevel` definition will take care of the constraints for this function. Of interest to us is the
function we need to write to make this `Encoder` work:

```haskell
  => (forall a. p a -> SegmentResult check parse a)
```

This is similar to the `p -> r` function that is required by the `enumEncoder` to handle the
different cases of the sum type. Now we have more type information available to us, and instead of
directly constructing the `PageName` ourselves, we build values of type `SegmentResult` and
`pathComponentEncoder` handles the rest for us.

### Aside: `SegmentResult`

This type is part of the interface to the `pathComponentEncoder` function. Because routes may be
nested in the definition of other routes. All of the routes related to pull requests nested under
the route for a repository, for example. We need a way to describe whether a route may have more
segments or if we've reached the end.

To do this, the `SegmentResult` has two constructors, one recursive allowing for more structure, and
the other terminating, indicating that this path is complete.

The recursive constructor has the following type:

```haskell
PathSegment :: Text -> Encoder check parse a PageName -> SegmentResult check parse a
```

This lets us specify a fixed component of a path, and then we must also provide an `Encoder` to turn
this value (the one on the left hand side of the case expression) into the remainder of the route.

The other constructor:

```haskell
PathEnd :: Encoder check parse a (Map Text (Maybe Text)) -> SegmentResult check parse a
```

Indicates that the path is complete and requires an `Encoder` to turn this value into query parameters.

Refer to the Haddock documentation for more detailed information.

----

Create the function from a `case` expression and place a typed hole on the right hand side of each branch:

```haskell
topLevelRouteEncoder
  :: ( MonadError Text check
     , MonadError Text parse
     )
  => Encoder check parse (R TopLevel) PageName
topLevelRouteEncoder = pathComponentEncoder $ \case
  TopLevel_API -> _apiTodo
  TopLevel_APP -> _appTodo
```

The type of `pathComponentEncoder` indicates we need to return a type `SegmentResult`. Because both
of these routes have more segments defined in a separate `Encoder` and we have a intermediate static
segment to differentiate the two routes, we will use the `PathSegment` constructor:

```haskell
PathSegment Text (Encoder check parse a PageName)
```

Add this constructor to the right hand side of the `case` branches each route. Use "api" as the
first argument for `TopLevel_API`, and "app" for `TopLevel_APP`. Leave a typed hole as the
second argument for both:

```haskell
  TopLevel_API -> PathSegment "api" _apiTodo
  TopLevel_APP -> PathSegment "app" _appTodo
```

The type of the `_apiTodo` hole will be:

```haskell
    • Found hole:
        _apiTodo :: Encoder check parse ApiRoute PageName
```

To satisfy this type, we will build the `Encoder`s for the backend and frontend and then replace
these holes with their respective `Encoder`s. We won't dwell on these `Encoder`s as they are built
using techniques we have already covered.

The `Encoder` for each of these routes is defined using the `enumEncoder`:

```haskell
apiRouteEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse ApiRoute PageName
apiRouteEncoder = enumEncoder $ \case
  ApiRoute_Status -> (["status"], mempty)
  ApiRoute_Version -> (["version"], mempty)
  ApiRoute_Uptime -> (["uptime"], mempty)

appRouteEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse AppRoute PageName
appRouteEncoder = enumEncoder $ \case
  AppRoute_Register -> (["register"], mempty)
  AppRoute_Contact -> (["contact"], mempty)
  AppRoute_About -> (["about"], mempty)
```

These can replace the typed holes to complete our `Encoder` with nested routes:

```haskell
topLevelRouteEncoder
  :: ( MonadError Text check
     , MonadError Text parse
     )
  => Encoder check parse (R TopLevel) PageName
topLevelRouteEncoder = pathComponentEncoder $
  TopLevel_API -> PathSegment "api" apiRouteEncoder
  TopLevel_APP -> PathSegment "app" appRouteEncoder
```

Now all of these routes are organised in a way that makes sense _for this application_. Both the
nested and top-level `Encoder`s are independent of one another and can be moved and altered without
impacting the other.

If `AppRoute` is extended or made more complex, those changes are localised to the `appRouteEncoder`.

If `ApiRoute_Status` needs to be extended with its own nested routes then it can be separated into
its own `Encoder` and again those changes are kept local to that `Encoder`.

### Aside: Packaged routes

This modularity allows you to package entire route structures and their functionality into a
standalone Haskell package that other people can import and use. Plugging it into their route
structure _where it makes sense for their application_.

It's important to note here that when using your routes in their application, they could leverage
the abstract definition of your routes and substitute their own encoding. This would allow them to
replace functionality provided by your routes as per their needs without breaking existing links.

If you have repeated functionality in your application you could abstract out the differences and
package the similarities, routes and all!

## Add parameter to a nested route {#nestedRouteParam}

Suppose we want to use part of the route input as a value that is available for use at the
destination. Let's assuming we want to add a homepage for every user. To do so we will need a route
to represent a path to the homepage for _every possible user ID_, which means we parameterise this
route by the type that represents every possible user ID.

Our application has `Text` based user IDs, but we're responsible Haskellers so we will use a
`newtype` to wrap this value up and keep our code cleaner:

```haskell
newtype UserID = UserID { unUserID :: Text } deriving (Show, Eq)
makeWrapped ''UserID
```

That final line is template haskell from the [lens](https://hackage.haskell.org/package/lens)
library that will generate a `Wrapped` typeclass instance for us. This provides a useful `Iso` for
us that makes writing the `Encoder`, and using the `UserID` type itself, easier.

Now we can define the route constructor as:

```haskell
  AppRoute_UserHome :: AppRoute UserID
```

This type represents one logical page per instantiation per possible user ID. In the address bar
this will appear as : `/user/$UserID`. After adding this new route, we will start seeing a
warning that not all of our route constructors are accounted for in the `Encoder`.

Extend that `case` expression to include this new route:

```haskell
  AppRoute_UserHome -> PathSegment "user" _userhomeTodo
```

Similar to other routes, we have a static component, `"user"`, and we have a typed hole that we need
to fill in. In this instance the typed hole as the following type:

```haskell
_userhomeTodo :: Encoder check parse UserID PageName
```

In order to fill this typed hole we need to provide an `Encoder` that turns a `UserID` into a
`PageName`. To do we're going to be composing existing `Encoder`s. However the `(.)` operator from
`Prelude` is specialised to the function arrow type `(->)` so we must import the more general
version from the `Control.Category` module, because it uses the `Category` typeclass constraint:

```haskell
-- (.) from Prelude
(.) :: (b -> c) -> (a -> b) -> a -> c

-- (.) from Control.Category
(.) :: Category cat => cat b c -> cat a b -> cat a c
```

The `Encoder` type is an instance of `Category` so we need to use the more general function. To ensure
that the correct `(.)` is imported use the following snippet:

 ```haskell
-- This imports everything from Prelude except for (.)
import Prelude hiding ((.))

-- This imports everything from Control.Category
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
`Encoder`s. Although we can _think of_ `Encoder`s like they are pure functions, the types are
distinct so we must use the correct function to compose them.

----

This package does not have any single `Encoder` that will satisfy that type. What this package
provides is the smaller pieces we need to _build_ this `Encoder`. Small `Encoder`s that do one thing
and do it correctly, such that we can compose these `Encoder` to perform the steps we need.

Of the entire path: `/user/$userid`. The `user` section is nested in the `app` segment, so the
`Encoder` we need to build must target the `$userid` segment, and only that segment, of the route.

This is done using the following `Encoder`:

```haskell
singlePathSegmentEncoder
  :: ( Applicative check
     , MonadError Text parse
     )
  => Encoder check parse Text PageName
```

The `singlePathSegmentEncoder` is an `Encoder` that will ignore any query parameters and focus only
on a single segment in the path, disregarding any further segments. Using the intuition of thinking
about `Encoder` like a pure function, this `Encoder` is like a function from `Text` to `PageName`.

We will also need an `Encoder` that to go from `UserID` to `Text`. This is where the `Wrapped`
typeclass we derived earlier comes in handy as that typeclass lets deal with all kinds of `newtype`
wrappers in a consistent way. We don't need their unique constructors and deconstructors, the
typeclass abstracts that away.

The following `Encoder` leverages this typeclass to provide this `Encoder`:

```haskell
unwrappedEncoder
  :: ( Wrapped a
     , Applicative check
     , Applicative parse
     )
  => Encoder check parse a (Unwrapped a)
```

The type `(Unwrapped a)` is an associated type from the `Wrapped` typeclass that represents the type that
the `newtype` wraps. In our case the type variable `a` can be replaced with `UserID` and `Unwrapped
UserID` with `Text`. These replacements give us an `Encoder` of the following type:


```haskell
unwrappedEncoder :: _ => Encoder check parse UserID Text
```

Now that we have the two `Encoder`s we need, we compose them together:

```haskell
  -- New route branch in the case expression
  AppRoute_UserHome -> PathSegment "user" $ singlePathSegmentEncoder . unwrappedEncoder
```

## Multiple Parameters {#multipleParams}

Sometimes one parameter isn't enough and you need more information, these parameters might be
grouped together in sequence because they're all associated with a single route. An example might be
puzzles hidden in your application that provide three inputs, that when combined will take the user
to the prize page:

```shell
/app/code/$solutionA/$solutionB/$solutionC
```

An alternative requirement might be that there is a set of nested routes that have a parameter and
one of those nested routes requires its own parameters. Such as a route for a specific repository
for a specific user:

```haskell
/app/user/$userId/repository/$repoId
```

Each require a slightly different approach because the parameters appear at different positions
along the route. This is important to note because when we have a 'non-parameter' route segment,
such as "user", it indicates that a decision needs to be made as to how we proceed down the route
hierarchy.

- In the case of `code/$solutionA/$solutionB/$solutionC`, we expect the types of the abstract routes to stay consistent as we go from `A` to `B` to `C`. those types can't change.

- However, in `user/$userId`, we expect that the next path segment could be "repository", or if we had implemented other routes, it could be a different structure with its own subroutes.

### In sequence

In the first example, there are puzzles that provide three inputs and when combined into a single
route will take the user to a specific page. This is only a single logical page but it has three
required inputs and the path would appear in the address bar as follows:

```shell
/app/code/$solutionA/$solutionB/$solutionC
```

Defining the first part of our constructor leads to an interesting question: What is the type that
this constructor will be parameterised by? Within `obelisk-route` is the type alias `(:.)` which is
an alias for a tuple `(,).`This allows us to express multiple possibly different types as the
type for a route constructor. Assuming that our puzzle solutions will be an `Int` value, a `Text`
value, and a final `Int` value, then our route is defined as:

```haskell
  AppRoute_CodePrize :: AppRoute (Int :. Text :. Int)
```

Which is equivalent to the following but easier to read:

```haskell
  AppRoute_CodePrize :: AppRoute (Int, (Text, Int))
```

Now it is a matter of building our `Encoder` to process the individual segments and package them all
up as required. To handle the `(:.)` portion of the encoding process, we're going to use the
`pathParamEncoder` that has the following type:

```haskell
pathParamEncoder
  :: ( ... )
  => Encoder check parse item Text
  -> Encoder check parse rest PageName
  -> Encoder check parse (item :. rest) PageName
```

This function requires two `Encoder`s, one for the initial parameter `item`, and another for the
`rest` of the route. In this case it will be another `pathParamEncoder` because the second parameter
is another tuple. Lets extend the `appRouteEncoder` with another branch on the `case` expression:

```haskell
  AppRoute_CodePrize -> PathSegment "code" $ pathParamEncoder
    _itemEncoder
    _restEncoder
```

The types of the two typed holes are :

```haskell
_itemEncoder :: Encoder check parse Int Text
_restEncoder :: Encoder check parse (Text :. Int) PageName
```

We're going to lean on the `Show` & `Read` instances for `Int` because for `Int` these typeclasses
are inverses of one another, meaning that we don't lose any information about the `Int` value. So
we're able to leverage these instances to create a safely bidirectional `Encoder`. To do this, we
will use the `unsafeTshowEncoder`:

```haskell
unsafeTshowEncoder
  :: ( Show a
     , Read a
     , Applicative check
     , MonadError Text parse
     )
  => Encoder check parse a Text
```

This `Encoder` that will `show` the value of type `a` when creating the URL segment, and `read` when
trying to turn that segment of the URL into a value of type `a`. This is only safe to do because _we
know_ that the `Show` and `Read` instances for `Int` are inverses. Refer to the Haddock
documentation for `unsafeTshowEncoder` for more information.

Replace the `_itemEncoder` with this `Encoder`:

```haskell
  AppRoute_CodePrize -> PathSegment "code" $ pathParamEncoder
    unsafeTshowEncoder
    _restEncoder
```

Now onto the next part of our `Encoder`. Checking the `_restEncoder` typed hole, we can see that it
has the following type:

```haskell
_restEncoder :: Encoder (Either Text) (Either Text) (Text :. Int) PageName
```

This `Encoder` is for the next two parameters of our tuple, so we can start building this `Encoder`
using the `pathParamEncoder`. This once again reduces the work to two separate `Encoder`s that do
the minimum we need, and the `pathParamEncoder` handles the tupling for us:


```haskell
  AppRoute_CodePrize -> PathSegment "code" $ pathParamEncoder
    unsafeTshowEncoder
    $ pathParamEncoder
      _itemEncoder
      _restEncoder
```

We repeat the process of working through the typed holes to build or find the `Encoder`s we need to
define our route. The `_itemEncoder` is now the second parameter to our tuple, which is a `Text`
value so the typed hole has the following type:

```haskell
_itemEncoder :: Encoder check parse Text Text
```

Remembering that an `Encoder check parse a b` may be thought of as a function of type `a` to `b`. In
this case it would be equivalent to a function of type `a` to `a` as both types are the same. This
is equivalent to the `id` function, which is part of the `Category` typeclass, of which `Encoder`
has an instance. Thus the `Encoder` is `id`:

```haskell
  AppRoute_CodePrize -> PathSegment "code" $ pathParamEncoder
    unsafeTshowEncoder
    $ pathParamEncoder
      id
      _restEncoder
```

The final `_restEncoder` typed hole has the following type, as the last value in the tuple and the
path itself:

```haskell
_restEncoder :: Encoder check parse Int PageName
```

We will use `unsafeTshowEncoder` again, but this time we compose it with the
`singlePathSegmentEncoder`, which has the following type:

```haskell
singlePathSegmentEncoder :: _ => Encoder check parse Text PageName
```

Making the final `Encoder` for the `(Int :. Text :. Int)` type:

```haskell
  AppRoute_CodePrize -> PathSegment "code" $ pathParamEncoder
    unsafeTshowEncoder
    $ pathParamEncoder
      id
      (singlePathSegmentEncoder . unsafeTshowEncoder)
```

### Aside: `unsafeShowEncoder`

Another `Encoder` that we could used as the final `_restEncoder` is the `unsafeShowEncoder`:

```haskell
unsafeShowEncoder
  :: ( MonadError Text parse
     , Read a
     , Show a
     , Applicative check
     )
  => Encoder check parse a PageName
```

It uses the `Show` and `Read` instances by returns a `PageName`. This one `Encoder` can be used in
place of the composition of:

```haskell
singlePathSegmentEncoder . unsafeTshowEncoder
```

It's not the use of `Show` and `Read` that is unsafe, it's that if the types change and the
instances exist then the compiler will automatically use those instances. Which might not be what
you want.

### Nested parameters

Another situation where you might have multiple parameters is when there are nested routes and an
upper level of the nesting has a parameter along with one or more of the nested routes:

```haskell
/app/user/$userId/repository/$repoId
```

For this example we will use the following `newtypes` and routes:

```haskell
newtype UserID = UserID { unUserID :: Text } deriving (Show, Eq)
makeWrapped ''UserId

newtype RepoId = RepoID { unRepoID :: Text } deriving (Show, Eq)
makeWrapped ''RepoId

data UserRoute :: * -> * where
  UserRoute_Home :: UserRoute ()
  UserRoute_Repository :: UserRoute RepoId

data AppRoute :: * -> * where
  AppRoute_CodePrize :: AppRoute (Int :. Text :. Int)
  AppRoute_User :: AppRoute (UserID :. R UserRoute)
```

Of most interest to us is the `AppRoute` definition. The `AppRoute_UserHome` constructor is changed
to be `AppRoute_User`, because there is more than one route under each `UserID`. Looking at its type
`(UserID :. R UserRoute)` we can see that this route will need a `UserID` followed by one of the
routes in `UserRoute`.

We will need to updated the `appRouteEncoder` because the name and the type of our constructor has changed:

```haskell
appRouteEncoder :: _ => Encoder check parse (R AppRoute) PageName
appRouteEncoder = pathComponentEncoder $ \case
  AppRoute_User -> PathSegment "user" $ _f
```

We're building an `Encoder` for set of routes (`R UserRoute`) as we've done before at the same time
is retrieving the `UserID` parameter that the `AppRoute_UserHome` constructor originally
required. As before we use the `pathParamEncoder` to manage the tupling for us:

```haskell
appRouteEncoder :: _ => Encoder check parse (R AppRoute) PageName
appRouteEncoder = pathComponentEncoder $ \case
  AppRoute_User -> PathSegment "user" $ pathParamEncoder

    -- Encoder check parse UserID Text
    _userIdEncoder

    -- Encoder check parse (R UserRoute) PageName
    _userRouteEncoder
```

Constructing the `Encoder` for the `UserID` is the same as we have before when dealing with a
`newtype` that has an instance of `Wrapped`:

```haskell
appRouteEncoder :: _ => Encoder check parse (R AppRoute) PageName
appRouteEncoder = pathComponentEncoder $ \case
  AppRoute_User -> PathSegment "user" $ pathParamEncoder

    -- Encoder check parse UserID Text
    (unsafeTshowEncoder . unwrappedEncoder)

    -- Encoder check parse (R UserRoute) PageName
    userRouteEncoder
```

Now we complete the route by defining the `Encoder` for `R UserRoute`:

```haskell
userRouteEncoder :: _ => Encoder check parse (R UserRoute) PageName
userRouteEncoder = pathComponentEncoder $ \case
  UserRoute_Repository -> PathSegment "repository" $ singlePathSegmentEncoder
    . unsafeTshowEncoder
    . unwrappedEncoder
```

## Query Parameters

This package has the capability to collect the various query parameters that may be included as part
of a route. Query parameters can be collected on their own:

```
/app/picture-of-the-day?width=800&height=600
```

Or they may be collected along with other route parameters:

```
/app/user/$userid/repository/$repoid?commit=abc1234
```

### Only query parameters

Displaying a 'picture of the day' that allows the user to set the height and width of the image is
an example of a route that only has a single logical page but is configurable through query parameters:

```
/app/picture-of-the-day?width=800&height=600
```

The type of this route will only have arguments that represent the query parameters, without
duplicates so the type of our route argument will be a `Map`:

```haskell
  AppRoute_POTD :: AppRoute (Map Text (Maybe Text))
```

The types of the values of the `Map` are `Maybe Text` because a parameter may not require an
explicit value, including the parameter at all could be sufficient.

For our type we have no additional parameters and no nesting, we're only interested in the query
parameters. The `Encoder` for this task is the `queryOnlyEncoder`:

```haskell
queryOnlyEncoder :: _ => Encoder check parse (Map Text (Maybe Text)) PageName
```

Assuming an `appRouteEncoder :: Encoder check parse (R AppRoute) PageName` that is defined using the
`pathComponentEncoder`, we will extend the `case` expression:

```haskell
  AppRoute_POTD -> PathSegment "picture-of-the-day" queryOnlyEncoder
```

### With another path parameter

Sometimes query parameters are included alongside other route parameters. Taking the "repository"
route from the [Multiple Parameters](#multipleParams) section, if we wanted to be able to view a
specific commit. It doesn't necessarily make sense to make an entirely new page for that, as it is a
configurable state of the existing page. So we're able to take the existing route type:

```haskell
  UserRoute_Repository :: UserRoute RepoId
```

Then modify it to accept a `Map` of query parameters as well as the `RepoId`:

```haskell
  UserRoute_Repository :: UserRoute (RepoId :. Map Text (Maybe Text))
```

Note the reuse of the `(:.)` operator to combine the two route inputs. This provides a hint as to
how we will modify the existing `Encoder` for this route:

```haskell
  UserRoute_Repository -> PathSegment "repository"
    $ singlePathSegmentEncoder . unsafeTshowEncoder . unwrappedEncoder
```

To now require the `RepoId` and have the capability to handle query parameters:

```haskell
  UserRoute_Repository -> PathSegment "repository" $ pathParamEncoder repoIdEncoder queryOnlyEncoder
  where
    repoIdEncoder :: (..) => Encoder check parse RepoId Text
    repoIdEncoder = unsafeTshowEncoder . unwrappedEncoder
```

We factor out the `Encoder` for the `RepoId` to keep things a bit more organised, which is more
general than the type signature suggests, but we're keeping things fixed for the moment. Pass this
`Encoder` as the first argument to `pathParamEncoder` then use the `queryOnlyEncoder` to take any
query parameters and turn them into the `Map` we require.

This route changes to now have the following possible representation in the address bar:

```
/app/user/$userid/repository/$repoid?commit=abc1234
```
