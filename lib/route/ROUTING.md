# Obelisk Routing

This document contains examples and introductory material for using the
`obelisk-routes` package in your application.

The `obelisk-routes` package is designed to help with managing the paths and
parameters for the various pages in your application. These routes are for
organising and structuring the navigation of your application.

This is distinct from packages like `servant` that are designed for building
endpoints for a REST API. When building these routes you're not going to be
handling headers or HTTP methods.

One of the goals of `obelisk-routes` is to allow for complex routing
structures to be partitioned into manageable chunks that are then combined in
a straight-forward manner. Allowing for re-use of various components and to
help the type-system provide as many guarantees as possible.

## Building blocks

There are a few pieces required to build routes in Obelisk and there is plenty to dig into
if you are curious. However for our purposes we can survive with a surface level
understanding.

#### `SegmentResult`

This is the primary building block of individual routes, it has a list-like
structure of; a recursive component, and a terminating case.

The `PathEnd ...` constructor may be used to indicate that there is only this
single segment in that route, or to terminate a longer route.

Or the `PathSegment "user" ...` to encode "user" as a literal segment in the
path, followed by more `Encoder`s.

#### `Encoder`

An `Encoder` describes how the either an individual segment of a route is to
be encoded. Such as how to read the number from a route like `/user/33`.

It can also describe how several routes are encoded. So you may have an
`Encoder` that describes how all of the routes pertaining to 'blog posts' are
encoded.

In particular note that `Encoder`s may be composed using the composition
operator from `Control.Category`. This means that complex route structures
can be broken down into manageable pieces and then composed together to
encode the entire structure.

#### `BackendRoute` & `FrontendRoute`

Obelisk has two distinct sets of routes for backend and frontend. These GADTs
are called `BackendRoute` and `FrontendRoute`. These are provided for you to
start you off, and we will create our own later on.

In a fresh Obelisk application these live in the `Common.Route` module.

`BackendRoute` is for serving static things or deferring to other backend endpoints. Such as:
- "static/files/img/:imgName:"
- "api/..." where the "..." portion is managed by a REST endpoint package of your choice.

`FrontendRoute` is for managing the frontend routing of your application. Such as:
- "blog/:userId:/:postId:"
- "dashboard?optionA=true".

Building routes for either one is the same process. Keeping the types separate enforces
the distinction at compile time and helps to prevent errors.

The routing system in Obelisk is quite powerful and we'll only touch on some
basics here, but this should be enough to start with and we can dive into the
gnarlier routing problems later.

## An example route

Consider the following `Frontend` request that we want our Obelisk
application to be able to handle:

```
GET /user/42
```

It has a static portion "user" and some number that we want to verify and
have available on our page for us to use.

We could use a bare `Int` in our types but we're responsible Haskellers so
define a `newtype` in `Common.Api` to wrap the number for greater type safety
and to be more descriptive:

```haskell
newtype UserId = UserId { unUserId :: Int }
```

For Obelisk to be aware of our routes existence, we add a constructor to
`FrontendRoute` GADT in `Common.Route`. The purpose of this constructor is to
give us something to match on when we're deciding which code to run for which
page. As well as declare what are the types of things that will be pulled
from the route and available on the resulting page.

Add the following constructor to `FrontendRoute`:

```haskell
  FrontendRoute_User :: FrontendRoute UserId
```

This says that we have route that will provide a `UserId` to our frontend.

> The naming of the route 'FrontendRoute_User' is an Obelisk convention of including the
> type name in individual constructor names. This helps disambiguate them in the code at
> the cost of a few extra keystrokes. As an example:
>
> ```haskell
> data Foo
>   = Foo_ConstructorA
>   | Foo_ConstructorB
> ```

Next we need to build the `SegmentResult` that will tell Obelisk how this route is constructed.

Obelisk router breaks down incoming routes into smaller segments that are then given to
our `Encoder` to try to find a match. We define these segments using the `SegmentResult`
type and add them to the case expression in `Common.Route` to be matched against.

Looking at our route:

```
/user/42
```

We're able to identify two segments:

```
/ "user" / (UserId 42)
```

So we need to describe the static "user" segment, and the dynamic segment that takes the
raw input of "42" and turns it into a `UserId`.

We need to tell Obelisk how to create a `UserId` from `Text` and back again. To do this
we're going to write two functions and put them together into a `Prism'`. A `Prism'` is a
condensed way of describing how to go from one type to another, and maybe back the other
way.

Turning our `UserId` into `Text` is easy so let's do that one first:

```haskell
userIdTextPrism :: Prism' Text UserId
userIdTextPrism = prism toText _fromText
  where
    -- Unwrap the UserId, use 'show' on the 'Int' value, and then turn that into 'Text'
    toText = T.pack . show . unUserId
```

Now we encode `Text` as a `UserId`. This means we have to check that this input is
valid. At this stage we only care that the input is a number greater than zero:

```haskell
userIdTextPrism :: Prism' Text UserId
userIdTextPrism = prism toText _fromText
  where
    -- Unwrap the UserId, use 'show' on the 'Int' value, and then turn that into 'Text'
    toText :: UserId -> Text
    toText = Text.pack . show . unUserId

    fromText :: Text -> Either Text UserId
    fromText t = case readMaybe $ Text.unpack x of
      Just i -> Right $ UserId i
      Nothing -> Left x
```

Our `Prism'` can now be used to create the rest of `SegmentResult` like so:

```haskell
PathSegment "user" $ singlePathSegmentEncoder . prismEncoder userIdTextPrism
```

We combine our `userIdTextPrism` using `prismEncoder` from `Obelisk.Route` to do the heavy
lifting for us, and compose that using `(.)` with `singlePathSegmentEncoder` to tell
Obelisk that this part of the route only has a single piece.

It should be a rare event that you will need to write your own `Encoder` directly. The
provided `Encoder`s in `Obelisk.Route` are designed to be composed and combined to build more
complicated encoders and should be sufficient for most requirements.

#### 'Wrapped' typeclass and Template Haskell.

If you're comfortable with Template Haskell, you can use the `makeWrapped` splice from
`Control.Lens.TH` to derive the `Wrapped` typeclass. This would let you avoid having to
write a `Prism'`:

```haskell
PathSegment "user" $ readShowEncoder . unwrappedEncoder
```

This is handy if you already have your newtypes defined and don't want to hand write
`Prism'`s. This uses the `_Wrapped` prism from the `lens` library and then the encoder
that uses the `Read` and `Show` instances for `Int`.

## Using the routes

`subRoute`
`askRoute`
`subPairRoute_`
