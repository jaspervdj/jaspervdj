---
title: Digestive functors 0.0.2
description: An upgrade of formlets
tags: haskell, code
---

Today, I'm releasing something I've been working on a while. I planned to
complete it on [BelHac]($root/posts/2010-11-09-belhac-summary.html), but it all
got delayed a little.

When I was writing the [blaze-html](http://jaspervdj.be/blaze) backend for
[formlets](http://github.com/chriseidhof/formlets) a while ago, I found formlets
one of the most interesting libraries I had ever worked with. However, there
were a few things that annoyed me:

- it was very hard to generate semantic HTML `<labels>`s;
- there was no good way to print error messages next to the fields which caused
  the errors;
- it fixed too many types, such as, for example, the type for file uploads -- it
  would be hard to port this to, for example, file uploads using iteratees.

With the blessing of [Chris](http://eidhof.nl/), I decided to create a new
version from scratch.

Digestive functors 0.0.2
========================

This file is written in literate Haskell. You can find the source code
[right here](http://github.com/jaspervdj/jaspervdj/blob/master/posts/2010-12-09-digestive-functors-0.0.2.lhs).
If you install `digestive-functors-blaze` and `digestive-functors-snap` from
Hackage, you should be good to go: run this file with `runghc` and you
should have a small webapp running at [localhost:8000](http://localhost:8000/).

We import `Text.Digestive` to get the general API provided by digestive
functors:

> {-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
> import Control.Applicative ((<$>), (<*>))
> import Text.Digestive

Digestive functors is structured into three layers:

![The layered design]($root/images/2010-12-09-digestive-functors-layers.png)

As actual web server responsible for I/O, we're using
[Snap](http://snapframework.com). A [Happstack](http://happstack.com) backend is
available, too.

> import Text.Digestive.Forms.Snap
> import Snap.Types
> import Snap.Http.Server (httpServe)

We're going to use blaze as frontend. This is the only supported frontend for
now, but I'm going to work on other frontends such as HSP.

> import Text.Digestive.Blaze.Html5
> import Text.Blaze (Html, (!))
> import qualified Text.Blaze.Html5 as H
> import qualified Text.Blaze.Html5.Attributes as A
> import Text.Blaze.Renderer.Utf8 (renderHtml)

We're going to make a small webapp to calculate a [weighted sum]. We have a
simple datatype describing the input for our mind-blowing calculations:

[weighted sum]: http://en.wikipedia.org/wiki/Weight_function

> data WeightedSum = WeightedSum [Double] [Double]

And a function calculating the result:

> weightedSum :: WeightedSum -> Double
> weightedSum (WeightedSum weights values) = sum $ zipWith (*) weights values

To get the sum, we're going to need two lists, entered by the user. We're going
to use the property here that list is an instance of `Read`.

> listForm :: (Read a, Show a) => [a] -> SnapForm Html BlazeFormHtml [a]
> listForm def = inputTextRead "Can't read list" (Just def) <++ errors

Let us examine this a little more closely.

~~~~~{.haskell}
listForm def =
~~~~~{.haskell}

We create a new Haskell function which will return a list. `def` is the default
value (to give the user a pointer on the syntax).

~~~~~{.haskell}
inputTextRead "Can't read list" (Just def)
~~~~~

Here, we specify a textbox for values which instantiate `Read`. We give an error
message in case the user enters something invalid -- this error message will be
thrown when the value can't be `read`. We also pass our default value.

~~~~~{.haskell}
<++ errors
~~~~~

`<++` is an operator used to append certain "special" forms on the right side
(`++>` also exists, of course). Here, we append `errors` which will basically
generate a list of errors for the corresponding field. Now, we can look at
the type of the form:

~~~~~{.haskell}
SnapForm Html BlazeFormHtml [a]
~~~~~

It simply is a form using the Snap backend, using the type `Html` for the errors
(we use `Html` instead of `String` because we might want to have some extra
formatting in the errors). `BlazeFormHtml` is the "view" we are producing, and
our form will return an `[a]`.

One of the main reasons for using applicative functors to create forms is
composability. We're going to compose two `listForm`s into a form we can use for
our `WeightedSum` type. Composition is done using the standard `<$>` and `<*>`
applicative interface.

> weightedSumForm :: SnapForm Html BlazeFormHtml WeightedSum
> weightedSumForm = (`validate` equalSize) $ (<++ errors) $ WeightedSum
>     <$> label "Weights: " ++> listForm [0.4, 0.4, 0.2]
>     <*> label "Values: "  ++> listForm [64, 67, 91]

We're using the `label` function here to create a semantic HTML `<label>` (when
the user clicks the label, the corresponding input field will be selected). We
`validate` our form using the `equalSize` validator (explained a bit further
down).

We also append `errors` to our `WeightedSum` form. Digestive functors has two
main functions for selecting errors:

- `errors` will list only the errors corresponding to this exact form;
- `childErrors` will list all errors belonging to form, as well as all errors
  belonging to one of the children forms. In this case, using `childErrors`
  would mean that we would see "Can't read list" errors appearing twice (once
  for the `listForm`, and once for this form) -- but it can be quite useful in
  certain scenario's.


To calculate a weighted sum, the lists must be of the same size -- this is why
we have the `equalSize` validator. Writing validator's isn't very hard, and this
one is particulary easy because it's a pure validator.

> equalSize :: Validator Snap Html WeightedSum
> equalSize = check "Lists must be of equal size" $ \(WeightedSum l1 l2) ->
>     length l1 == length l2

With the `check` function, you simply have to give an error message and a
predicate, and your done.

Now, we need to get the webapp running on snap. We first need a simple utility
function to render our blaze templates:

> blaze :: Html -> Snap ()
> blaze response = do
>     modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
>     writeLBS $ renderHtml response

Then, we can write a `Snap` handler to serve this form.

> weightedSumHandler :: Snap ()
> weightedSumHandler = do

`eitherSnapForm` is where the real digestive magic works. It will will evaluate
the form on a `POST` request, and view the form on a `GET` request.

>     r <- eitherSnapForm weightedSumForm "weighted-sum-form"
>     case r of

If we get a form back, something went wrong, or the user just wants to view the
form. Either case, we just render the form using blaze.

>         Left form' -> blaze $ do
>             let (formHtml', enctype) = renderFormHtml form'
>             H.style ! A.type_ "text/css" $ do
>                   "input {display: block;}\n"
>                   ".digestive-error-list {\n"
>                   "    color: white;\n"
>                   "    background-color: rgb(100, 0, 0);\n"
>                   "}"
>             H.h1 "Evaluate a weighted sum"
>             H.form ! A.enctype (H.stringValue $ show enctype)
>                    ! A.method "POST" ! A.action "/" $ do
>                 formHtml'
>                 H.input ! A.type_ "submit" ! A.value "Submit"

Note how the encoding type (`enctype`) is also returned from the form view.
We're using `.digestive-error-list` to style it up a little. These classes are,
of course, completely customizable.

If we got an actual `WeightedSum`, it means that the user filled in everything
correctly (the input validated). We can now evaluate and print this result.

>         Right weightedSum' -> blaze $ do
>             H.h1 "HUGE SUCCES"
>             H.p $ do
>                 H.strong "Result: "
>                 H.string $ show $ weightedSum weightedSum'

A main function to server the handler, and we're set!

> main :: IO ()
> main = httpServe "*" 8000 "weighted-sum" Nothing Nothing weightedSumHandler

That's it
---------

I hope this blogpost clarified what digestive functors is and how you use it. If
you're interested, you can check out
[digestive-functors](http://github.com/jaspervdj/digestive-functors) on GitHub.
Feedback is welcome, as always.
