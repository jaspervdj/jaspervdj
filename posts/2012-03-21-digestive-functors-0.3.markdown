---
title: digestive-functors 0.3
description: Another formlets upgrade
tags: haskell
---

I've just released digestive-functors 0.3, which is a major rehaul of my
formlets library. It has a number of great features which (as far as I know)
have never been implemented for any formlets library.

## What are formlets?

In 2008, a paper was published, called ["The Essence of Form Abstraction"]. The
paper applied a well-known functional design pattern (Applicative Functors) to
have clean way of creating HTML forms, which are *inherently composable*.

["The Essence of Form Abstraction"]: http://groups.inf.ed.ac.uk/links/formlets/

Let's have a quick look at how this typically works. The following code is based
on [Chris Eidhof]'s [initial Haskell implementation] of formlets. We usually have
a data structure we want to create a type for:

[Chris Eidhof]: http://eidhof.nl/
[initial Haskell implementation]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/formlets

~~~~~{.haskell}
data Date = Date {month :: Integer, day :: Integer} deriving (Show)
~~~~~

...for which we create a form...

~~~~~{.haskell}
validDate :: Date -> Bool
validDate (Date m d) = m `elem` [1..12] && d `elem` [1..31]

dateComponent :: FailingForm Date
dateComponent = Date <$> inputIntegerF (Just 1) <*> inputIntegerF (Just 16)

dateFull :: FailingForm Date
dateFull = dateComponent `check` ensure validDate "This is not a valid date"
~~~~~

And to illustrate the power of any formlets library, it's composability, let's
see how you can easily reuse the `dateFull` form anywhere:

~~~~~{.haskell}
data User = User {name :: String, password :: String, birthdate :: Date}
    deriving (Show)
 
userFull :: FailingForm User
userFull = User <$> inputF Nothing <*> passwordF Nothing <*> dateFull
~~~~~

## digestive-functors

While I was working on a few patches for the formlets library back in 2010, I
noticed a few things were impossible to do using this library.

For example, For usability reasons, you might want to add `<label>` tags in your
forms. These labels need to be associated with an input element using the `for`
attribute -- when this is the case, the user will be able to click the label
instead of the (relatively small) checkbox. Demo:

<form>
    <label>Without <code>for</code> attribute:</label>
    <input type="checkbox" id="checkbox1" />
</form>
<form>
    <label for="checkbox2">With <code>for</code> attribute:</label>
    <input type="checkbox" id="checkbox2" />
</form>

Another thing is error handling. When evaluating a form using the original
formlets library, you would get a list of errors, and the user would see
something like this:

<div style="color: red; text-weight: bold">Cannot parse age</div>
<form>
    <label for="text1">Name:</label>
    <input type="text" id="text1" value="Jasper" />
</form>
<form>
    <label for="text2">Age:</label>
    <input type="text" id="text2" value="hello" />
</form>

While this is usually pretty clear, it is nicer when the library can actually
associate the errors with input field(s), so you are able to show something
like:

<form>
    <label for="text3">Name:</label>
    <input type="text" id="text3" value="Jasper" />
</form>
<form>
    <label for="text4">Age:</label>
    <input type="text" id="text4" value="hello" />
    <span style="color: red; text-weight: bold">Cannot parse age</span>
</form>

Along with some other things, these were the practical improvements the
digestive-functors library made in comparison to formlets.

## digestive-functors 0.3

However, one serious issue remained. When you write down a form in a formlets
library, you specify the *HTML layout* as well as *the validation rules*. This
is really a bad thing: sepataration of model and view is a well known goal in
programming.

Separating the HTML layout and the validation rules would lead to a number of
benefits:

- It would be possible to create multiple representations for a single form (a
  good example is a login form in the site header, and a login form in the page
  body, which you see find on many websites).

- The code to specify the validation rules becomes smaller and easier to read.

- It becomes easier to insert custom HTML code in between your form HTML.

- You can rewrite the form using another HTML templating engine, e.g.
  blaze-html, Hamlet or Heist, without touching the validation rules.

However, it does seem to come with a serious disadvantage as well:

- It seems impossible [^impossible] to have a type-safe coupling between
  validation rules and HTML layout without losing flexibility or ease-of-use.

[^impossible]: I've thought about this for some time, and haven't found a way
    to do it, and discovered many problems with the different approaches one
    could take. The reasoning behind these is outside of the scope of this
    blogpost, but I'd be happy to elaborate if anyone is interested.

In order to make the coupling between the validation rules and the HTML layout,
digestive-functors-0.3 uses simple `Text` values. An example of some validation
rules:

~~~~~{.haskell}
dateForm = check "This is not a valid date" validDate $ Date
    <$> "month" .: stringRead "Could not parse month"
    <*> "day"   .: stringRead "Could not parse day"
~~~~~

And we can write an HTML layout for it using e.g. blaze-html. The code for this
is a bit verbose (HTML always is), but clear, and it's possible to add some
utility combinators for it:

~~~~~{.haskell}
userView :: View Html -> Html
userView view = do
    errorList "month" view
    label     "month" view "Name: "
    inputText "month" view
    H.br

    errorList "day" view
    label     "day" view "Email address: "
    inputText "day" view
    H.br
~~~~~
