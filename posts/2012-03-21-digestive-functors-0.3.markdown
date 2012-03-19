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
