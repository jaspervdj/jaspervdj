---
title: digestive-functors 0.3
description: Another formlets upgrade
tags: haskell
---

I've just released digestive-functors 0.3, which is a major rehaul of my
formlets library. It has a number of great features which (as far as I know)
have never been implemented for any formlets library.

This blogpost is very general, so some users might want to jump directly to the
[tutorial]. Installation is through cabal: `cabal install digestive-functors`.

[tutorial]: http://github.com/jaspervdj/digestive-functors/blob/master/examples/tutorial.lhs

# What are formlets?

In 2008, a paper was published, called ["The Essence of Form Abstraction"]. The
paper applied a well-known functional design pattern (Applicative Functors) to
have clean way of creating HTML forms, which are *inherently composable*.

["The Essence of Form Abstraction"]: http://groups.inf.ed.ac.uk/links/formlets/

Let's have a quick look at how this typically works. The following code is based
on the [initial Haskell implementation] of formlets by the paper authors, later
maintained by [Chris Eidhof] and others. We usually have a data structure we
want to create a type for:

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

# digestive-functors

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

# digestive-functors 0.3

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

A major difference you can immediately notice is that we use `Text` labels
(`"month"`, `"day"`) and use the custom `.:` operator to assign these to parts
of our form. We will use these labels to refer to fields in the HTML layout.

Form composition is obviously still very important. Let's give an example by
implemening `userForm` using `dateForm`:
 
~~~~~{.haskell}
userForm = User
    <$> "name"      .: string Nothing
    <*> "password"  .: string Nothing
    <*> "birthdate" .: dateForm
~~~~~

One important difference between `userFull` and `userForm` is that we used
`string` twice here -- where the original one used `inputF` and `passwordF`.
This is a result of the separation of concerns. After all, a password is just a
string: it is up to the view to represent it as a password box.

Let's look at the views now and write an HTML layout using e.g. blaze-html. The
code for this is a bit verbose (HTML always is), but clear, and it's possible to
add some utility combinators for it:

~~~~~{.haskell}
dateView view = do
    errorList "month" view
    label     "month" view "Month: "
    inputText "month" view
    H.br

    errorList "day" view
    label     "day" view "Day: "
    inputText "day" view
    H.br
~~~~~

Views are composable as well, which is very important. A developer might want to
inline a view, e.g.:

~~~~~{.haskell}
userView view = do
    errorList "name" view
    label     "name" view "Name: "
    inputText "name" view
    H.br

    errorList     "password" view
    label         "password" view "Password: "
    inputPassword "password" view
    H.br

    errorList "birthdate.month" view
    label     "birthdate.month" view "Month: "
    inputText "birthdate.month" view
    H.br

    errorList "birthdate.day" view
    label     "birthdate.day" view "Day: "
    inputText "birthdate.day" view
    H.br
~~~~~

A few things to note. While `"name"` and `"password"` are both of the same type
(`String`) we chose to use a textbox for the former and a password box for the
latter: this is a possibility we gain because of the separation we made. A
(probably more useful) example is when the user has to choose between a number
of options (e.g. Apples, Oranges or Bananas), we can decide in the view code
whether we want to use a combobox or a set of radio buttons.

We use a `"foo.bar"` notation to refer to fields of "subforms". This is useful
when a designer wants a custom form layout, but it leads to duplication of code.
To counter this, views are composable, just like forms!

~~~~~{.haskell}
userView view = do
    -- Name, password...

    dateView $ subView "birthdate" view
~~~~~

This concludes this blogpost about the digestive-functors 0.3 release.

Note that I have omitted types and other details -- you can find everything in
this [tutorial]. The digestive-functors library provides a very easy interface
for writing these view libraries: you can basically query the previous input,
errors, etc. for each field. This makes it very easy to add libraries for e.g.
Hamlet or Heist (but I haven't done so yet, if anyone is interested, contact
me!).
