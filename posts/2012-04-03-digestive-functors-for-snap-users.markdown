---
title: digestive-functors for Snap users
description: Uploads for Snap backend, and a Heist library!
tags: haskell
---

The past week there's been a bit of work done on better integration between the
[digestive-functors] library and the [Snap framework].

[digestive-functors]: https://github.com/jaspervdj/digestive-functors/
[Snap framework]: http://snapframework.com/

[Daniel Patterson] has been so kind to implement file uploads for the Snap
backend of digestive-functors -- this was previously only implemented in the
[Happstack]. The great thing is that this implementation still has all flexible
options Snap allows for file uploads.

[Daniel Patterson]: http://dbpatterson.com/
[Happstack]: http://happstack.com/

For now, digestive-functors enforced you to use the [blaze-html] library, simply
because no other frontend library was available. Snap users will be delighted to
hear we've written a [Heist] library now. Let's take a look at an example. I'll
go over the forms rather quickly and focus on the Heist library. If this is the
first time you read about digestive-functors, you might want to take a glance at
[this tutorial] first.

[blaze-html]: http://jaspervdj.be/blaze
[Heist]: http://snapframework.com/docs/tutorials/heist
[this tutorial]: https://github.com/jaspervdj/digestive-functors/blob/master/examples/tutorial.lhs

First, we need some datatypes:

~~~~~{.haskell}
data Date = Date
    { dateDay   :: Int
    , dateMonth :: Int
    , dateYear  :: Int
    } deriving (Show)

data Sex = Female | Male
    deriving (Eq, Show)

data User = User
    { userName      :: Text
    , userPassword  :: Text
    , userSex       :: Sex
    , userBirthdate :: Date
    } deriving (Show)
~~~~~

And then some forms:

~~~~~{.haskell}
dateForm :: Monad m => Form Text m Date
dateForm = check "Not a valid date" validDate $ Date
    <$> "day"   .: stringRead "Not a number" (Just 16)
    <*> "month" .: stringRead "Not a number" (Just 6)
    <*> "year"  .: stringRead "Not a number" (Just 1990)
  where
    validDate (Date day month _) =
        day   >= 1 && day   <= 31 &&
        month >= 1 && month <= 12

userForm :: Monad m => Form Text m User
userForm = User
    <$> "name"      .: text (Just "Jasper")
    <*> "password"  .: text Nothing
    <*> "sex"       .: choice [(Female, "Female"), (Male, "Male")] Nothing
    <*> "birthdate" .: dateForm
~~~~~

Because of the composable nature of digestive-functors, we will first write a
template for the date form, and reuse that for the user form.

Let's place the template for the date form in `snaplets/heist/date-form.tpl`.
Almost all splices take a `ref` attribute, which tells the library what the
field is for -- by doing this, we automatically get the previous value if a form
submission fails, etc. We can also easily pass arbitrary attributes to the input
elements, like we do with `size` here.

~~~~~{.html}
<dfInputText ref="day" size="2" />
/
<dfInputText ref="month" size="2" />
/
<dfInputText ref="year" size="4" />
~~~~~

There's a bit more in `snaplets/heist/user-form.tpl`.

- `dfForm` generates a `<form>` tag, and takes care of the `method` and
  `enctype` attributes for us -- we just have to take care of `action`.

- We use `dfChildErrorList` with `ref=""` to generate a list of *all* errors. It
  is also possible to generate the errors next to the relevant fields, should
  you wish to do so.

- There's obviously more than `dfInputText` -- we have `dfInputPassword` here,
  and `dfInputSelect` to generate a combobox.

- In order to reuse the `date-form` template, we first use `dfSubView`. This
  changes the focus on the "form tree", so our `date-form` template can use
  `month` instead of `birthdate.month`. The `apply` splice is a standard Heist
  splice.

~~~~~{.html}
<dfForm action="/">
    <dfChildErrorList ref="" />

    <dfLabel ref="name">Name: </dfLabel>
    <dfInputText ref="name" />
    <br>

    <dfLabel ref="password">Password: </dfLabel>
    <dfInputPassword ref="password" />
    <br>

    <dfLabel ref="sex">Sex: </dfLabel>
    <dfInputSelect ref="sex" />
    <br>

    Birthday:
    <dfSubView ref="birthdate">
        <apply template="date-form" />
    </dfSubView> 
    <br>

    <dfInputSubmit value="Enter" />
</dfForm>
~~~~~

The `digestive-functors-heist` library provides no [Snaplet] but integrates well
with the concept. We just need to call `runForm` from the
`digestive-functors-snap` library, and most is taken care of:

[Snaplet]: http://snapframework.com/docs/tutorials/snaplets-tutorial

~~~~~{.haskell}
form :: Handler App App ()
form = do
    (view, result) <- runForm "form" userForm
    case result of
        Just x  -> -- do something with the user in x...
        Nothing -> heistLocal (bindDigestiveSplices view) $ render "user-form"
~~~~~

I hope this quick glance at the library was useful. Any comments and criticism
are welcome, as always. The full source code for this example can be found
[here](https://github.com/jaspervdj/digestive-functors/tree/master/examples)
(`snap-heist.hs`, and the templates in `snaplets/heist/templates`).
