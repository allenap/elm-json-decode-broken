# elm-json-decode-broken


## Decode broken JSON in Elm.

Sometimes a 3rd party publishes or gives you some slightly broken JSON. Newlines
([U+000A](https://www.compart.com/en/unicode/U+000A)) and/or carriage returns
([U+000D](https://www.compart.com/en/unicode/U+000D)) in strings, for example. A
proper JSON parser will rightly complain that it's broken, but you still need to
get stuff done.

This package allows you to compose a custom parser that allows for weirdness in
the JSON you have to work with. Check out the tests for an example of how to do
that. Without customisation it's a specification compliant parser, implemented
in Elm using [elm/parser][].

If the customisation hooks are not enough or you find them unwieldy there's
another option open to you: take the code as it stands, copy-and-paste it into
your project, change it to work with the broken JSON you're forced to deal with,
and go from there. Sure, you won't get bug fixes and you'll have to maintain it
yourself, but that's in the spirit of this package: get your work done, move on.
But do go back to the source of the broken JSON and ask them to fix the issue,
then you can use one of the regular JSON decoders in the Elm universe, like
[elm/json][].


[elm/parser]: https://package.elm-lang.org/packages/elm/parser/latest/
[elm/json]: https://package.elm-lang.org/packages/elm/json/latest/


## To do.

- Consider changing the parser to use [`Parser.Advanced`][Parser.Advanced] which
  allows more control over errors.


[json-tools/json-value]: https://package.elm-lang.org/packages/json-tools/json-value/latest/
[Parser.Advanced]: https://package.elm-lang.org/packages/elm/parser/latest/Parser-Advanced
