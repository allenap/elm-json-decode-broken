# elm-json-decode-broken


## Decode broken JSON in Elm.

Sometimes a 3rd party publishes or gives you some slightly broken JSON. Newlines
([U+000A](https://www.compart.com/en/unicode/U+000A)) and/or carriage returns
([U+000D](https://www.compart.com/en/unicode/U+000D)) in strings, for example. A
proper JSON parser will rightly complain that it's broken, but you still need to
get stuff done.

This package will allow you to compose a custom parser that allows for weirdness
in the JSON you have to work with. I say *will* because right now it's just a
specification compliant parser, albeit implemented in Elm using [elm/parser][].
There's not yet the facility for composing a custom parser, though the pieces
are there.

One option open to you is to take the code as it stands, copy-and-paste it into
your project, change it to work with the broken JSON you're forced to deal with,
and go from there. Sure, you won't get bug fixes and you'll have to maintain it
yourself, but that's in the spirit of this package: get your work done, move on.
But do go back to the source of the broken JSON and ask them to fix the issue,
then you can use one of the regular JSON decoders in the Elm universe, like
[elm/json][].


[elm/parser]: https://package.elm-lang.org/packages/elm/parser/latest/
[elm/json]: https://package.elm-lang.org/packages/elm/json/latest/


## To do.

- Consider switching from our own custom type to represent JSON values to
  `JsonValue` from [json-tools/json-value][]. **Or** provide a converter.


[json-tools/json-value]: https://package.elm-lang.org/packages/json-tools/json-value/latest/
