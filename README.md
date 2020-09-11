# Hariko

> Hariko: Property-Based-Testing for **Fun** and Functional Programming

## About

Hariko is a property-based-testing library written in Scala.
It is inspired by [QuickCheck](https://hackage.haskell.org/package/QuickCheck) and [Hedgehog](https://hedgehog.qa).
Bonus, its name means "hedgie" in Japanese.

Highlighted Features:

- *Simple, but Powerful*

  To write property testing, you need to know an API only: `Property.check`.
  But you can know useful APIs like `Gen` if you need.
  For instance, `Gen` can customize its generating values easily.

- *Integrated Shrinking*

  Shrinking is integrated with generating values.
  So, you always get the simplified counter example when a property is failed.
  Of course, it is done even if the generator is customized.

- *Higher-Order*

  It is the most exciting feature of Hariko!
  Hariko supports generating a higher-order function accepts functions.
  It helps to write complex law checking.

## License

MIT License.

2020 (C) TSUYUSATO "MakeNowJust" Kitsune
