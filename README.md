<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/obgs-mark-dark.svg">
  <img alt="" src="assets/obgs-mark-light.svg" width="56" align="left" hspace="14" vspace="4">
</picture>

# obgs-type-level

**Type families and first-class families for type-level programming.**

Built on top of [`first-class-families`][fcf], this package collects the
type-level machinery the OBGS packages are written with: first-class families
for the standard types and classes, the ordinary type families beneath them,
helpers for building readable compile-time errors, and type-level counterparts
of `Finitary` and of the `Integral` types.

It is self-contained. Nothing here depends on the rest of OBGS, and it is meant
to be useful on its own.

## What's inside

The modules fall into five groups.

| Modules | Contents |
| --- | --- |
| `Obgs.Fcf.*` | First-class families for working with the standard types and classes, adding branching, constraints, and `Enum`, `Num` and `Finitary` type-level counterparts. |
| `Obgs.TL.*` | Type utilities for working with lists, tuples, `Maybe`s, `Nat`s, `Constraint`s and `Type`s. |
| `Obgs.TL.Error` | Type utilities for building `ErrorMessage`s. |
| `Obgs.TL.Finitary` | A type-level variant of `Data.Finitary.Finitary`. |
| `Obgs.TL.Integral` | A representation of `Integral` numbers that can be promoted to the type level. |

## Example

Everything is written to be evaluated with `:kind!`.

```haskell
>>> :kind! Eval (Compare 2 1)
GT

>>> :kind! Eval (Nub '[1, 2, 1, 3, 2])
[1, 2, 3]

>>> :kind! Eval (IsBetween 1 5 3)
True

>>> :kind! Eval (Succ 1)
2
```

`Obgs.TL.Integral` is the piece least like the rest. Where `GHC.TypeNats.Nat`
is unsigned and unbounded, `TypeIntegral` follows the type it wraps, so
`TypeIntegral Int8` really does run from `-128` to `127` and overflows the way
`Int8` does. Its `Num`, `Integral`, `Bounded`, `Enum` and `Finitary` instances
are property-checked against their term-level counterparts.

## Building

```sh
cabal build
cabal test
cabal haddock
```

## Requirements

GHC 9.12.2 and cabal-install 3.16. The `base` and `template-haskell` bounds
pin the compiler, so no other GHC is currently supported.

## OBGS

OBGS is a set of packages for describing type-safe board game score sheets
declaratively.

| Package | Role |
| --- | --- |
| **`obgs-type-level`** | Type families and first-class families. |
| `obgs-data` | Data structures: `HList`, `HVector`, `Range`, `RangeSet`, `Subset`. |
| `obgs-control` | `Matcher`, `Formula`, `Indexed`, and effect helpers. |
| `obgs-form` | The language itself: entities, selectors, formulas, option sets, stores. |

`obgs-type-level` is the only one released so far. The others are in progress
and are not yet on Hackage.

## License

MIT. See [LICENSE](LICENSE).

[fcf]: https://hackage.haskell.org/package/first-class-families
