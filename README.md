<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/obgs-mark-dark.svg">
  <img alt="" src="assets/obgs-mark-light.svg" width="56" align="left">
</picture>

# obgs-type-level

**Type families and first-class families for type-level programming.**

A package providing the type-level foundation for the **OBGS** project (**O**pen **B**oard **G**ame **S**heets), a suite of Haskell packages for declaratively describing board game score sheets and evaluating their rules with static type safety.

---

## What's Inside

The modules fall into five main groups:

| Modules | Contents |
| --- | --- |
| `Obgs.Fcf.*` | First-class families built on top of [`first-class-families`](https://hackage.haskell.org/package/first-class-families) for higher-order type-level computations. |
| `Obgs.TL.*` | Ordinary closed type families operating on promoted values. |
| `Obgs.TL.Integral` | A representation of arbitrary numerical types (signed or unsigned, bounded or unbounded) promoted to the type level. |
| `Obgs.TL.Finitary` | A type-level counterpart of `Data.Finitary.Finitary` for computing cardinality and enumerating finite inhabitants. |
| `Obgs.TL.Error` | Utilities for composing structured, readable `ErrorMessage` trees for compile-time diagnostics. |

---

## Building and Testing

`obgs-type-level` requires **GHC 9.12** or **GHC 9.14**, and `cabal-install` **3.12** or later.

```sh
cabal build obgs-type-level
cabal test obgs-type-level
cabal haddock obgs-type-level
```

---

## License

MIT. See [LICENSE](LICENSE).
