# fortran_float_num_extract

A command-line tool written for [**PRIMA #302**](https://github.com/libprima/prima/pull/302)
that scans Fortran source code to identify floating-point literals lacking a
precision kind suffix and not exactly representable in binary floating-point
format.

In Fortran, an unsuffixed literal like `0.1` is of type `default real` (usually
32-bit single precision). When `RP` is configured as 64-bit double precision,
comparing or combining `real(RP)` variables with unsuffixed literals triggers an
implicit type conversion. Worse, `0.1` in single precision and `0.1_RP` in double
precision are different binary approximations. This means a condition such as

```fortran
if (rand() <= 0.1) then
```

may evaluate differently in rare cases.

This tool automates the detection of such literals so that they can
be corrected (e.g., `0.1` → `0.1_RP`) by hand.

## How it works

1. parses Fortran source files into an AST using
   [`fortran-src`](https://hackage.haskell.org/package/fortran-src),
2. traverses the AST to find all real/integer literals without a kind
   suffix,
3. checks whether those literals can be exactly represented in binary form
   and reports them if not.

## Building

Requires [GHC](https://www.haskell.org/ghc/) ≥ 9.0 and [Cabal](https://www.haskell.org/cabal/) ≥ 3.0.

```bash
git clone https://github.com/chenyu76/fortran_float_num_extract.git
cd fortran_float_num_extract
cabal build
```

## Usage

```bash
cd fortran_float_num_extract
cabal run fortran_float_num_extract -- /path/to/identify
```
