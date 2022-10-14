# Changelog

## 2.2.1

### Fixes

- Added trailing newline in Kotlin output

## 2.2.0

### Added

- More tests for TypeScript output; the "basic" suite

### Fixes
- Fixed bug where TypeScript's optional parameters to constructors would not
  use `?`, i.e. `function Constructor(data?: string)`.

## 2.1.0

### Fixes

- Made it so Kotlin started using types from enum definitions instead of `Any`.

## 2.0.0

### Breaking changes

- Enums can now only contain the same type. This simplifies their usage greatly
  in languages that either force you or allow you to choose a backing type for
  your enum values. It's a breaking change because of the lack of flexibility
  caused by enforcing only one type, but I think that it's a reasonable change
  overall.

### Added DLang

D was added as a beta language. It needs to be tested in production.

## 1.1.0

### General

- Made the parser much less strict, allowing extra whitespace in many places.
  This is experimental for now, but should allow people to have just a bit more
  of their own style.

### Features

### Fixes

- Added checks for whether or not the user is applying enough/too many type
  parameters to the usage of a given definition/declaration. This was previously
  punted to the output language but if we are to output to languages with very
  poor static checks themselves, this safeguard is good to have.

## 1.0.2

### General

- Uploaded package to Stackage

### Features

### Fixes

- Clearing definitions & declarations on module parsing success *and* failure
- Using debouncing to handle duplicate file events when using `--watch`
