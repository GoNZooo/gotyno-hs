# Changelog

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