# Style Guide for Scala Code

**In general, observe the style of existing code and respect it.**

## Formating

- Maximal line length is limited to 80 characters.

- Code blocks () should not exceed 30 lines.

## val & var

- Be explicit, use `var` only when really needed.

## class & trait

- When a class extending a trait,
  only `override` for method with default method,
  do not use `override` for abstract method of the trait.
