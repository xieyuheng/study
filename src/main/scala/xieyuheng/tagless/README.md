# Typed Tagless Final Interpreters

- By Oleg Kiselyov

## About the title

It is all about implementing languages in metalanguages.
To implement a target language,
we need to encode the syntax of the target language in the metalanguage.

"Final" means "Final encoding":

| Inital encoding    | Final encoding |
|--------------------|----------------|
| algebraic datatype | typeclass      |
