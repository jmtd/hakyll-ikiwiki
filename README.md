# Hakyll-IkiWiki

Experimental code for interpreting IkiWiki-format wiki-links and directives
in a Hakyll site.

I'm exploring whether/how easily I could migrate from
[IkiWiki](http://ikiwiki.info) to [Hakyll](https://jaspervdj.be/hakyll/). I'm
keen to continue to use the IkiWiki link syntax (`[[example]]`), as well as
some of the features that are obtained via plugins via directives (`[[!example
label=value]]`), and IkiWiki's link-resolving logic.

The wikilink-and-directive finding is implemented as a Parsec parser, that
should run prior to the content being passed to Pandoc. This can't easily be
achieved with a Pandoc filter (operating on the Pandoc AST) because the tokens
we want might be split up across any kind of different text nodes. The approach
we use is to substitute, where possible, the link syntax with a HTML link.
Pandoc then later will interpret the result. This means that thing will only
work if the underlying markup language supports in-line HTML. Markdown does,
Textile does, HTML obviously does, restructuredText does not.

I've begun implementing logic for some IkiWiki plugins in the Directive handling.
Presently they can generate plain text or updated Metadata. I still need to figure
out how to plumb the collated Metadata back into Hakyll.

I want to support IkiWiki's `[[!template]]`, where possible, and plan to see if
translating them into Hakyll's template tokens (e.g.
`$partial("filename.html")$`) as inline text works or not.

## Example in use

The `site.hs` is a simple example Hakyll program, derived from the basic template
that `hakyll-init` generates for you. I've trimmed it a little bit and integrated
`IkiWiki.hs` into it. The content in `css` and `images` is unchanged; `posts` has
blog posts that demonstrate IkiWiki-style interlinks and directives in use.

`hakyll-ikiwiki.cabal` and `stack.yaml` are (for now) specific to this
demonstration. You can use either [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
or [Cabal](https://www.haskell.org/cabal/) to build the demo site.
