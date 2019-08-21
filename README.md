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

I will add a complete demo Hakyll site to this repository eventually. Here's a
snippet showing how to integrate:

```
import           IkiWiki
…
    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ do
            getResourceBody
            >>= return . fmap handleWikiLinks
            >>= renderPandoc
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
```
