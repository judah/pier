# Pier: Yet another Haskell build system.

[![Relevant XCKD](https://imgs.xkcd.com/comics/standards.png)](https://xkcd.com/927/)

# FAQs

### I heard you like `pier`, so I built `pier` with `pier`
Building `pier` with `pier` is OK, I guess:

    stack build pier
    stack exec -- pier build pier

But what about using **that** `pier` to build `pier`?  We just need to
distinguish Shake's metadata between the two invocations:

    $ stack exec -- pier -- run -- pier build pier --shake-arg=--metadata=temp-metadata
    Build completed in 0:10m

    Build completed in 0:10m

The inner run of `pier build` is able to reuse all of the build outputs that
were created by the outer call to `pier` (and stored under `_pier/artifacts`),
so it only takes about 10 seconds on my laptop.  It spends that time parsing
package metadata, computing dependencies, and (re)creating all the build
commands in the dependency tree.
