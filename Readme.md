# Pier: Yet another Haskell build system.

Pier is a command-line tool for building Haskell projects.  (Yes,
[another one](https://xkcd.com/927).)

Pier is similar in purpose to [Stack](https://www.haskellstack.org); it
uses `*.cabal` files for package configuration, and uses Stackage for
consistent sets of package dependencies.  However, Pier attempts to
address some of Stack's limitations by exploring a different approach:

- Pier invokes tools such as `ghc` directly, implementing the fine-grained
  Haskell build logic from (nearly) scratch.  In contrast, Stack relies on a
  separate framework to implement most of its build steps (i.e.,
  `Cabal`/`Distribution.Simple`), giving it mostly coarse control over the build.
- Pier layers its Haskell-specific logic on top of a general-purpose
  library for hermetic, parallel builds and dependency tracking.  That library
  is itself implemented using [Shake](http://shakebuild.com), and motivated by
  tools such as [Nix](https://nixos.org/nix) and [Bazel](https://bazel.build).
  In contrast, Stack's build and dependency logic is more specific to
  Haskell projects.

(Interestingly, Stack originally did depend on Shake.  The project stopped using it
early on, in part due to added complexity from the extra layer of Cabal build
logic.  For more information, see write-ups by authors of
[Stack](https://groups.google.com/d/msg/haskell-stack/icN7M0tJgxw/obPPZUVeAgAJ)
and
[Shake](http://neilmitchell.blogspot.com/2016/07/why-did-stack-stop-using-shake.html).)

For examples of project configuration, see the [sample](example/pier.yaml)
project, or alternately [pier itself](pier.yaml).

## Status
Pier is still experimental.  It has been tested on small projects, but not yet used in anger.

Pier is already able to build most the packages in Stackage (specifically, 90% of
the more than 2600 packages in `lts-10.3`). There is a
[list of open issues](https://github.com/judah/pier/issues?q=is%3Aissue+is%3Aopen+label%3A%22Build+All+The+Packages%22)
to increase Pier's coverage.  (Notably, packages with [Custom Setup.hs scripts](https://github.com/judah/pier/issues/22)
are not supported.)

## Contents

- [Installation](#installation)
- [Project Configuration](#project-configuration)
- [Using pier](#using-pier)
- [Build Outputs](#build-outputs)
- [Frequently Asked Questions](#frequently-asked-questions)

# Installation
First clone this repository, and then build and install the `pier` executable using `stack` (version 1.6 or newer):

```
git clone https://github.com/judah/pier.git
cd pier
stack install
```

Add `~/.local/bin` to your `$PATH` in order to start using `pier`.  For example, try:

```
cd example
pier build
```

# Project Configuration
A `pier.yaml` file specifies the configuration of a project.  For example:

```
resolver: lts-10.3
packages:
  - '.'
  - 'foo'
  - 'path/to/bar'
```

### resolver
The `resolver` specifies a set of package versions (as well as a version of GHC), using [Stackage](https://stackage.org).  It can be either an LTS or nightly version.  For example:

```
resolver: lts-10.3
```
```
resolver: nightly-2018-02-10
```

### packages
The `packages` section lists paths to local directories containing Cabal packages (i.e., `*.cabal` and associated source files).  For example:

```
packages:
  - '.'
  - 'foo'
  - 'path/to/bar'
```

### extra-deps
An `extra-deps` section may be used to add new versions of packages from Hackage that are not in the `resolver`, or to override existing versions.  For example:

```
extra-deps:
  - text-1.2.3.4
  - shake-0.15
```

### system-ghc
By default, pier downloads and installs its own, local copy of GHC from
`github.com/stackage`.  To override this behavior and use a GHC that's already
installed on the system, set:

```
system-ghc: true
```

This setting will make `pier` look in the `$PATH`
for a binary named `ghc-VERSION`, where `VERSION` is the version specified in the
resolver (for example: `ghc-8.2.2`).

# Using `pier`

For general comnmand-line usage, pass the `--help` flag:

```
pier --help
pier build --help
pier run --help
# etc.
```
## Common Options

| Option | Result | Default |
| --- | --- | --- |
| `--pier-yaml={PATH}` | Use that file for build configuration | `pier.yaml` |
| `--jobs={N}`, `-j{N}` | Run with at most this much parallelism | The number of detected CPUs |
| `-V` | Increase the verbosity level | |
| `--shake-arg={ARG}` | Pass the argument directly to Shake | |
| `--keep-temps` | Preserve temporary directories | False |
| `--download-local` | Save downloads under the local `_pier` instead of `$HOME/.pier` | False |



### `pier build`

`pier build {TARGETS}` builds one or more Haskell libraries and/or binaries from the project, as well as their dependencies.  There are a few different ways to specify the targets:

| Command | Targets |
| --- | --- |
| `pier build` | All the libraries and executables for every entry in `packages`. |
| `pier build {PACKAGE}` | The library and executables (if any) for the given package.  For example: `text` or `pier`.  `{PACKAGE}` can be a local package, one from the LTS, or one specified in `extra-deps`. |
| `pier build {PACKAGE}:lib` | The library for the given package. |
| `pier build {PACKAGE}:exe` | The executables for the given package, but not the library (unless it is a dependency of one of them). |
| `pier build {PACKAGE}:exe:{NAME}` | A specific executable in the given package. |

### `pier run`
`pier run {TARGET} {ARGUMENTS}` builds the given target, and then runs it with the given command-line arguments.  `{TARGET}` should be a specific executable (for example, `pier:exe:pier`), which may be elided to a package if it contains an executable of the same name (for example, `pier`).

By default, the command will run in the same directory where `pier.yaml` is located.  To run in a temporary, hermetic directory, use `pier run --sandbox`.

In case of ambiguity, `--` can be used to separate arguments of `pier` from arguments of the target.

### `pier clean`
`pier clean` marks some metadata in the Shake database as "dirty", so that it will be recreated on the next build.  This command may be required if you build a new version of `pier`, but should be unnecessary otherwise.

### `pier clean-all`
`pier clean-all` completely deletes all build outputs (other than downloaded
files, as described [here](#build-outputs)), so that future builds will start
from scratch.  Note that this command will require Pier to reinstall a local
copy of GHC unless `system-ghc: true` is set.

# Build Outputs

`pier` saves most output files in a folder called `_pier/`, located in the
same directory as `pier.yaml`. The only exception is downloaded files (for
example, package tarballs for dependencies), which are saved under
`$HOME/.pier` so that they may be reused between different projects on the same
machine.

Each build command (for example, a single invocation of `ghc` or
`ghc-pkg`) runs separately in a temporary directory with a limited, explicit
set of input files.  This approach is inspired by the `Bazel` project, which
sandboxes each command in order to get reliable, deterministic builds.
Note though that Pier does not currently provide the same strict guarantees
as Bazel.  Instead, it uses file organization and marking outputs as
read-only to catch a subset of potential bugs in the build logic.

The outputs of each command are saved into a distinct directory of the form:

    `_pier/artifact/{HASH}`

where the `{HASH}` is a unique string depending on the command's command-line
arguments and input dependencies.  This file organization is similar to Nix,
though Pier aims for much more fine-grained build steps than a standard Nix
package.

If necessary, `pier clean-all` will delete the `_pier` folder (and thus wipe out the entire build).  That folder can also be deleted manually with `chmod -R u+w _pier && rm -rf _pier`.  (Files and folders in `_pier` are marked as read-only.)

# Frequently Asked Questions

### How much of Cabal/Stack does this project re-use?

`pier` implements nearly all build logic itself, including: configuration, dependency tracking, and invocation of command-line programs such as `ghc` and `ghc-pkg`.  It uses Cabal/Hackage/Stackage in the follow ways:

- Downloads Stackage's build plans from `github.com/fpco`, and uses them to get the version numbers for the packages in that plan and for GHC.
- Downloads GHC releases from `github.com/commercialhaskell`, getting the exact download location from a file hosted by `github.com/stackage`.
- Downloads individual Haskell packages directly from Hackage.
- Uses the `Cabal` library to parse the `.cabal` file format for each package, and to generate CPP version macros (for example, `MIN_VERSION_{package}`).

In particular, it does not:

- Call the `stack` executable or depend on the `stack` library
- Call the `cabal` binary
- Import `Distribution.Simple{.*}` from the `Cabal` library


### I heard you like `pier`, so I built `pier` with `pier`.
Building `pier` with `pier` is OK, I guess:

    pier build pier

But what about using *that* `pier` to build `pier`?  We'll just need to
distinguish Shake's metadata between the two invocations:

    $ pier -- run pier build pier \
            --shake-arg=--metadata=temp-metadata
    Build completed in 0:10m

    Build completed in 0:10m

The inner run of `pier build` only takes about 10 seconds on my laptop, because it reuses all of the build outputs that
were created by the outer call to `pier` (and were stored under `_pier/artifacts`).  It spends its time parsing
package metadata, computing dependencies, and (re)creating all the build
commands in the dependency tree.
