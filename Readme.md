# Pier: Yet another Haskell build system.

Pier is a command-line tool for building Haskell projects.

[![Relevant XCKD](https://imgs.xkcd.com/comics/standards.png)](https://xkcd.com/927/)

Its main features:

- Hermetic builds: each build step runs in a temporary directory with a limited set of inputs
- All generated files are immutable (read-only) and stored within a single directory
- Uses `.cabal` files to configure local packages
- Use Stackage's package sets to specify dependencies
- Invokes `ghc` directly, reimplementing the build logic from (nearly) scratch
- Runs each build step hermetically and in parallel
- Built using [Shake](http://shakebuild.com)

For examples of project configuration, see the [sample](example/pier.yaml)
project, or alternately [pier itself](pier.yaml).

## Status
`pier` is still experimental.  It is currently able to build 88% of Stackage (2311 out of 2629 packages in `lts-10.3`).

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

## Project Configuration
A `pier.yaml` file specifies the configuration of a project.  For example:

```
resolver: lts-10.3
packages:
  - '.'
  - 'foo'
  - 'path/to/bar'
```

The `resolver` specifies a set of package versions (as well as a version of GHC), using [Stackage](https://stackage.org).  It can be either an LTS or nightly version.

The `packages` section lists paths to local directories containing Cabal packages (i.e., `*.cabal` and associated source files).

Additionally, an `extra-deps` section may be used to add new versions of packages from Hackage that are not in the `resolver`, or to override existing versions.  For example:

```
extra-deps:
  - text-1.2.3.4
  - shake-0.15
```

# Usage

For general comnmand-line usage, pass the `--help` flag:

```
pier --help
pier build --help
pier run --help
# etc.
```

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

### Common Options

| Option | Result | Default |
| --- | --- | --- |
| `--pier-yaml={PATH}` | Use that file for build configuration | `pier.yaml` |
| `--jobs={N}`, `-j{N}` | Run with at most this much parallelism | The number of detected CPUs |
| `-V` | Increase the verbosity level | |
| `--shake-arg={ARG}` | Pass the argument directly to Shake | |

# Output File Locations

`pier` saves most output files in a directory called `_pier`, located in the
same directory as `pier.yaml`.  That output directory can generally be ignored
(for example, with a `.gitignore` directive).

Each individual command's outputs (for example, from a single invocation ofv
`ghc`) live in a separate directory `_pier/artifact/{HASH}`.  The `{HASH}` is
a string that depends on the command's arguments and inputs, as well as its dependencies
(commands that generated its inputs).

If necessary, `pier clean-all` will delete the `_pier` folder (and thus wipe out the entire build).  That folder can also be deleted manually with `chmod -R u+w _pier && rm -rf _pier`.  (Files and folders in `_pier` are marked as read-only.)

Downloaded files (for example, package release tarballs) are saved under `$HOME/.pier`, so that they may be reused between different projects on the same machine.

# FAQs

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
