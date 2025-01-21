# smlpkg [![CI](https://github.com/diku-dk/smlpkg/workflows/CI/badge.svg)](https://github.com/diku-dk/smlpkg/actions)

This program constitutes a generic package manager for Standard ML
libraries and programs. The package manager assumes nothing and knows
nothing about the Standard ML compilers used and is thus quite
generic.

The package manager is centered around the notion of [semantic versioning](https://semver.org/) and currently supports packages
hosted on [GitHub](https://github.com/) and
[GitLab](https://about.gitlab.com/).

The package manager takes care of downloading and upgrading dependent
packages and works well with the use of MLB files supported by
Standard ML compilers such as [MLton](http://mlton.org/),
[MLKit](http://elsman.com/mlkit/), and
[SMLtoJs](http://www.smlserver.org/smltojs/).

# Usage

## Adding a package

```
$ smlpkg add github.com/diku-dk/sml-random
```

This command modifies only `sml.pkg` and creates it if it does not already exist.

## Downloading required packages

```
$ smlpkg sync
```

This command populates the `lib` directory based on the packages listed in `sml.pkg`.

## Creating a new package

```
$ smlpkg init github.com/foo/bar
```

This command creates a file `sml.pkg` and initiates it with the given
package name (`foo` should be a github user name or an organisation
name and `bar` should be a repository name). You can now add code in the
directory `lib/github.com/foo/bar/`.

## Releasing a package

```
$ git tag vX.Y.Z
$ git push --tags
```

Remember to follow [semantic versioning](https://semver.org). Once a
package has been released, other packages can safely add the package
to their own source code tree using `smlpkg add` (see above).

## Probably incomplete list of available packages

* [![CI](https://github.com/diku-dk/futhark-data-sml/workflows/CI/badge.svg)](https://github.com/diku-dk/futhark-data-sml/actions) [github.com/diku-dk/futhark-data-sml](https://github.com/diku-dk/futhark-data-sml)
* [![CI](https://github.com/diku-dk/futhark-server-sml/workflows/CI/badge.svg)](https://github.com/diku-dk/futhark-server-sml/actions) [github.com/diku-dk/futhark-server-sml](https://github.com/diku-dk/futhark-server-sml)
* [![CI](https://github.com/diku-dk/sml-aplparse/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-aplparse/actions)
  [github.com/diku-dk/sml-aplparse](https://github.com/diku-dk/sml-aplparse)
* [![CI](https://github.com/diku-dk/sml-base64/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-base64/actions)
  [github.com/diku-dk/sml-base64](https://github.com/diku-dk/sml-base64)
* [![CI](https://github.com/diku-dk/sml-complex/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-complex/actions)
  [github.com/diku-dk/sml-complex](https://github.com/diku-dk/sml-complex)
* [![CI](https://github.com/diku-dk/sml-cstring/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-cstring/actions)
  [github.com/diku-dk/sml-cstring](https://github.com/diku-dk/sml-cstring)
* [![CI](https://github.com/diku-dk/sml-hashtable/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-hashtable/actions)
  [github.com/diku-dk/sml-hashtable](https://github.com/diku-dk/sml-hashtable)
* [![CI](https://github.com/diku-dk/sml-http/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-http/actions)
  [github.com/diku-dk/sml-http](https://github.com/diku-dk/sml-http)
* [![CI](https://github.com/diku-dk/sml-json/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-json/actions)
  [github.com/diku-dk/sml-json](https://github.com/diku-dk/sml-json)
* [![CI](https://github.com/diku-dk/sml-matrix/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-matrix/actions)
  [github.com/diku-dk/sml-matrix](https://github.com/diku-dk/sml-matrix)
* [![CI](https://github.com/diku-dk/sml-md5/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-md5/actions)
  [github.com/diku-dk/sml-md5](https://github.com/diku-dk/sml-md5)
* [![CI](https://github.com/diku-dk/sml-getopt/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-getopt/actions)
  [github.com/diku-dk/sml-getopt](https://github.com/diku-dk/sml-getopt)
* [![CI](https://github.com/diku-dk/sml-parse/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-parse/actions)
  [github.com/diku-dk/sml-parse](https://github.com/diku-dk/sml-parse)
* [![CI](https://github.com/diku-dk/sml-pickle/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-pickle/actions)
  [github.com/diku-dk/sml-pickle](https://github.com/diku-dk/sml-pickle)
* [![CI](https://github.com/diku-dk/sml-regexp/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-regexp/actions)
  [github.com/diku-dk/sml-regexp](https://github.com/diku-dk/sml-regexp)
* [![CI](https://github.com/diku-dk/sml-random/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-random/actions)
  [github.com/diku-dk/sml-random](https://github.com/diku-dk/sml-random)
* [![CI](https://github.com/diku-dk/sml-server/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-server/actions)
  [github.com/diku-dk/sml-server](https://github.com/diku-dk/sml-server)
* [![CI](https://github.com/diku-dk/sml-setmap/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-setmap/actions)
  [github.com/diku-dk/sml-setmap](https://github.com/diku-dk/sml-setmap)
* [![CI](https://github.com/diku-dk/sml-sort/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-sort/actions)
  [github.com/diku-dk/sml-sort](https://github.com/diku-dk/sml-sort)
* [![CI](https://github.com/diku-dk/sml-sha256/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-sha256/actions)
  [github.com/diku-dk/sml-sha256](https://github.com/diku-dk/sml-sha256)
* [![CI](https://github.com/diku-dk/sml-sobol/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-sobol/actions)
  [github.com/diku-dk/sml-sobol](https://github.com/diku-dk/sml-sobol)
* [![CI](https://github.com/diku-dk/sml-unicode/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-unicode/actions)
  [github.com/diku-dk/sml-unicode](https://github.com/diku-dk/sml-unicode)
* [![CI](https://github.com/diku-dk/sml-uref/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-uref/actions)
  [github.com/diku-dk/sml-uref](https://github.com/diku-dk/sml-uref)
* [github.com/pzel/assert-polyml](https://github.com/pzel/assert-polyml)
* [github.com/pzel/sml-either](https://github.com/pzel/sml-either)
* [github.com/pzel/sqlite3-polyml](https://github.com/pzel/sqlite3-polyml)
* [github.com/shwestrick/sml-audio](https://github.com/shwestrick/sml-audio)
* [github.com/shwestrick/sml-uri](https://github.com/shwestrick/sml-uri)
* [github.com/shwestrick/sml-parseq](https://github.com/shwestrick/sml-parseq)


## Design details

See this [blog post on the design of the Futhark package
manager](https://futhark-lang.org/blog/2018-08-03-the-present-futhark-package-manager.html).

# Compilation

To compile the package manager, you need a Standard ML compiler such
as [MLton](http://mlton.org/) or [MLKit](http://elsman.com/mlkit/).

## Compilation using MLKit on macOS

```
$ brew install mlkit
$ make all
```

## Compilation using MLton

```
$ MLCOMP=mlton make clean all
```

# License

This software is distributed under the [MIT LICENSE](LICENSE).

The package manager is almost a complete port of the Futhark
package manager, designed, and implemented in Haskell by Troels
Henriksen.
