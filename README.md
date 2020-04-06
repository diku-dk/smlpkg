# smlpkg [![Build Status](https://travis-ci.org/diku-dk/smlpkg.svg?branch=master)](https://travis-ci.org/diku-dk/smlpkg)

This program constitutes a generic package manager for Standard ML
libraries and programs. The package manager assumes nothing and knows
nothing about the Standard ML compilers used and is thus quite
generic.

The package manager is centered around the notion of [semantic
versioning](https://semver.org/) and currently supports packages
hosted on [GitHub](https://github.com/) and
[GitLab](https://about.gitlab.com/).

The package manager takes care of downloading and upgrading dependent
packages and works well with the use of MLB files supported by
Standard ML compilers such as [MLton](http://mlton.org/),
[MLKit](http://elsman.com/mlkit/), and
[SMLtoJs](http://www.smlserver.org/smltojs/).

# Usage

See also Troels Henriksen's [blog post on the design of the Futhark
package
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
