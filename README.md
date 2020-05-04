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

## Adding a package

```
$ smlpkg add github.com/diku-dk/sml-random
```

This modifies only `sml.pkg`.

## Downloading required packages

```
$ smlpkg sync
```

This populates the `lib` directory based on the packages listed in `sml.pkg`.

## Creating a new package

```
$ smlpkg init github.com/foo/bar
```

Put all code in the directory `lib/github.com/foo/bar/`.

## Releasing a package

```
$ git tag vX.Y.Z
$ git push --tags
```

Remember to follow [semantic versioning](https://semver.org).

## Probably incomplete list of available packages

* [![Build Status](https://travis-ci.org/diku-dk/sml-hashtable.svg?branch=master)](https://travis-ci.org/diku-dk/sml-hashtable)
  [github.com/diku-dk/sml-hashtable](https://github.com/diku-dk/sml-hashtable)
* [![Build Status](https://travis-ci.org/diku-dk/sml-json.svg?branch=master)](https://travis-ci.org/diku-dk/sml-json)
  [github.com/diku-dk/sml-json](https://github.com/diku-dk/sml-json)
* [![Build Status](https://travis-ci.org/diku-dk/sml-md5.svg?branch=master)](https://travis-ci.org/diku-dk/sml-md5)
  [github.com/diku-dk/sml-md5](https://github.com/diku-dk/sml-md5)
* [![Build Status](https://travis-ci.org/diku-dk/sml-pickle.svg?branch=master)](https://travis-ci.org/diku-dk/sml-pickle)
  [github.com/diku-dk/sml-pickle](https://github.com/diku-dk/sml-pickle)
* [![Build Status](https://travis-ci.org/diku-dk/sml-regexp.svg?branch=master)](https://travis-ci.org/diku-dk/sml-regexp)
  [github.com/diku-dk/sml-regexp](https://github.com/diku-dk/sml-regexp)
* [![Build Status](https://travis-ci.org/diku-dk/sml-random.svg?branch=master)](https://travis-ci.org/diku-dk/sml-random)
  [github.com/diku-dk/sml-random](https://github.com/diku-dk/sml-random)
* [![Build Status](https://travis-ci.org/diku-dk/sml-setmap.svg?branch=master)](https://travis-ci.org/diku-dk/sml-setmap)
  [github.com/diku-dk/sml-setmap](https://github.com/diku-dk/sml-setmap)
* [![Build Status](https://travis-ci.org/diku-dk/sml-sort.svg?branch=master)](https://travis-ci.org/diku-dk/sml-sort)
  [github.com/diku-dk/sml-sort](https://github.com/diku-dk/sml-sort)
* [![Build Status](https://travis-ci.org/diku-dk/sml-uref.svg?branch=master)](https://travis-ci.org/diku-dk/sml-uref)
  [github.com/diku-dk/sml-uref](https://github.com/diku-dk/sml-uref)


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
