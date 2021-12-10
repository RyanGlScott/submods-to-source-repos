# `submods-to-source-repos`
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build Status](https://github.com/RyanGlScott/submods-to-source-repos/workflows/Haskell-CI/badge.svg)](https://github.com/RyanGlScott/submods-to-source-repos/actions?query=workflow%3AHaskell-CI)

[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

Convert a submodule-using `cabal.project` file to one that uses `source-repository-package`s.

# Instructions

1. First, clone the repository with submodules that you are interested in:

   ```
   $ git clone <repo> && cd <repo-dir> && git submodule update --init --depth 1
   ```

   The `git submodule update --init --depth 1` part is necessary because
   `submods-to-source-repos` must look into the contents of each submodule to
   determine where each submodule's `.cabal` files are located. However,
   this does not require the full `git` history of each submodule, so you can
   save some time by cloning each submodule shallowly with `--depth 1`.

2. Invoke `submods-to-source-repos` on the main `cabal.project` file:

   ```
   $ submods-to-source-repos cabal.project
   ```

   If you want to save this as a new `cabal.project` file, you can redirect the
   output like so:

   ```
   $ submods-to-source-repos cabal.project > cabal.project.new
   ```
