#!/bin/bash # <!-- markdownlint-disable-line MD018 MD041 -->

# Clean Compiled Haskell Files

## Jump to the current directory

cd "$(dirname "$0")" || return

## Setup variable before running the code

export WORK_DIR=haskell/crash-course-2025/day-6

## Compile `MazeV1.hs` file

rm -rf *.o *.hi
