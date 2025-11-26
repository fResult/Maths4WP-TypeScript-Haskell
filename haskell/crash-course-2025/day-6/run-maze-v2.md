#!/bin/bash # <!-- markdownlint-disable-line MD018 MD041 -->

# Day 6 - Haskell Maze Adventure V2 (Parser)

## Jump to the current directory

cd "$(dirname "$0")" || return

## Setup variable before running the code

export WORK_DIR=haskell/crash-course-2025/day-6

## Compile `MazeV2.hs` and `ParserV1.hs` files

ghc maze-v2 ParserV1

## Start the adventure!!

runHaskell maze-v2
