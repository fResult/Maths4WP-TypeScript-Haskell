#!/bin/bash # <!-- markdownlint-disable-line MD018 MD041 -->

# Day 6 - Haskell Maze Adventure V1 (Stable)

## Jump to the current directory

cd "$(dirname "$0")" || return

## Setup variable before running the code

export WORK_DIR=haskell/crash-course-2025/day-6

## Compile `MazeV1.hs` file

ghc maze-v1

## Start the adventure!!

runHaskell maze-v1
