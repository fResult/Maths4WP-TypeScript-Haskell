#!/bin/bash # <!-- markdownlint-disable-line MD018 MD041 -->

# Day 7 - Haskell Maze Adventure V4 (State)

## Jump to the current directory

cd "$(dirname "$0")" || exit 1

## Setup variable before running the code

export WORK_DIR=haskell/crash-course-2025/day-7

## Start the development!!

TARGET_MAZE_NUMBER="${1:-1}"
ghci MazeV4 ParserV2 -ghci-script <(echo ":set args $TARGET_MAZE_NUMBER")
