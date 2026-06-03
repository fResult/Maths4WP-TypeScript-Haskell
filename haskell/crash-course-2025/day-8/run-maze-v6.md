#!/bin/bash # <!-- markdownlint-disable-line MD018 MD041 -->

# Day 8 - Haskell Maze Adventure V6 (Sequence DSL)

## Jump to the current directory

cd "$(dirname "$0")" || exit 1

## Setup variable before running the code

export WORK_DIR=haskell/crash-course-2025/day-8

## Start the adventure!!

TARGET_MAZE_NUMBER="${1:-1}"
runghc MazeV6 "$TARGET_MAZE_NUMBER"
