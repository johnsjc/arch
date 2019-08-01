#!/bin/bash
# Driver program for the sort program.
# Creates a temporary file containing a list of n numbers in the range [1, n] in a random order.
# The first line in the file is n.
# Each subsequent line is a number.
#
# After generating the file, runs the sort program with the file as input.
# Cleans up by deleting the temporary file.
# Usage: ./sort.sh <n>
#
# e.g.
# 5
# 1
# 3
# 4
# 5
# 2

echo $1 > nums

declare -i x=1
while [ $x -le $1 ]; do
    echo $x
    ((x++))
done | sort -R >> nums; cat nums | java -jar ~/bin/mars.jar main.s; rm nums
