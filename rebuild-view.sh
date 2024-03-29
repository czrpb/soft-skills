#!/bin/bash

pandoc $1.md --shift-heading-level-by=-1  -o $1.pdf
mupdf $1.pdf &
