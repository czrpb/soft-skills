#!/bin/bash

pandoc $1.md -V geometry:margin=1cm --shift-heading-level-by=-1 -o $1.pdf
mupdf $1.pdf &
