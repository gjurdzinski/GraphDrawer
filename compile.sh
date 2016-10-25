#!/bin/bash

if [ "$1" -eq 1 ]
then
    ocamlc -c graphics.cma GraphDrawing.ml
fi

if [ "$2" -eq 1 ]
then
    ocamlc -o program graphics.cma str.cma GraphDrawing.cmo program.ml
fi
