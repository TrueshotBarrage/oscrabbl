#!/bin/bash
# DO NOT EDIT THIS FILE

TARGETS="dictionary.cmo dictionarySet.cmo printer.cmo scrabble.cmo state.cmo main.cmo test.cmo treeDictionary.cmo"

ocamlbuild -use-ocamlfind $TARGETS
if [[ $? -ne 0 ]]; then
  cat <<EOF
===========================================================
WARNING

Your code currently does not compile.  You will receive
little to no credit for submitting this code. Check the
error messages above carefully to determine what is wrong.
See a consultant for help if you cannot determine what is
wrong.
===========================================================
EOF
  exit 1
fi

if ocamlbuild -use-ocamlfind checktypes.cmo ; then
  cat <<EOF
===========================================================
Your function names and types look good to me.
Congratulations!
===========================================================
EOF
else
  cat <<EOF
===========================================================
WARNING

Your function names and types look broken to me.  The code
that you submit might not compile on the grader's machine,
leading to heavy penalties.  Please fix your names and
types.  Check the error messages above carefully to
determine what is wrong.  See a consultant for help if you
cannot determine what is wrong.
===========================================================
EOF
fi

