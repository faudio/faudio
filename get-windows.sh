#!/bin/bash

if [ $# -lt 1 ]
then
  echo "Usage: `basename $0` [version]"
  exit $E_BADARGS
else
    VERSION=$1
fi

scp hans@192.168.0.69:/c/Users/Doremir/Documents/bygge/faudio/faudio-$VERSION.zip .
