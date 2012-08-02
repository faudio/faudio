#!/bin/bash
find -E $@ -regex ".*\\.(h|hpp|c|cc)" \
    | xargs astyle -n