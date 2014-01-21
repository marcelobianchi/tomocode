#!/bin/bash

awk '
BEGIN {
    nseg = 0;
    counter = 0;
    currentn = 0;
}
{
    if (NF == 1) {
        if ( nseg > 0 && currentn != counter) {
            print "> Invalid last segment, expected: ",currentn," found ",counter
        }
        currentn = $1;
        nseg += 1;
        counter = 0;
        print "> ",$0;
    } else {
        print $0;
        counter += 1;
    }
}
END {
        if ( nseg > 0 && currentn != counter) {
            print "> Invalid last segment, expected: ",currentn," found ",counter
        }
}' "$1"


