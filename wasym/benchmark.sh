#!/usr/bin/bash

echo -n "" > "$2.log";

for i in $(eval echo {$3..$4..$5}); do
    echo -n "$i" ", " >> "$2.log";
    echo "depth:" "$i";
    ( /bin/time -f "%es , %MKB" ./wasym-exe $1 $2 $i unreachable > /dev/null ) &>> "$2.log";
done
