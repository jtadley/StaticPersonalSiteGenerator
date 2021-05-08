#!/bin/sh

raco test movies.rkt

scp out/*.html jacob@192.168.1.177:~/Site/Movies

rm out/*.html
