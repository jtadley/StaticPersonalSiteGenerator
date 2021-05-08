#!/bin/sh

raco test music.rkt

scp out/*.html jacob@192.168.1.177:~/Site/Music

rm out/*.html
