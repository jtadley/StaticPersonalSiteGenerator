#!/bin/sh

# --------------------

RAWDIR="./SiteSource/Raw"
CSS="styles.css"
CSSFILE=$RAWDIR/$CSS
SRCDIR="src"
MUSICROOT=""
MUSICALL="generate-music-table-page.rkt"
ALLNAME="All.html"
OUTDEFAULT="out"
RUNNINGFOLDER="Running"
MUSICFOLDER="Music"
MOVIEFOLDER="Movies"

# --------------------

if [ -z $1 ];
	then
		echo "Provide a directory to build the site";
		exit;
fi

mkdir $1/Running
mkdir $1/Music
mkdir $1/Movies

# --------------------

cp $CSSFILE $1
cp $CSSFILE $1/$RUNNINGFOLDER
cp $CSSFILE $1/$MUSICFOLDER
cp $CSSFILE $1/$MOVIEFOLDER

# --------------------

raco test $SRCDIR/$MUSICALL
mv $SRCDIR/$OUTDEFAULT $1/$MUSICFOLDER/$ALLNAME
