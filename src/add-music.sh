#!/bin/sh

SITESOURCE="../SiteSource"
MUSICDIR="Music"
ABORTING="Aborting..."

# --- ARTIST ---
read -p "Artist: " ARTIST
if [ -z "$ARTIST" ]
then
	echo "$ABORTING"
	exit
fi

# --- ALBUM ---
read -p "Album: " ALBUM
if [ -z "$ALBUM" ]
then
	echo "$ABORTING"
	exit
fi

# --- YEAR ---
read -p "Year: " YEAR
if [ -z "$YEAR" ]
then
	echo "$ABORTING"
	exit
fi

# --- GENRELIST ---
read -p "Genre: " GENRELIST
if [ -z "$GENRELIST" ]
then
	echo "$ABORTING"
	exit
fi

read -p "Genre: " GENRE
while [ ! -z "$GENRE" ]
do
	GENRELIST="$GENRELIST,$GENRE"
	read -p "Genre: " GENRE
done

# --- RATING ---
read -p "Rating: " RATING
if [ -z "$RATING" ]
then
	echo "$ABORTING"
	exit
fi

# --- WRITE TO FILE ---

FILE="$SITESOURCE/$MUSICDIR/$ARTIST-$ALBUM" 
FILE=$( echo $FILE | sed 's/ //g' )

echo "Artist:$ARTIST
Album:$ALBUM
Year:$YEAR
Genres:$GENRELIST
Rating:$RATING
Opinion: " >> $FILE
