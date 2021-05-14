#!/bin/sh

SITESOURCE="../SiteSource"
MOVIEDIR="Movie"
ABORTING="Aborting..."

# --- TITLE ---
read -p "Title: " TITLE
if [ -z "$TITLE" ]
then
	echo "$ABORTING"
	exit
fi

# --- DIRECTOR ---
read -p "Director: " DIRECTOR
if [ -z "$DIRECTOR" ]
then
	echo "$ABORTING"
	exit
fi

# --- WRITERLIST ---
read -p "Writer: " WRITERLIST
if [ -z "$WRITERLIST" ]
then
	echo "$ABORTING"
	exit
fi

read -p "Writer: " WRITER
while [ ! -z "$WRITER" ]
do
	WRITERLIST="$WRITERLIST,$WRITER"
	read -p "Writer: " WRITER
done

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

FILE="$SITESOURCE/$MOVIEDIR/$TITLE" 
FILE=$( echo $FILE | sed 's/ //g' )

echo "Title:$TITLE
Director:$DIRECTOR
Writers:$WRITERLIST
Year:$YEAR
Genres:$GENRELIST
Rating:$RATING
Opinion: " >> $FILE
