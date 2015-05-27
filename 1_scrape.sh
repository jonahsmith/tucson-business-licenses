while read p; do
	dir="permits/"
	curl "$p" > $dir${p: -20};
done <urls.txt