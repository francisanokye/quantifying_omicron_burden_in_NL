all: *.md *.html

%.md : %.Rmd
	Rscript -e "rmarkdown::render('$<', output_file = '$@')"

%.html : %.md
	Rscript -e "rmarkdown::render('$<', output_file = '$@')"
