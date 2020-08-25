

SOURCES = $(wildcard *.md)
PDFS    = $(patsubst %.md, %.pdf, $(SOURCES))

all: $(PDFS) 



%.pdf: %.md
	pandoc -V linkcolor:blue $< -o $@ --filter pandoc-plantuml	



clean:
	rm *.pdf
