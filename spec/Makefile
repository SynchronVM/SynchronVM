

SOURCES = $(wildcard *.md)
PDFS    = $(patsubst %.md, %.pdf, $(SOURCES))

all: $(PDFS) 



%.pdf: $(SOURCES)
	pandoc $< -o $@ --filter pandoc-plantuml	
