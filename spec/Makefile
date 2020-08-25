

SOURCES = $(wildcard *.md)
PDFS    = $(patsubst %.md, %.pdf, $(SOURCES))

all: $(PDFS) 



%.pdf: %.md
	pandoc $< -o $@ --filter pandoc-plantuml	
