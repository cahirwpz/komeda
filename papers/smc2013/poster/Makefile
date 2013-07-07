LATEX := latexmk -pdf
OUTPUT := poster.pdf

all: $(OUTPUT)

%.pdf: %.tex
	$(LATEX) $<

clean:
	latexmk -f -C $(OUTPUT)
	rm -f *~ *.bbl *.nav *.snm
