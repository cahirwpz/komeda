LATEX := latexmk -pdf
OUTPUT := komeda-overview.pdf

all: $(OUTPUT)

%.pdf: %.tex
	$(LATEX) $^

clean:
	latexmk -f -C $(OUTPUT)
	rm -f *.nav *.snm *vrb *~

.PHONY:	all clean
