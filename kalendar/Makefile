
main: kalendar.pdf

kalendar.pdf: kal.tex kalendar.tex
	pdflatex kalendar.tex

kal.tex: generator.hs
	ghc --make generator.hs
	./generator > kal.tex

clean:
	rm -f generator *.log *.aux *.hi *.o kalendar.pdf
