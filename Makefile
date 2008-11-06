DOCNAME := paper

PDFLATEX := pdflatex
BIBTEX := bibtex8 -B

${DOCNAME}.aux: ${DOCNAME}.tex ${DOCNAME}.bib
	${PDFLATEX} ${DOCNAME}
	${BIBTEX} ${DOCNAME}

${DOCNAME}.pdf: ${DOCNAME}.aux
	${PDFLATEX} ${DOCNAME}
	${PDFLATEX} ${DOCNAME}

doc: ${DOCNAME}.pdf

clean:
	@rm -frv `hg status --unknown --no-status`