PDFLATEX := pdflatex -shell-escape
BIBTEX := bibtex8 -B
GNUPLOT := gnuplot

DOCNAME := paper

define get-target-method
$(shell echo $1 | sed -e "s/\-.*//")
endef

define get-target-function
$(shell echo $1 | sed -e "s/.*\-//")
endef

.SECONDEXPANSION:

.PHONY: doc clean

include ${DOCNAME}-deps.mk

${DOCNAME}-deps.mk: ${DOCNAME}.tex
	texdepend -o $@ -print=if $<

${DOCNAME}.aux: ${INCLUDES} ${DOCNAME}.tex ${DOCNAME}.bib
	${PDFLATEX} ${DOCNAME}
	${BIBTEX} ${DOCNAME}

${DOCNAME}.pdf: ${DOCNAME}.aux
	${PDFLATEX} ${DOCNAME}
	${PDFLATEX} ${DOCNAME}

%.tkz: %.tkz.tex

%-plot.tkz.tex: $$(call get-target-function,$$*).gp \
                plot-method.sh \
                plot-method.tpl.tkz.tex \
                contour-path.tpl.tkz.tex
	@rm -fr $*-contours-*.out
	${GNUPLOT} $<
	${SHELL} plot-method.sh $(call get-target-function,$*) > $@

doc: ${DOCNAME}.pdf

clean:
	@rm -frv `hg status --unknown --no-status`