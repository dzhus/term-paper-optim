You'll need PLT Scheme 4, GNU Make, GNU bash, GNU m4, gnuplot, patched
texdepend and modern TeX distribution (TeX Live 2008 is ok) to «build»
this paper.

texdepend patch which allows using of several additional characters in
`\input` commands is available from the author of this paper.

There was an issue in pgfplots which prevented this document from
successful compilation, see
http://article.gmane.org/gmane.comp.tex.pgf.user/2082. By the end of
December 2008, Christian Feuersänger introduced a fix for the bug in
CVS version of pgfplots.

Scheme pyani-lib for PLT is available here:
<https://github.com/dzhus/scheme-pyani-lib>. PLT package
`wmfarr/simple-matrix` is also used (fetched automatically).

Feel free to mail author: dima@dzhus.org
