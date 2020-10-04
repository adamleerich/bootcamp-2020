@echo off
setlocal
set fname=%~n1

Rscript --arch i386 -e "knitr::knit(input = '%fname%.Rmd', output = '%fname%.md')" 
REM pandoc +RTS -K512m -RTS   "%fname%.md"  --dpi=600   --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash-implicit_figures   --to=revealjs   --output="..\slide_output\%fname%.html"  --verbose   --template=reveal.js/template.html   --metadata-file=reveal.js/config.yml    --mathjax    --no-highlight
pandoc +RTS -K512m -RTS   "%fname%.md"  --dpi=600   --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash-implicit_figures   --to=revealjs   --standalone   --output="..\slide_output\%fname%_full.html"  --verbose   --template=reveal.js/template.html   --metadata-file=reveal.js/config.yml    --mathjax    --no-highlight   --self-contained

del "%fname%.md"
"..\slide_output\%fname%_full.html"

