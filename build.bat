@echo off
setlocal
set fname=%~n1


echo.%fname%


Rscript -e "knitr::knit(input = '%fname%.Rmd', output = '%fname%.md')" 
pandoc +RTS -K512m -RTS   "%fname%.md"  --dpi=600   --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash-implicit_figures   --to=revealjs   --standalone   --output="%fname%.html"  --verbose   --template=../_common/template.html   --metadata-file=../_common/config.yml    --no-highlight   --self-contained
 
del "%fname%.md"
"%fname%.html"

