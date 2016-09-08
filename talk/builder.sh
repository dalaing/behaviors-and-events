source $stdenv/setup
PATH=$pandoc/bin:$texlive/bin:$PATH
mkdir $out
pandoc $src -t beamer -o $out/slides.pdf
