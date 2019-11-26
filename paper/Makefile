all: README.md \
	supplementary_material.md \
	paper-journal/manuscript.pdf \
	paper-journal/supporting_information.pdf \
	docx

docx: supplementary_material.docx manuscript.docx

#############################
### Paper-writing Scripts ###
#############################

scripts/paper-scripts:
	if [ -d $@ ]; \
	then(cd $@ && git pull); \
	else git clone git@github.com:eyzhao/paper-scripts.git $@; \
	fi

###############################

styles:
	git -C $@ pull || git clone https://github.com/citation-style-language/styles.git $@

intermediates/variables.yml:
	if [ -f "../analysis/variables.yml" ]; then \
		cp -f ../analysis/variables.yml $@; \
	else \
		touch $@; \
	fi;

intermediates/tables.md:
	if [ -f "../analysis/tables.md" ]; then \
		cp -f ../analysis/tables.md $@; \
	else \
		touch $@; \
	fi;

intermediates/supplemental_tables.md:
	if [ -f "../analysis/supplemental_tables.md" ]; then \
		cp -f ../analysis/supplemental_tables.md $@; \
	else \
		touch $@; \
	fi

supplemental_figures.yml:
	touch $@

intermediates/supplemental_figures.md: supplemental_figures.yml intermediates/variables.yml
	python scripts/paper-scripts/supplemental-figures.py -i $< -o $@ -v intermediates/variables.yml

intermediates/supplementary_material_raw.md: \
	supplemental_information.md \
	intermediates/supplemental_figures.md \
	intermediates/supplemental_tables.md
	cat intermediates/supplemental_figures.md intermediates/supplemental_tables.md supplemental_information.md > $@

intermediates/meta_supplementary_material.md: \
	metadata.yml supplementary_material.md
	cat metadata.yml supplementary_material.md > $@

supplementary_material.md: intermediates/supplementary_material_raw.md
	scripts/paper-scripts/markdown-variables.py -i $< -v intermediates/variables.yml > $@

figures:
	if [ -d "../analysis/figures" ]; then \
		rm -rf $@; cp -r ../analysis/figures $@; \
	else \
		mkdir -p figures; \
	fi

intermediates/manuscript_figcaptions.md: \
	manuscript.md \
	scripts/paper-scripts
	mkdir -p intermediates && \
	chmod 755 scripts/paper-scripts/figure-labels.py; \
		scripts/paper-scripts/figure-labels.py -i $< > $@

intermediates/manuscript_figcaptions_variables.md: \
	intermediates/manuscript_figcaptions.md \
	intermediates/variables.yml \
	supplementary_material.md
	chmod 755 scripts/paper-scripts/markdown-variables.py; \
		scripts/paper-scripts/markdown-variables.py -i $< -v intermediates/variables.yml > $@

intermediates/manuscript_variables.md: \
	manuscript.md \
	intermediates/variables.yml \
	supplementary_material.md
	chmod 755 scripts/paper-scripts/markdown-variables.py; \
		scripts/paper-scripts/markdown-variables.py -i $< -v intermediates/variables.yml > $@

# % takes on either "figcaptions_variables" or "variables"
intermediates/merged_%.md: \
	metadata.yml \
	intermediates/manuscript_%.md
	cat $^ > $@;

intermediates/merged_%-with-tables.md: intermediates/merged_%.md intermediates/tables.md
	cat $^ > $@

README.md: \
	intermediates/merged_figcaptions_variables-with-tables.md \
	readme.markdown_strict \
	figures \
	library.bib \
	acronyms.txt \
	supplementary_material.md
	echo "Compiling README.md. Make sure that the appropriate CSL style file is used in metadata.yml" && \
	pandoc -s --template=readme \
		   -t markdown_strict \
		   --columns=100 \
		   --filter=scripts/paper-scripts/pandoc-acronym.py \
		   --filter=pandoc-fignos \
		   --filter=pandoc-tablenos \
		   --filter=pandoc-citeproc \
		   --mathjax \
		   -o $@ $<

README.html: \
	intermediates/merged_variables-with-tables.md \
	readme.markdown_strict \
	figures \
	library.bib \
	acronyms.txt \
	supplementary_material.md
	echo "Compiling README.html. Make sure that the appropriate CSL style file is used in metadata.yml" && \
	pandoc -s --template=main \
		   -t html \
		   --columns=100 \
		   --filter=scripts/paper-scripts/pandoc-acronym.py \
		   --filter=pandoc-fignos \
		   --filter=pandoc-tablenos \
		   --filter=pandoc-citeproc \
		   --webtex \
		   -o $@ $<

manuscript.docx: README.html
	pandoc -s -t docx -o $@ $<

supplementary_material.docx: supplementary_material.html
	pandoc -s -t docx -o $@ $<

supplementary_material.html: \
	intermediates/meta_supplementary_material.md \
	supplementary.html
	pandoc \
		-s \
		-f markdown \
		-t html \
		--template=supplementary \
		--columns=100 \
	    --filter=scripts/paper-scripts/pandoc-acronym.py \
		--filter=pandoc-tablenos \
		--webtex \
		-o $@ $<

#################################
### LATEX MANUSCRIPT CREATION ###
#################################

%-journal:
	mkdir -p $@; cp -f latex-assets/* $@;

%-journal/figures: figures %-journal
	cp -rf $< $*-journal

%-journal/schematics: schematics %-journal
	cp -rf $< $*-journal

%-journal/library.bib: library.bib %-journal
	cp -rf $< $@

%-journal/supporting_information_raw.tex: \
	intermediates/meta_supplementary_material.md \
	%-journal \
	%_supporting.latex
	pandoc \
		-f markdown \
		-t latex \
		--template=$*_supporting \
		--columns=80 \
	    --filter=scripts/paper-scripts/pandoc-acronym.py \
		--filter=pandoc-tablenos \
		-o $@ $<

%-journal/supporting_information.tex: %-journal/supporting_information_raw.tex
	cat $< \
		| perl -ne 's/includegraphics/includegraphics\[width=\\columnwidth]/g; s/\.png/\.pdf/g; print' \
		| perl -ne 's/includegraphics\[width\=\\columnwidth\]\[(.*?)\]/includegraphics\[\1\]/g; print' \
		> $@

%-journal/supporting_information.pdf: %-journal/supporting_information.tex %-journal/figures %-journal/schematics
	cd $*-journal; pdflatex supporting_information; pdflatex supporting_information

%-journal/manuscript-raw.tex: \
	intermediates/merged_variables.md \
	%.latex \
	%-journal/figures \
	%-journal/schematics \
	%-journal/library.bib
	pandoc --template=$* \
		   -t latex \
		   --columns=80 \
		   --filter=scripts/paper-scripts/pandoc-acronym.py \
		   --filter=pandoc-fignos \
		   --filter=pandoc-tablenos \
		   --filter=pandoc-citeproc \
		   --natbib \
		   -o $@ $<

%-journal/manuscript.tex: %-journal/manuscript-raw.tex
	cat $< \
		| perl -ne 's/includegraphics/includegraphics\[width=\\columnwidth]/g; s/\.png/\.pdf/g; print' \
		| perl -ne 's/includegraphics\[width\=\\columnwidth\]\[(.*?)\]/includegraphics\[\1\]/g; print' \
		> $@

%-journal/manuscript.pdf: \
	%-journal/manuscript.tex \
	%-journal/figures
	cd $*-journal; \
		pdflatex manuscript; bibtex manuscript; pdflatex manuscript; pdflatex manuscript


