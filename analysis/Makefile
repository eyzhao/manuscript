dependencies_repo_url := git@github.com:eyzhao/bio-pipeline-dependencies.git
project_root := /projects/ezhao_prj/papers/pog-signatures-paper
analysis_root := $(project_root)/analysis

all: scripts/pipelines

###########################
### Directory Structure ###
###########################

meta:
	mkdir -p meta

paths:
	mkdir -p paths

#################
### Load Code ###
#################

dependencies:
	if [ -d $@ ]; \
	then (cd $@ && git pull); \
	else git clone ${dependencies_repo_url} $@; \
	fi && \
	cd dependencies && \
	make

scripts/SignIT:
	if [ -d $@ ]; \
	then(cd $@ && git pull); \
	else git clone git@github.com:eyzhao/SignIT.git $@; \
	fi

scripts/SignIT-osx:
	if [ -d $@ ]; \
	then(cd $@ && git pull); \
	else git clone git@github.com:eyzhao/SignIT.git $@; \
	fi

scripts/pipelines:
	if [ -d $@ ]; \
	then (cd $@ && git pull); \
	else git clone git@github.com:eyzhao/bio-pipelines.git $@; \
	fi

scripts/SignatureAnalyzer/SignatureAnalyzer.Broad.R:
	mkdir scripts/SignatureAnalyzer \
		&& wget -O scripts/SignatureAnalyzer/install.zip \
		http://archive.broadinstitute.org/cancer/cga/sites/default/files/data/tools/msp/SignatureAnalyzer.0421-2017.Broad_Institute.zip \
		&& unzip -d scripts/SignatureAnalyzer scripts/SignatureAnalyzer/install.zip 

scripts/oncotree:
	if [ -d $@ ]; \
	then (cd $@ && git pull); \
	else git clone git@github.com:eyzhao/oncotree.git $@; \
	fi
