ROOT_DIR = .
OUTPUT_FORMAT = pdf_document
OUTPUTS_DIR = $(ROOT_DIR)/outputs
R_DIR = $(ROOT_DIR)/R
RMD_FILES = $(wildcard $(R_DIR)/*.Rmd)
PDF_FILES = $(OUTPUTS_DIR)/$(basename $(R_FILES)).pdf

# Sorted order happens to be the correct one
KNIT =  R -e  'for(rmd in sort(unlist(strsplit("$^", split = " ")))){ rmarkdown::render(rmd, output_dir = "$(OUTPUTS_DIR)", envir = parent.frame())}'

all : $(PDF_FILES)

$(R_DIR)/eda.Rmd : $(R_DIR)/tidy.R
	R -f $(R_DIR)/tidy.R

$(PDF_FILES) : $(RMD_FILES)
	$(KNIT)
