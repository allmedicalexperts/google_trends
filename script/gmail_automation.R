# All Medical Expert

# Project: Google Trends Automation ----


# 1.0 Libraries ---


# ------ Load Package ------

# GMAIL API
library(gmailr)

# Report Automation
library(rmarkdown)

# core
library(tidyverse)
library(lubridate)

# File System
library(fs)

# 2.0 Key Paramaters ----

# 2.1 Report Parameters ----

search_terms <- c('Avaliação Pré-Operatória','Doppler Transcraniano','Consulta Medicina Interna', 'Check Up')

# search_terms <- c('docker', 'git')

# 2.2 Email Parameters ----
to <- "dr.rodolfomelare@gmail.com"
subject <- "Google Trends"
body <- str_glue("
                 Hey Rodolfo.
                 Here is the report with Google Trends Keywords: {str_c(search_terms, collapse = ',')}
                 
                 -Rodolfo")

# 3.0 Report Automation ----
file_path <- now() %>%
  str_replace_all("[[:punct:]]", "_") %>%
  str_replace(" ","T") %>%
  str_c("_trends.report.pdf")


rmarkdown::render(
  input           = "Google Trends Report.Rmd",
  output_format   = "pdf_document",
  output_file     = file_path,
  output_dir      = "reports",
  params          = list(search_terms = search_terms) 
)




















