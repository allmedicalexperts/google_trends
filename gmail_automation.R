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

search_terms <- c('Médico',
                  'Site',
                  'Automação')

# search_terms <- c('docker', 'git')

# 2.2 Email Parameters ----
to <- "rmasiniexpert@gmail.com"
subject <- "Google Trends"
body <- str_glue("
                 Hey Rodolfo.
                 Here is the report with Google Trends Keywords: {str_c(search_terms, collapse = ',')}
                 
                 -All Medical Expert")

# 3.0 Report Automation ----
file_path <- now() %>%
  str_replace_all("[[:punct:]]", "_") %>%
  str_replace(" ","T") %>%
  str_c("_trends.report.pdf")


rmarkdown::render(
  input           = "Google_Trends_Report_BR.Rmd",
  output_format   = "pdf_document",
  output_file     = file_path,
  output_dir      = "reports",
  params          = list(search_terms = search_terms) 
)


# 4.0 GMAIL API AUTOMATION ----
# Musr register an app with the google Developers console
# gmail Instructions:   http://github.com/r-lib/gmailr
# - Make an App:        http://developers.google.com/gmail/api/quickstart/python
# - Run remotely:       http://gargle.r-lib.org/articles/non-interactive-auth.html

# Download Gmail App Credentials & Configure App
gm_auth_configure(path = 'reports/credentials.json') # replace path to app credentials

# Authorize your gmail account
gm_auth(email = 'dr.rodolfomelare@gmail.com') # Replace e-mail

# Create mail
email <- gm_mime() %>%
  gm_to(to) %>%
  gm_from("dr.rodolfomelare@gmail.com") %>%
  # gm_cc("") %>%
  gm_subject(subject) %>%
  gm_text_body(body) %>%
  gm_attach_file(str_c("reports/", file_path))

gm_send_message(email, user_id = "me")  


















