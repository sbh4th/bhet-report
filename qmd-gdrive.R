#  program:  qmd-gdrive.R
#  task:     push and pull from Google Drive
#  input:    hei-report.qmd
#  author:   sam harper \ 2023-08-28


# load packages
# remotes::install_github("claudiozandonella/trackdown")
library(trackdown)

my_client <- gargle::gargle_oauth_client(
  name = "Desktop client 1",
  id = "800379350069-17t98fv6o0eos001aq7lhe645asjjev7.apps.googleusercontent.com",
  secret = "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"
)
trackdown_auth_configure(client = my_client)


# code to send to Google Drive (use `upload` for first
# time or `update` to replace)

trackdown::upload_file(
# trackdown::update_file(
  file = "hei-report.qmd", 
  gpath="HEI-Final-Report", 
  gfile = "hei-report",
  hide_code = TRUE)


# Download a google doc into your current working 
# directory as an Rmd file

# individual chapter
trackdown::download_file(
  gfile = "08-policies",
  gpath = "mcgill-admin/epi-phd-handbook",
  file = "08-policies.qmd") 



# update an individual chapter
# individual chapter
trackdown::update_file(
  file = "08-policies.qmd",
  path = "mcgill-admin/epi-phd-handbook",
  gfile = "08-policies") 






