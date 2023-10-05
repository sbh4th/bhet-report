#  program:  qmd-gdrive.R
#  task:     push and pull from Google Drive
#  input:    hei-report.qmd
#  author:   sam harper \ 2023-09-25


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

# trackdown::upload_file()
trackdown::update_file(
  file = "hei-report.qmd", 
  gpath="HEI-Final-Report", 
  gfile = "hei-report",
  hide_code = TRUE)


# Download a google doc into your current working 
# directory as an Rmd file

trackdown::download_file(
  gfile = "hei-report",
  gpath = "HEI-Final-Report",
  file = "hei-report.qmd") 









