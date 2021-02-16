# Setup


library(devtools)
library(roxygen2)

# Vignette building

devtools::install(build_vignettes = TRUE)

# Spell checking

spell_check()

rhub::validate_email(email = "yuehu@tsinghua.edu.cn", token = "345011f4ca60404abf76007e9ae89e3e")

check_rhub(email = "yuehu@tsinghua.edu.cn")


# Package website building
library(devtools)

usethis::git_vaccinate() #Adds .DS_Store, .Rproj.user, .Rdata, .Rhistory, and .httr-oauth to your global (a.k.a. user-level) .gitignore. This is good practice as it decreases the chance that you will accidentally leak credentials to GitHub.

use_github_links()
use_github_action_check_standard()

library(pkgdown)
build_site()
