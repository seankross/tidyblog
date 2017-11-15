new_tidyblog <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }

  template <- paste0('---\ntitle: "Tidy Blog"\noutput: html_document\n---\n\n',
                     '```{r, echo=FALSE, message=FALSE, warning=FALSE}\n',
                     '# Please do not edit this code chunk.\n\n',
                     'post_tbl <- read.csv("post_tbl.csv", stringsAsFactors = FALSE)\n',
                     'knitr::kable(dplyr::arrange(post_tbl, dplyr::desc(Date)))\n',
                     '```')

  ask_to_create_file(file.path(path, "index.Rmd"))
  cat(template, file = file.path(path, "index.Rmd"))
}

ask_to_create_file <- function(path) {
  if (file.exists(path)) {
    message("Is it okay to overwrite ", path, " ?")
    response <- select.list(c("Yes", "No"))
    if (response == "No") {
      stop("Could not overwrite file.")
    }
  }
}
