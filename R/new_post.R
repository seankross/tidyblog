new_post <- function(slug, path = NULL, open_post = TRUE) {
  if(grepl("\\s", slug)){
    stop("Make sure your slug doesn't contain any whitespace.")
  }

  if (is.null(path)) {
    path <- normalizePath(".")
  } else {
    path <- normalizePath(path)
  }

  template <- paste0('---\ntitle: "Title"\ndescription: "A simple post."\nauthor: "Your Name"\ndate: "',
                     as.character(Sys.Date()), '"\noutput: html_document\n',
                     '---\n\n## Introduction')

  if (!dir.exists(file.path(path, slug))) {
    dir.create(file.path(path, slug), showWarnings = FALSE, recursive = TRUE)
  }

  ask_to_create_file(file.path(path, slug, "index.Rmd"))
  cat(template, file = file.path(path, slug, "index.Rmd"))

  if (open_post) {
    file.edit(file.path(path, slug, "index.Rmd"))
  }

  invisible(file.exists(file.path(path, slug, "index.Rmd")))
}
