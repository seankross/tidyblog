
#' @importFrom rmarkdown yaml_front_matter render
#' @importFrom dplyr data_frame
render_blog <- function(input, posts = NULL) {
  home_path <- normalizePath(dirname(input))

  if(is.null(posts)){
    posts_path <- home_path
  } else {
    posts_path <- file.path(home_path, posts)
  }

  if(!file.exists(posts_path)){
    stop("The folder containing your posts does not exist.")
  }

  slugs <- NULL
  descriptions <- NULL
  dates <- NULL

  for(post_dir in list.dirs(posts_path, recursive = FALSE)){
    rmds <- list.files(post_dir, pattern = "[r|R]md$", full.names = TRUE)
    index <- grep("index\\.[r|R]md$", rmds)[1]
    config <- grep("config\\.[r|R]md$", rmds)[1]

    if (file.exists(file.path(post_dir, ".tidyblogignore"))) {
      next
    }

    if (!is.na(config)) {
      # Found config file
      config_yaml <- yaml_front_matter(rmds[config])

      if( is.null(config_yaml$link) ) {
        config_yaml$link <- ""
      }

      slugs <- c(slugs, paste0('[', post_dir, ']',
                               '(', post_dir, '/', config_yaml$link, ')'))
      descriptions <- c(descriptions, config_yaml$description)
      dates <- c(dates, config_yaml$date)
    } else if(!is.na(index)) {
      # Found an index
      index_yaml <- yaml_front_matter(rmds[index])
      slugs <- c(slugs, paste0('[', post_dir, ']',
                               '(', post_dir, ')'))
      descriptions <- c(descriptions, index_yaml$description)
      dates <- c(dates, index_yaml$date)

      # Render post if changes have been made
      html_index <- file.path(dirname(rmds[index]), "index.html")
      if (!file.exists(html_index) ||
          file.info(rmds[index])$mtime > file.info(html_index)$mtime) {
        render(rmds[index])
      }
    }
  }

  post_tbl <- data_frame("/" = slugs, Description = descriptions, Date = dates)
  write.csv(post_tbl, file.path(home_path, "post_tbl.csv"))
  on.exit(unlink(file.path(home_path, "post_tbl.csv"), force = TRUE))
  render(input)
}
