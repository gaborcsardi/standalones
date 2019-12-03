
sdirs <- list.dirs("tools", recursive = FALSE)

for (d in sdirs) {
  readme_path <- file.path(d, "README.md")
  code_path <- dir(d, pattern = "\\.R$")
  if (length(code_path) != 1) stop("A standalone must be a single .R file")
  readme <- if (file.exists(readme_path)) readLines(readme_path)
  if (length(readme)) readme <- paste0("# ", readme)
  contents <-  c(readme, readLines(file.path(d, code_path)))
  out_path <- file.path("R", basename(code_path))
  writeLines(contents, out_path)
}
