
sdirs <- list.dirs("tools", recursive = FALSE)

for (d in sdirs) {
  readme_path <- file.path(d, "README.md")
  code_path <- dir(d, pattern = "\\.R$")
  if (length(code_path) != 1) stop("A standalone must be a single .R file")
  out_path <- file.path("R", basename(code_path))
  unlink(out_path)
}
