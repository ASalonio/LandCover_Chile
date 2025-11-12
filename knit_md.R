#!/usr/bin/env Rscript
# --------------------------------------------------------------
# knit_md.R – knit private .Rmd → public .md (GitHub‑friendly)
# --------------------------------------------------------------

library(rmarkdown)
library(rprojroot)

## ---- 1. Repository root ------------------------------------------------
repo_root <- rprojroot::find_root(rprojroot::is_git_root)
cat("Repo root :", repo_root, "\n")

out_dir <- file.path(repo_root, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## ---- 2. Paths to the *private* .Rmd files -------------------------------
rmd_dir <- file.path(repo_root, "scripts")   # <-- they live here, but are ignored

part_i_rmd  <- file.path(rmd_dir, "part_I.Rmd")
part_ii_rmd <- file.path(rmd_dir, "part_II.Rmd")

## ---- 3. Safety checks ---------------------------------------------------
if (!file.exists(part_i_rmd))  stop("Missing: ", part_i_rmd)
if (!file.exists(part_ii_rmd)) stop("Missing: ", part_ii_rmd)

cat("Found both .Rmd files\n")

## ---- 4. Knit to markdown ------------------------------------------------
render(
  input         = part_i_rmd,
  output_file   = file.path(repo_root, "part_I.md"),
  output_format = "github_document",
  clean         = TRUE,
  quiet         = TRUE
)

render(
  input         = part_ii_rmd,
  output_file   = file.path(repo_root, "part_II.md"),
  output_format = "github_document",
  clean         = TRUE,
  quiet         = TRUE
)

cat("SUCCESS → part_I.md  &  part_II.md created\n")

