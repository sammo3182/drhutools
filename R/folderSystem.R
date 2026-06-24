#' @name folderSystem
#' @title Folder hierarchy creator for academic research
#'
#' @details The function constructs a standardized folder hierarchy encompassing `codes/`, `data/`, `output/`, `paper/`, and `document/`. These directories are designated for storing programming scripts, data sets, processed outputs, manuscript drafts (including related images and submission documents), and assorted materials, respectively. This organization facilitates easy retrieval of project components, ensuring that members of Dr. Hu's Amazing Team can efficiently navigate and manage shared resources. This structure is advocated for collaborative projects to maintain uniformity across team operations.
#'
#' In addition, the function assembles a ready-to-write manuscript template under `paper/` from the choices below. When an argument is left `NULL` and the session is interactive, the function asks for the choice; otherwise it falls back to the first option.
#'
#' * `language` --- `"english"` writes `paper_en.qmd`; `"chinese"` writes `paper_cn.qmd` (Chinese heading numbering, dual abstract, online appendix; modelled on the bundled `organizedParticipation.qmd`).
#' * `format` --- `"typst"` (PDF via Typst) or `"word"` (`.docx`).
#' * `bibliography` --- `"single"` produces one reference list via Quarto's citeproc; `"separate"` produces separate main-text and appendix lists via the Alexandria Typst package (Typst only; Word output is forced to `"single"`).
#' * `csl` --- `"apsa"` (American Political Science Association) or `"apa"` (APA 7th edition).
#'
#' Only the support files required by the chosen configuration are copied into `paper/` (the selected `.csl`, the matching `.bib` file(s), and the relevant Lua filters).
#'
#' @param overwrite Logical. Should existing template files in `paper/` be overwritten? Defaults to `FALSE`, leaving any files the user has already edited untouched.
#' @param language Character or `NULL`. Manuscript language: `"english"` or `"chinese"`. `NULL` (default) prompts interactively.
#' @param format Character or `NULL`. Output format: `"typst"` or `"word"`. `NULL` (default) prompts interactively.
#' @param bibliography Character or `NULL`. Bibliography layout: `"single"` or `"separate"`. `NULL` (default) prompts interactively (skipped, and forced to `"single"`, when `format = "word"`).
#' @param csl Character or `NULL`. Citation style: `"apsa"` or `"apa"`. `NULL` (default) prompts interactively.
#'
#' @return Invisibly returns `NULL`. Called for its side effect of creating local folders, placeholder files, a default `.gitignore`, and the configured manuscript template under the project's working directory.
#'
#' @export

folderSystem <- function(overwrite = FALSE,
                         language = NULL,
                         format = NULL,
                         bibliography = NULL,
                         csl = NULL) {
  ls_dir <- c(
    "codes",
    "data",
    "output",
    "paper",
    "paper/images",
    "paper/submission",
    "document",
    "codes/put codes here; delete this file after locating the real files.txt",
    "data/all data file here.csv",
    "output/image, results, and other output files here; delete this file after locating the real files here.txt",
    "paper/images/non-code-generated images here; delete this file after locating the real files.txt",
    "paper/submission/files for submission here; delete this file after locating the real files here.txt",
    "document/documents and materials here; delete this file after locating the real files.txt"
  )

  # First create all directories if they do not exist
  unique_dirs <- unique(dirname(ls_dir))
  lapply(unique_dirs, function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  })

  # Then create files only if their respective directories already exist
  lapply(ls_dir, function(path) {
    if (grepl("\\.\\w+$", path) && !file.exists(path)) {
      file.create(path)
    }
  })

  # Resolve the manuscript-template choices (interactive when NULL) ----------
  language <- .fs_resolve(
    language, c("english", "chinese"),
    c("English", "Chinese (\u4e2d\u6587)"),
    "Which language is the paper written in?",
    aliases = list(en = "english", eng = "english", cn = "chinese", zh = "chinese", chi = "chinese")
  )
  format <- .fs_resolve(
    format, c("typst", "word"),
    c("Typst (PDF)", "Word (.docx)"),
    "Which output format?",
    aliases = list(docx = "word", pdf = "typst", typ = "typst")
  )
  if (format == "word") {
    if (!is.null(bibliography) &&
        tolower(as.character(bibliography)[1]) %in% c("separate", "sep", "dual")) {
      message("Word output supports a single reference list only; using a single bibliography.")
    }
    bibliography <- "single"
  } else {
    bibliography <- .fs_resolve(
      bibliography, c("single", "separate"),
      c("Single bibliography (default)", "Separate main-text & appendix bibliographies"),
      "Bibliography layout?",
      aliases = list(one = "single", default = "single", sep = "separate", dual = "separate")
    )
  }
  csl <- .fs_resolve(
    csl, c("apsa", "apa"),
    c("American Political Science Association (APSA)", "APA 7th edition"),
    "Citation style?",
    aliases = list(apsr = "apsa", "american-political-science-association" = "apsa")
  )

  # Assemble and write the manuscript template ------------------------------
  template_dir <- system.file("template", package = "drhutools")
  if (nzchar(template_dir) && dir.exists(template_dir)) {
    skeleton_name <- if (language == "chinese") "paper_cn.qmd" else "paper_en.qmd"
    skeleton_path <- file.path(template_dir, skeleton_name)
    dest_qmd <- file.path("paper", skeleton_name)

    if (file.exists(skeleton_path) && (overwrite || !file.exists(dest_qmd))) {
      qmd_lines <- readLines(skeleton_path, encoding = "UTF-8", warn = FALSE)
      header <- .fs_build_yaml(language, format, bibliography, csl, template_dir)
      body <- .fs_assemble_body(.fs_extract_body(qmd_lines), language, format, bibliography)
      out_lines <- enc2utf8(c(header, body))
      con <- file(dest_qmd, open = "wb")
      writeLines(out_lines, con, useBytes = TRUE)
      close(con)
    }

    .fs_copy_support(template_dir, language, format, bibliography, csl, overwrite)

    message(sprintf(
      "Manuscript template written to '%s' (%s, %s output, %s bibliography, %s style).",
      dest_qmd, language, format, bibliography, toupper(csl)
    ))
  } else {
    warning(
      "Could not locate the bundled 'template' folder; ",
      "no manuscript template was created under paper/.",
      call. = FALSE
    )
  }

  # Create default .gitignore in project root if not present
  gitignore_path <- ".gitignore"
  if (!file.exists(gitignore_path)) {
    gitignore_content <- c(
      "# R artifacts",
      ".Rhistory",
      ".RData",
      ".Ruserdata",
      ".Rproj.user/",
      "*.Rproj",
      "",
      "# Data files",
      "data/",
      "",
      "# Output",
      "output/",
      "",
      "# OS files",
      ".DS_Store",
      "Thumbs.db",
      "",
      "# Quarto / RMarkdown caches",
      "*_cache/",
      "*_files/",
      "*.html",
      "!paper/submission/*.html"
    )
    writeLines(gitignore_content, gitignore_path)
  }

  invisible(NULL)
}


# Internal helpers ----------------------------------------------------------

# Resolve one template choice: honour an explicit value (with partial/alias
# matching), prompt interactively when NULL, or fall back to the first option.
.fs_resolve <- function(value, choices, labels, prompt, aliases = NULL) {
  if (!is.null(value)) {
    value <- tolower(as.character(value)[1])
    if (!is.null(aliases) && value %in% names(aliases)) value <- aliases[[value]]
    return(match.arg(value, choices))
  }
  if (interactive()) {
    idx <- utils::menu(labels, title = prompt)
    if (idx == 0L) return(choices[1])
    return(choices[idx])
  }
  choices[1]
}

.fs_csl_file <- function(csl) {
  if (csl == "apa") "apa.csl" else "american-political-science-association.csl"
}

# Build the complete YAML header (including the --- fences) for the chosen
# configuration.
.fs_build_yaml <- function(language, format, bibliography, csl, template_dir) {
  csl_file <- .fs_csl_file(csl)
  c(
    "---",
    .fs_format_block(language, format, bibliography, csl_file, template_dir),
    .fs_crossref_block(language),
    "",
    .fs_typeset_block(language),
    "",
    .fs_bib_block(language, format, bibliography, csl_file),
    "",
    .fs_meta_block(language),
    "",
    .fs_exec_block(),
    "",
    "---"
  )
}

.fs_format_block <- function(language, format, bibliography, csl_file, template_dir) {
  if (format == "typst") {
    if (language == "chinese") {
      header_lines <- readLines(
        file.path(template_dir, "_header-typst-cn.txt"),
        encoding = "UTF-8", warn = FALSE
      )
    } else {
      header_lines <- '#set text(font: ("Libertinus Serif", "Songti SC", "SimSun"))'
    }
    if (bibliography == "separate") {
      alexandria <- c(
        '#import "@preview/alexandria:0.2.2": *',
        '#show: alexandria(prefix: "main:", read: path => read(path))',
        '#show: alexandria(prefix: "appendix:", read: path => read(path))',
        sprintf('#let ref-style = "%s"', csl_file)
      )
      header_lines <- c(alexandria, header_lines)
    }
    c(
      "format:",
      "  typst:",
      "    number-sections: true",
      "    papersize: a4",
      "    keep-typ: true",
      "    margin:",
      "      x: 1in",
      "      y: 1in",
      "    include-in-header:",
      "      text: |",
      paste0("        ", header_lines)
    )
  } else {
    c(
      "format:",
      "  docx:",
      "    number-sections: true",
      "    # reference-doc: reference.docx  # optional: a styled Word reference document"
    )
  }
}

.fs_crossref_block <- function(language) {
  if (language == "chinese") {
    c(
      "crossref:",
      '  fig-prefix: "\u56fe"',
      '  fig-title: "\u56fe"',
      '  tbl-prefix: "\u8868"',
      '  tbl-title: "\u8868"',
      '  appendix-title: "\u9644\u5f55"'
    )
  } else {
    c(
      "crossref:",
      '  sec-prefix: OSM      # appendix sections referenced as "OSM A", "OSM B", ...',
      "  sec-labels: alpha A  # A, B, C, ..."
    )
  }
}

.fs_typeset_block <- function(language) {
  lines <- "fontsize: 12pt"
  if (language == "english") lines <- c(lines, "indent: true")
  c(lines, "linestretch: 1.5", "colorlinks: true", "link-citations: true")
}

.fs_bib_block <- function(language, format, bibliography, csl_file) {
  if (bibliography == "separate") {
    c(
      "# --- Separate bibliographies (Alexandria): main-text & appendix lists ---",
      'alexandria-main-bib: "references_main.bib"',
      'alexandria-appendix-bib: "references_app.bib"',
      'alexandria-appendix-id: "online-supplementary-materials"',
      "validate-yaml: false",
      "filters:",
      "  - cite-prefix.lua"
    )
  } else {
    lines <- c(
      "# --- Single bibliography (Quarto citeproc) ---",
      "bibliography: references.bib",
      sprintf("csl: %s", csl_file),
      "citeproc: true"
    )
    filt <- character(0)
    if (language == "chinese") {
      filt <- c("  - fix-lang.lua", "  - localize-cnbib.lua")
      if (format == "word") filt <- c(filt, "  - center-caption.lua")
    }
    if (length(filt)) lines <- c(lines, "filters:", filt)
    lines
  }
}

.fs_meta_block <- function(language) {
  if (language == "chinese") {
    c(
      "# author:",
      "#   - \u7b2c\u4e00\u4f5c\u8005",
      "#   - \u7b2c\u4e8c\u4f5c\u8005",
      "",
      'title: "\u8bba\u6587\u6807\u9898"',
      'abstract-title: "\u6458\u8981"',
      "abstract: |",
      "  \u5728\u6b64\u586b\u5199\u4e2d\u6587\u6458\u8981\u3002\u6bcf\u53e5\u4e00\u884c\uff0c\u4fbf\u4e8e\u7248\u672c\u6bd4\u8f83\u3002",
      "",
      "  **\u5173\u952e\u8bcd\uff1a** \u5173\u952e\u8bcd\u4e00\uff1b\u5173\u952e\u8bcd\u4e8c\uff1b\u5173\u952e\u8bcd\u4e09",
      "",
      "keywords: [\u5173\u952e\u8bcd\u4e00, \u5173\u952e\u8bcd\u4e8c, \u5173\u952e\u8bcd\u4e09]"
    )
  } else {
    c(
      'title: "PAPER TITLE HERE"',
      "author:",
      "  - name: First Author",
      "    affiliation: Your Institution",
      "  - name: Second Author",
      "    affiliation: Your Institution",
      "abstract: |",
      "  Replace with the abstract. One sentence per line keeps diffs clean.",
      "",
      "  **Keywords:** keyword one; keyword two; keyword three"
    )
  }
}

.fs_exec_block <- function() {
  c(
    "execute:",
    "  echo: false",
    "  message: false",
    "  warning: false",
    "  dpi: 300",
    "editor_options:",
    "  chunk_output_type: console",
    "editor:",
    "  markdown:",
    "    wrap: 84"
  )
}

# Extract the body (everything after the YAML front matter) of a .qmd file.
.fs_extract_body <- function(qmd_lines) {
  fences <- which(trimws(qmd_lines) == "---")
  if (length(fences) >= 2) qmd_lines[(fences[2] + 1):length(qmd_lines)] else qmd_lines
}

# Fill the reference-region markers and, for Word, drop raw Typst blocks.
.fs_assemble_body <- function(body_lines, language, format, bibliography) {
  body_lines <- .fs_replace_region(
    body_lines,
    "<!-- drhutools:refs-main:start -->", "<!-- drhutools:refs-main:end -->",
    .fs_refs_main(language, format, bibliography)
  )
  body_lines <- .fs_replace_region(
    body_lines,
    "<!-- drhutools:refs-appendix:start -->", "<!-- drhutools:refs-appendix:end -->",
    .fs_refs_appendix(language, format, bibliography)
  )
  if (format == "word") body_lines <- .fs_strip_typst_blocks(body_lines)
  body_lines
}

# Replace the lines between (but not including) two marker lines.
.fs_replace_region <- function(lines, start_marker, end_marker, new_content) {
  s <- which(trimws(lines) == start_marker)
  e <- which(trimws(lines) == end_marker)
  if (length(s) != 1L || length(e) != 1L || e <= s) return(lines)
  c(lines[seq_len(s)], new_content, lines[e:length(lines)])
}

# Remove ```{=typst} ... ``` fenced blocks (used when targeting Word).
.fs_strip_typst_blocks <- function(lines) {
  out <- character(0)
  in_block <- FALSE
  for (ln in lines) {
    t <- trimws(ln)
    if (!in_block && grepl("^```+\\{=typst\\}", t)) {
      in_block <- TRUE
      next
    }
    if (in_block) {
      if (grepl("^```+$", t)) in_block <- FALSE
      next
    }
    out <- c(out, ln)
  }
  out
}

.fs_refs_main <- function(language, format, bibliography) {
  heading <- if (language == "chinese") "\u53c2\u8003\u6587\u732e" else "References"
  if (bibliography == "separate" && format == "typst") {
    c(
      "{{< pagebreak >}}",
      "",
      sprintf("# %s {.unnumbered}", heading),
      "",
      "```{=typst}",
      '#bibliographyx("references_main.bib", prefix: "main:", title: none, style: ref-style)',
      "```"
    )
  } else {
    c(
      "{{< pagebreak >}}",
      "",
      sprintf("# %s {.unnumbered}", heading),
      "",
      "::: {#refs}",
      ":::"
    )
  }
}

.fs_refs_appendix <- function(language, format, bibliography) {
  if (bibliography == "separate" && format == "typst") {
    heading <- if (language == "chinese") "\u53c2\u8003\u6587\u732e" else "References"
    c(
      "{{< pagebreak >}}",
      "",
      sprintf("# %s {.unnumbered}", heading),
      "",
      "```{=typst}",
      '#bibliographyx("references_app.bib", prefix: "appendix:", title: none, style: ref-style)',
      "```"
    )
  } else {
    character(0)
  }
}

# Copy only the support files the chosen configuration needs.
.fs_copy_support <- function(template_dir, language, format, bibliography, csl, overwrite) {
  files <- .fs_csl_file(csl)
  if (bibliography == "separate") {
    files <- c(files, "references_main.bib", "references_app.bib", "cite-prefix.lua")
  } else {
    files <- c(files, "references.bib")
    if (language == "chinese") {
      files <- c(files, "fix-lang.lua", "localize-cnbib.lua")
      if (format == "word") files <- c(files, "center-caption.lua")
    }
  }
  for (f in unique(files)) {
    src <- file.path(template_dir, f)
    dest <- file.path("paper", f)
    if (file.exists(src) && (overwrite || !file.exists(dest))) {
      file.copy(src, dest, overwrite = overwrite)
    }
  }
}
