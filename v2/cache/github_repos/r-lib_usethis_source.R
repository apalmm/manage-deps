

# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/addin.R ---
#' Add minimal RStudio Addin binding
#'
#' This function helps you add a minimal
#' [RStudio Addin](https://rstudio.github.io/rstudioaddins/) binding to
#' `inst/rstudio/addins.dcf`.
#'
#' @param addin Name of the addin function, which should be defined in the
#' `R` folder.
#' @inheritParams use_template
#'
#' @export
use_addin <- function(addin = "new_addin", open = rlang::is_interactive()) {
  addin_dcf_path <- proj_path("inst", "rstudio", "addins.dcf")

  if (!file_exists(addin_dcf_path)) {
    create_directory(proj_path("inst", "rstudio"))
    file_create(addin_dcf_path)
    ui_bullets(c("v" = "Creating {.path {pth(addin_dcf_path)}}"))
  }

  addin_info <- render_template("addins.dcf", data = list(addin = addin))
  addin_info[length(addin_info) + 1] <- ""
  write_utf8(addin_dcf_path, addin_info, append = TRUE)
  ui_bullets(c(
    "v" = "Adding binding to {.fun {addin}} to {.path addins.dcf}"
  ))

  if (open) {
    edit_file(addin_dcf_path)
  }

  invisible(TRUE)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/air.R ---
#' Configure a project to use Air
#'
#' @description
#' [Air](https://posit-dev.github.io/air/) is an extremely fast R code
#' formatter. This function sets up a project to use Air. Specifically, it:
#'
#' - Creates an empty `air.toml` configuration file. If either an `air.toml` or
#'   `.air.toml` file already existed, nothing is changed. If the project is an
#'   R package, `.Rbuildignore` is updated to ignore this file.
#'
#' - Creates a `.vscode/` directory and adds recommended settings to
#'   `.vscode/settings.json` and `.vscode/extensions.json`. These settings are
#'   used by the Air extension installed through either VS Code or Positron, see
#'   the Installation section for more details. Specifically it:
#'
#'   - Sets `editor.formatOnSave = true` for R and Quarto files to enable
#'     formatting on every save.
#'
#'   - Sets `editor.defaultFormatter` to Air for R files to ensure that Air is
#'     always selected as the formatter for this project. Likewise, sets the
#'     default formatter for Quarto.
#'
#'   - Sets the Air extension as a "recommended" extension for this project,
#'     which triggers a notification for contributors coming to this project
#'     that don't yet have the Air extension installed.
#'
#'   If the project is an R package, `.Rbuildignore` is updated to ignore the
#'   `.vscode/` directory.
#'
#'   If you'd like to opt out of VS Code / Positron specific setup, set `vscode
#'   = FALSE`, but remember that even if you work in RStudio, other contributors
#'   may prefer another editor.
#'
#' Note that "using Air" breaks down into a few steps, and `use_air()` does
#' *one* of them. Here's an overview:
#'
#' * Installation: Air might already be included in your IDE (e.g. Positron) or
#'   can be added as an external formatter (e.g. RStudio) or as an extension
#'   (e.g. VS Code). Read the guide that applies to your situation:
#'   - [Air in an editor](https://posit-dev.github.io/air/editors.html)
#'   - [Air at the command line](https://posit-dev.github.io/air/cli.html)
#' * Configuration: `use_air()` does this!
#' * Invocation: There are many ways to run Air. In an IDE, you can expect
#'   support for moves like "format on save", "format selection", and so on.
#'   At the command line, you can format individual files or entire directories.
#' * Continuous integration: Two workflows are available for running Air via
#'   GitHub Actions: `format-suggest` or `format-check`. Learn more in
#'   [Air's documentation of its GHA integrations](https://posit-dev.github.io/air/integration-github-actions.html).
#'   You can set up either workflow in your project like so:
#'   ```
#'   use_github_action(url = "https://github.com/posit-dev/setup-air/blob/main/examples/format-suggest.yaml")
#'   use_github_action(url = "https://github.com/posit-dev/setup-air/blob/main/examples/format-check.yaml")
#'   ```
#'
#' @param vscode Either:
#'   - `TRUE` to set up VS Code and Positron specific Air settings. This is the
#'     default.
#'   - `FALSE` to opt out of those settings.
#'
#' @export
#' @examples
#' \dontrun{
#' # Prepare an R package or project to use Air
#' use_air()
#' }
use_air <- function(vscode = TRUE) {
  check_bool(vscode)

  ignore <- is_package()

  # Create empty `air.toml` if it doesn't exist
  create_air_toml(ignore = ignore)

  if (vscode) {
    create_vscode_directory(ignore = ignore)

    # Create project level `settings.json` if it doesn't exist,
    # and write in Air specific formatter settings
    path <- create_vscode_json_file("settings.json")
    write_air_vscode_settings_json(path)

    # Create project level `extensions.json` if it doesn't exist,
    # and write in Air as a recommended extension for this project
    path <- create_vscode_json_file("extensions.json")
    write_air_vscode_extensions_json(path)
  }

  ui_bullets(c(
    "_" = "Read {.href [Air's editors guide](https://posit-dev.github.io/air/editors.html)}
           to learn how to invoke Air in your preferred editor.",
    "_" = "Read {.href [Air's GitHub Actions guide](https://posit-dev.github.io/air/integration-github-actions.html)}
           to learn about GHA workflows that continuously run formatting checks."
  ))

  invisible(TRUE)
}

#' Creates an empty `air.toml`
#'
#' If either `air.toml` or `.air.toml` already exist, no new file is created.
#'
#' @keywords internal
#' @noRd
create_air_toml <- function(ignore = FALSE) {
  path <- path_first_existing(proj_path(c("air.toml", ".air.toml")))

  if (is.null(path)) {
    # No pre-existing configuration file, create it
    path <- proj_path("air.toml")
    file_create(path)
    ui_bullets(c("v" = "Creating {.path {pth(path)}}."))
  }

  if (ignore) {
    use_build_ignore(air_toml_regex(), escape = FALSE)
  }

  invisible(path)
}

uses_air <- function() {
  path <- path_first_existing(proj_path(c("air.toml", ".air.toml")))
  !is.null(path)
}

air_toml_regex <- function() {
  # Pre-escaped regex allowing both `air.toml` and `.air.toml`
  "^[.]?air[.]toml$"
}

create_vscode_json_file <- function(name) {
  arg_match(name, values = c("settings.json", "extensions.json"))

  path <- proj_path(".vscode", name)

  if (!file_exists(path)) {
    file_create(path)
    ui_bullets(c("v" = "Creating {.path {pth(path)}}."))
  }

  # Tools like jsonlite fail to read empty json files,
  # so if we've just created it, write in `{}`. The easiest
  # way to do that is to write an empty named list.
  if (is_file_empty(path)) {
    jsonlite::write_json(set_names(list()), path = path, pretty = TRUE)
  }

  invisible(path)
}

write_air_vscode_settings_json <- function(path) {
  settings <- jsonlite::read_json(path) %||% set_names(list())

  patch <- list(
    `[r]` = list(
      "editor.formatOnSave" = TRUE,
      "editor.defaultFormatter" = "Posit.air-vscode"
    ),
    `[quarto]` = list(
      "editor.formatOnSave" = TRUE,
      "editor.defaultFormatter" = "quarto.quarto"
    )
  )
  settings <- utils::modifyList(settings, patch)

  write_vscode_json(x = settings, path = path)
}

write_air_vscode_extensions_json <- function(path) {
  settings <- jsonlite::read_json(path)
  settings_recommendations <- settings[["recommendations"]]

  if (is.null(settings_recommendations)) {
    # Mock it
    settings_recommendations <- list()
  }

  already_recommended <- any(map_lgl(
    settings_recommendations,
    function(recommendation) {
      identical(recommendation, "Posit.air-vscode")
    }
  ))

  if (!already_recommended) {
    settings_recommendations <- c(
      settings_recommendations,
      list("Posit.air-vscode")
    )
  }

  settings[["recommendations"]] <- settings_recommendations

  write_vscode_json(x = settings, path = path)
}

#' Write JSON to a VS Code settings file
#'
#' @description
#' Small shim to use in place of [jsonlite::write_json()] when writing to
#' `.vscode/settings.json` or `.vscode/extensions.json`.
#'
#' Notably:
#'
#' - 4 space indent, as that is the standard indent level for these files
#'
#' - Auto unbox, because we want `TRUE` to show up as `true` not `[true]`.
#'
#' - Trims newlines from the right hand side after the ending `}`. Unfortunately
#'   setting `pretty = 4L` causes the special libyajl formatter to kick in, and
#'   that always adds a trailing newline after every `]` or `}`, even the last
#'   one, which we don't want.
#'
#' @keywords internal
#' @noRd
write_vscode_json <- function(x, path) {
  json <- jsonlite::toJSON(x, pretty = 4L, auto_unbox = TRUE)
  json <- base::trimws(json, which = "right")
  base::writeLines(json, path, useBytes = TRUE)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/author.R ---
#' Add an author to the `Authors@R` field in DESCRIPTION
#'
#' @description

#' `use_author()` adds a person to the `Authors@R` field of the DESCRIPTION
#' file, creating that field if necessary. It will not modify, e.g., the role(s)
#' or email of an existing author (judged using their "Given Family" name). For
#' that we recommend editing DESCRIPTION directly. Or, for programmatic use,
#' consider calling the more specialized functions available in the \pkg{desc}
#' package directly.
#'
#' `use_author()` also surfaces two other situations you might want to address:

#' * Explicit use of the fields `Author` or `Maintainer`. We recommend switching
#' to the more modern `Authors@R` field instead, because it offers richer
#' metadata for various downstream uses. (Note that `Authors@R` is *eventually*
#' processed to create `Author` and `Maintainer` fields, but only when the
#' `tar.gz` is built from package source.)

#' * Presence of the fake author placed by [create_package()] and
#' [use_description()]. This happens when \pkg{usethis} has to create a
#' DESCRIPTION file and the user hasn't given any author information via the
#' `fields` argument or the global option `"usethis.description"`. The
#' placeholder looks something like `First Last <first.last@example.com> [aut,
#' cre]` and `use_author()` offers to remove it in interactive sessions.
#'
#' @inheritParams utils::person
#' @inheritDotParams utils::person
#' @export
#' @examples
#' \dontrun{
#' use_author(
#'   given = "Lucy",
#'   family = "van Pelt",
#'   role = c("aut", "cre"),
#'   email = "lucy@example.com",
#'   comment = c(ORCID = "LUCY-ORCID-ID")
#' )
#'
#' use_author("Charlie", "Brown")
#' }
#'
use_author <- function(given = NULL, family = NULL, ..., role = "ctb") {
  check_is_package("use_author()")
  maybe_name(given)
  maybe_name(family)
  check_character(role)

  d <- proj_desc()
  challenge_legacy_author_fields(d)
  # We only need to consider Authors@R

  authors_at_r_already <- d$has_fields("Authors@R")
  if (authors_at_r_already) {
    check_author_is_novel(given, family, d)
  }
  # This person is not already in Authors@R

  author <- utils::person(given = given, family = family, role = role, ...)
  aut_fmt <- format(author, style = 'text')
  if (authors_at_r_already) {
    ui_bullets(c(
      "v" = "Adding to {.field Authors@R} in DESCRIPTION:",
      " " = "{aut_fmt}"
    ))
  } else {
    ui_bullets(c(
      "v" = "Creating {.field Authors@R} field in DESCRIPTION and adding:",
      " " = "{aut_fmt}"
    ))
  }
  d$add_author(given = given, family = family, role = role, ...)

  challenge_default_author(d)

  d$write()

  invisible(TRUE)
}

challenge_legacy_author_fields <- function(d = proj_desc()) {
  has_legacy_field <- d$has_fields("Author") || d$has_fields("Maintainer")
  if (!has_legacy_field) {
    return(invisible())
  }

  ui_bullets(c(
    "x" = "Found legacy {.field Author} and/or {.field Maintainer} field in
           DESCRIPTION.",
    " " = "usethis only supports modification of the {.field Authors@R} field.",
    "i" = "We recommend one of these paths forward:",
    "_" = "Delete the legacy fields and rebuild with {.fun use_author}; or",
    "_" = "Convert to {.field Authors@R} with
           {.fun desc::desc_coerce_authors_at_r}, then delete the legacy fields."
  ))
  if (ui_yep("Do you want to cancel this operation and sort that out first?")) {
    ui_abort("Cancelling.")
  }
  invisible()
}

check_author_is_novel <- function(
  given = NULL,
  family = NULL,
  d = proj_desc()
) {
  authors <- d$get_authors()
  authors_given <- purrr::map(authors, "given")
  authors_family <- purrr::map(authors, "family")
  m <- purrr::map2_lgl(authors_given, authors_family, function(x, y) {
    identical(x, given) && identical(y, family)
  })
  if (any(m)) {
    aut_name <- glue("{given %||% ''} {family %||% ''}")
    ui_abort(c(
      "x" = "{.val {aut_name}} already appears in {.field Authors@R}.",
      " " = "Please make the desired change directly in DESCRIPTION or call the
             {.pkg desc} package directly."
    ))
  }
  invisible()
}

challenge_default_author <- function(d = proj_desc()) {
  defaults <- usethis_description_defaults()
  default_author <- eval(parse(text = defaults[["Authors@R"]]))

  authors <- d$get_authors()
  m <- map_lgl(
    authors,
    # the `person` class is pretty weird!
    function(x) identical(x, unclass(default_author)[[1]])
  )

  if (any(m)) {
    ui_bullets(c(
      "i" = "{.field Authors@R} appears to include a placeholder author:",
      " " = "{format(default_author, style = 'text')}"
    ))
    if (is_interactive() && ui_yep("Would you like to remove it?")) {
      # TODO: Do I want to suppress this output?
      # Authors removed: First Last, NULL NULL.
      do.call(d$del_author, unclass(default_author)[[1]])
    }
  }

  return(invisible())
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/badge.R ---
#' README badges
#'
#' These helpers produce the markdown text you need in your README to include
#' badges that report information, such as the CRAN version or test coverage,
#' and link out to relevant external resources. To allow badges to be added
#' automatically, ensure your badge block starts with a line containing only
#' `<!-- badges: start -->` and ends with a line containing only
#' `<!-- badges: end -->`. The templates used by [use_readme_md()] and
#' [use_readme_rmd()] include this block.
#'
#' @details
#'
#' * `use_badge()`: a general helper used in all badge functions
#' * `use_bioc_badge()`: badge indicates [BioConductor build
#' status](https://bioconductor.org/developers/)
#' * `use_cran_badge()`: badge indicates what version of your package is
#' available on CRAN, powered by <https://www.r-pkg.org>
#' * `use_lifecycle_badge()`: badge declares the developmental stage of a
#' package according to <https://lifecycle.r-lib.org/articles/stages.html>.
#' * `use_r_universe_badge()`: `r lifecycle::badge("experimental")` badge
#' indicates what version of your package is available on [R-universe
#' ](https://r-universe.dev/search). It is assumed that you have already
#' completed the
#' [necessary R-universe setup](https://docs.r-universe.dev/publish/set-up.html).
#' * `use_binder_badge()`: badge indicates that your repository can be launched
#' in an executable environment on <https://mybinder.org/>
#' * `use_posit_cloud_badge()`: badge indicates that your repository can be launched
#' in a [Posit Cloud](https://posit.cloud) project
#' * `use_rscloud_badge()`: `r lifecycle::badge("deprecated")` Use
#' [use_posit_cloud_badge()] instead.
#'
#' @param badge_name Badge name. Used in error message and alt text
#' @param href,src Badge link and image src
#' @param stage Stage of the package lifecycle. One of "experimental",
#'   "stable", "superseded", or "deprecated".
#' @eval param_repo_spec()
#' @seealso [use_github_action()] helps with the setup of various continuous
#'   integration workflows, some of which will call these specialized badge
#'   helpers.
#'
#' @name badges
#' @examples
#' \dontrun{
#' use_cran_badge()
#' use_lifecycle_badge("stable")
#' }
NULL

#' @rdname badges
#' @export
use_badge <- function(badge_name, href, src) {
  path <- find_readme()
  if (is.null(path)) {
    ui_bullets(c(
      "!" = "Can't find a README for the current project.",
      "i" = "See {.fun usethis::use_readme_rmd} for help creating this file.",
      "i" = "Badge link will only be printed to screen."
    ))
    path <- "README"
  }
  changed <- block_append(
    glue("{badge_name} badge"),
    glue("[![{badge_name}]({src})]({href})"),
    path = path,
    block_start = badge_start,
    block_end = badge_end
  )

  if (changed && path_ext(path) == "Rmd") {
    ui_bullets(c(
      "_" = "Re-knit {.path {pth(path)}} with {.run devtools::build_readme()}."
    ))
  }
  invisible(changed)
}

#' @rdname badges
#' @export
use_cran_badge <- function() {
  check_is_package("use_cran_badge()")
  pkg <- project_name()

  src <- glue("https://www.r-pkg.org/badges/version/{pkg}")
  href <- glue("https://CRAN.R-project.org/package={pkg}")
  use_badge("CRAN status", href, src)

  invisible(TRUE)
}

#' @rdname badges
#' @export
use_bioc_badge <- function() {
  check_is_package("use_bioc_badge()")
  pkg <- project_name()

  src <- glue(
    "http://www.bioconductor.org/shields/build/release/bioc/{pkg}.svg"
  )
  href <- glue(
    "https://bioconductor.org/checkResults/release/bioc-LATEST/{pkg}"
  )
  use_badge("BioC status", href, src)

  invisible(TRUE)
}

#' @rdname badges
#' @export
use_lifecycle_badge <- function(stage) {
  check_is_package("use_lifecycle_badge()")
  pkg <- project_name()

  stage <- tolower(stage)
  stage <- arg_match0(stage, names(stages))
  colour <- stages[[stage]]

  src <- glue("https://img.shields.io/badge/lifecycle-{stage}-{colour}.svg")
  href <- glue("https://lifecycle.r-lib.org/articles/stages.html#{stage}")
  use_badge(paste0("Lifecycle: ", stage), href, src)

  invisible(TRUE)
}

stages <- c(
  experimental = "orange",
  stable = "brightgreen",
  superseded = "blue",
  deprecated = "orange"
)

#' @rdname badges
#' @param ref A Git branch, tag, or SHA
#' @param urlpath An optional `urlpath` component to add to the link, e.g.
#'   `"rstudio"` to open an RStudio IDE instead of a Jupyter notebook. See the
#'   [binder
#'   documentation](https://mybinder.readthedocs.io/en/latest/howto/user_interface.html)
#'    for additional examples.
#' @export
use_binder_badge <- function(ref = git_default_branch(), urlpath = NULL) {
  repo_spec <- target_repo_spec()

  if (is.null(urlpath)) {
    urlpath <- ""
  } else {
    urlpath <- glue("?urlpath={urlpath}")
  }
  url <- glue("https://mybinder.org/v2/gh/{repo_spec}/{ref}{urlpath}")
  img <- "https://mybinder.org/badge_logo.svg"
  use_badge("Launch binder", url, img)

  invisible(TRUE)
}
#' @rdname badges
#' @export
use_r_universe_badge <- function(repo_spec = NULL) {
  check_is_package("use_r_universe_badge()")
  pkg <- project_name()

  if (is.null(repo_spec)) {
    this_env <- current_call()
    tryCatch(
      github_url <- github_url(),
      error = function(e) {
        ui_abort(
          c(
            "x" = "Can't determine the R-universe owner of the {.pkg {pkg}} package.",
            "!" = "No GitHub URL found in DESCRIPTION or the Git remotes.",
            "i" = "Update the project configuration or provide an explicit {.arg repo_spec}."
          ),
          call = this_env
        )
      }
    )
    repo_spec <- parse_repo_url(github_url)[["repo_spec"]]
  }

  owner <- parse_repo_spec(repo_spec)[["owner"]]
  src <- glue("https://{owner}.r-universe.dev/{pkg}/badges/version")
  href <- glue("https://{owner}.r-universe.dev/{pkg}")
  use_badge("R-universe version", href, src)
}

#' @rdname badges
#' @param url A link to an existing [Posit Cloud](https://posit.cloud)
#'   project. See the [Posit Cloud
#'   documentation](https://posit.cloud/learn/guide#project-settings-access)
#'   for details on how to set project access and obtain a project link.
#' @export
use_posit_cloud_badge <- function(url) {
  check_name(url)
  project_url <- "posit[.]cloud/content"
  spaces_url <- "posit[.]cloud/spaces"
  if (grepl(project_url, url) || grepl(spaces_url, url)) {
    # TODO: Get posit logo hosted at https://github.com/simple-icons/simple-icons/
    # and add to end of img url as `?logo=posit` (or whatever slug we get)
    img <- "https://img.shields.io/badge/launch-posit%20cloud-447099?style=flat"
    use_badge("Launch Posit Cloud", url, img)
  } else {
    ui_abort(
      "
      {.fun usethis::use_posit_cloud_badge} requires a link to an
      existing Posit Cloud project of the form
      {.val https://posit.cloud/content/<project-id>} or
      {.val https://posit.cloud/spaces/<space-id>/content/<project-id>}."
    )
  }

  invisible(TRUE)
}

has_badge <- function(href) {
  readme_path <- proj_path("README.md")
  if (!file_exists(readme_path)) {
    return(FALSE)
  }

  readme <- read_utf8(readme_path)
  any(grepl(href, readme, fixed = TRUE))
}

# Badge data structure ----------------------------------------------------

badge_start <- "<!-- badges: start -->"
badge_end <- "<!-- badges: end -->"

find_readme <- function() {
  path_first_existing(proj_path(c("README.Rmd", "README.md")))
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/block.R ---
block_append <- function(
  desc,
  value,
  path,
  block_start = "# <<<",
  block_end = "# >>>",
  block_prefix = NULL,
  block_suffix = NULL,
  sort = FALSE
) {
  if (!is.null(path) && file_exists(path)) {
    lines <- read_utf8(path)
    if (all(value %in% lines)) {
      return(FALSE)
    }

    block_lines <- block_find(lines, block_start, block_end)
  } else {
    block_lines <- NULL
  }

  if (is.null(block_lines)) {
    ui_bullets(c(
      "_" = "Copy and paste the following lines into {.path {pth(path)}}:"
    ))
    ui_code_snippet(c(
      block_prefix,
      block_start,
      value,
      block_end,
      block_suffix
    ))
    return(FALSE)
  }

  ui_bullets(c("v" = "Adding {.val {desc}} to {.path {pth(path)}}."))

  start <- block_lines[[1]]
  end <- block_lines[[2]]
  block <- lines[seq2(start, end)]

  new_lines <- union(block, value)
  if (sort) {
    new_lines <- sort(new_lines)
  }

  lines <- c(
    lines[seq2(1, start - 1L)],
    new_lines,
    lines[seq2(end + 1L, length(lines))]
  )
  write_utf8(path, lines)

  TRUE
}

block_replace <- function(
  desc,
  value,
  path,
  block_start = "# <<<",
  block_end = "# >>>"
) {
  if (!is.null(path) && file_exists(path)) {
    lines <- read_utf8(path)
    block_lines <- block_find(lines, block_start, block_end)
  } else {
    block_lines <- NULL
  }

  if (is.null(block_lines)) {
    ui_bullets(c(
      "_" = "Copy and paste the following lines into {.path {pth(path)}}:"
    ))
    ui_code_snippet(c(block_start, value, block_end))
    return(invisible(FALSE))
  }

  start <- block_lines[[1]]
  end <- block_lines[[2]]
  block <- lines[seq2(start, end)]

  if (identical(value, block)) {
    return(invisible(FALSE))
  }

  ui_bullets(c("v" = "Replacing {desc} in {.path {pth(path)}}."))

  lines <- c(
    lines[seq2(1, start - 1L)],
    value,
    lines[seq2(end + 1L, length(lines))]
  )
  write_utf8(path, lines)
}


block_show <- function(path, block_start = "# <<<", block_end = "# >>>") {
  lines <- read_utf8(path)
  block <- block_find(lines, block_start, block_end)
  lines[seq2(block[[1]], block[[2]])]
}

block_find <- function(lines, block_start = "# <<<", block_end = "# >>>") {
  # No file
  if (is.null(lines)) {
    return(NULL)
  }

  start <- which(lines == block_start)
  end <- which(lines == block_end)

  # No block
  if (length(start) == 0 && length(end) == 0) {
    return(NULL)
  }

  if (!(length(start) == 1 && length(end) == 1 && start < end)) {
    ui_abort(c(
      "Invalid block specification.",
      "Must start with {.code {block_start}} and end with
       {.code {block_end}}."
    ))
  }

  c(start + 1L, end - 1L)
}

block_create <- function(
  lines = character(),
  block_start = "# <<<",
  block_end = "# >>>"
) {
  c(block_start, unique(lines), block_end)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/browse.R ---
#' Visit important project-related web pages
#'
#' These functions take you to various web pages associated with a project
#' (often, an R package) and return the target URL(s) invisibly. To form
#' these URLs we consult:
#' * Git remotes configured for the active project that appear to be hosted on
#'   a GitHub deployment
#' * DESCRIPTION file for the active project or the specified `package`. The
#'   DESCRIPTION file is sought first in the local package library and then
#'   on CRAN.
#' * Fixed templates:
#'   - Circle CI: `https://circleci.com/gh/{OWNER}/{PACKAGE}`
#'   - CRAN landing page: `https://cran.r-project.org/package={PACKAGE}`
#'   - GitHub mirror of a CRAN package: `https://github.com/cran/{PACKAGE}`
#'   Templated URLs aren't checked for existence, so there is no guarantee
#'   there will be content at the destination.
#'
#' @details
#' * `browse_package()`: Assembles a list of URLs and lets user choose one to
#'   visit in a web browser. In a non-interactive session, returns all
#'   discovered URLs.
#' * `browse_project()`: Thin wrapper around `browse_package()` that always
#'   targets the active usethis project.
#' * `browse_github()`: Visits a GitHub repository associated with the project.
#'   In the case of a fork, you might be asked to specify if you're interested
#'   in the source repo or your fork.
#' * `browse_github_issues()`: Visits the GitHub Issues index or one specific
#'   issue.
#' * `browse_github_pulls()`: Visits the GitHub Pull Request index or one
#'   specific pull request.
#' * `browse_circleci()`: Visits the project's page on
#'   [Circle CI](https://circleci.com).
#' * `browse_cran()`: Visits the package on CRAN, via the canonical URL.
#'
#' @param package Name of package. If `NULL`, the active project is targeted,
#'   regardless of whether it's an R package or not.
#' @param number Optional, to specify an individual GitHub issue or pull
#'   request. Can be a number or `"new"`.
#'
#' @examples
#' # works on the active project
#' # browse_project()
#'
#' browse_package("httr")
#' browse_github("gh")
#' browse_github_issues("fs")
#' browse_github_issues("fs", 1)
#' browse_github_pulls("curl")
#' browse_github_pulls("curl", 183)
#' browse_cran("MASS")
#' @name browse-this
NULL

#' @export
#' @rdname browse-this
browse_package <- function(package = NULL) {
  maybe_name(package)

  if (is.null(package)) {
    check_is_project()
  }

  urls <- character()
  details <- list()

  if (is.null(package) && uses_git()) {
    grl <- github_remote_list(these = NULL)
    ord <- c(
      which(grl$remote == "origin"),
      which(grl$remote == "upstream"),
      which(!grl$remote %in% c("origin", "upstream"))
    )
    grl <- grl[ord, ]
    grl <- set_names(grl$url, nm = grl$remote)
    parsed <- parse_github_remotes(grl)
    urls <- c(
      urls,
      glue_data(parsed, "https://{host}/{repo_owner}/{repo_name}")
    )
    details <- c(
      details,
      map(parsed$name, \(x) cli::cli_fmt(cli::cli_text("{.val {x}} remote")))
    )
  }

  desc_urls_dat <- desc_urls(package, include_cran = TRUE)
  urls <- c(urls, desc_urls_dat$url)
  details <- c(
    details,
    map(
      desc_urls_dat$desc_field,
      \(x) {
        if (is.na(x)) {
          "CRAN"
        } else {
          cli::cli_fmt(cli::cli_text("{.field {x}} field in DESCRIPTION"))
        }
      }
    )
  )
  if (length(urls) == 0) {
    ui_bullets(c(x = "Can't find any URLs."))
    return(invisible(character()))
  }

  if (!is_interactive()) {
    return(invisible(urls))
  }

  prompt <- "Which URL do you want to visit? (0 to exit)"
  pretty <- purrr::map2(
    format(urls, justify = "left"),
    details,
    \(x, y) glue("{x} ({y})")
  )
  choice <- utils::menu(title = prompt, choices = pretty)
  if (choice == 0) {
    return(invisible(character()))
  }
  view_url(urls[choice])
}

#' @export
#' @rdname browse-this
browse_project <- function() browse_package(NULL)

#' @export
#' @rdname browse-this
browse_github <- function(package = NULL) {
  view_url(github_url(package))
}

#' @export
#' @rdname browse-this
browse_github_issues <- function(package = NULL, number = NULL) {
  view_url(github_url(package), "issues", number)
}

#' @export
#' @rdname browse-this
browse_github_pulls <- function(package = NULL, number = NULL) {
  pull <- if (is.null(number)) "pulls" else "pull"
  view_url(github_url(package), pull, number)
}
#' @export
#' @rdname browse-this
browse_github_actions <- function(package = NULL) {
  view_url(github_url(package), "actions")
}

#' @export
#' @rdname browse-this
browse_circleci <- function(package = NULL) {
  gh <- github_url(package)
  circle_url <- "circleci.com/gh"
  view_url(sub("github.com", circle_url, gh))
}

#' @export
#' @rdname browse-this
browse_cran <- function(package = NULL) {
  view_url(cran_home(package))
}

# Try to get a GitHub repo spec from these places:
# 1. Remotes associated with GitHub (active project)
# 2. BugReports/URL fields of DESCRIPTION (active project or arbitrary
#    installed package)
github_url <- function(package = NULL) {
  maybe_name(package)

  if (is.null(package)) {
    check_is_project()
    url <- github_url_from_git_remotes()
    if (!is.null(url)) {
      return(url)
    }
  }

  desc_urls_dat <- desc_urls(package)

  if (is.null(desc_urls_dat)) {
    if (is.null(package)) {
      ui_abort(c(
        "Project {.val {project_name()}} has no DESCRIPTION file and
         has no GitHub remotes configured.",
        "No way to discover URLs."
      ))
    } else {
      ui_abort(c(
        "Can't find DESCRIPTION for package {.pkg {package}} locally
         or on CRAN.",
        "No way to discover URLs."
      ))
    }
  }

  desc_urls_dat <- desc_urls_dat[desc_urls_dat$is_github, ]

  if (nrow(desc_urls_dat) > 0) {
    parsed <- parse_github_remotes(desc_urls_dat$url[[1]])
    return(glue_data_chr(parsed, "https://{host}/{repo_owner}/{repo_name}"))
  }

  if (is.null(package)) {
    ui_abort(
      "
      Project {.val {project_name()}} has no GitHub remotes configured
      and has no GitHub URLs in DESCRIPTION."
    )
  }
  cli::cli_warn(c(
    "!" = "Package {.val {package}} has no GitHub URLs in DESCRIPTION.",
    " " = "Trying the GitHub CRAN mirror."
  ))
  glue_chr("https://github.com/cran/{package}")
}

cran_home <- function(package = NULL) {
  package <- package %||% project_name()
  glue_chr("https://cran.r-project.org/package={package}")
}

# returns NULL, if no DESCRIPTION found
# returns 0-row data frame, if DESCRIPTION holds no URLs
# returns data frame, if successful
# include_cran whether to include CRAN landing page, if we consult it
desc_urls <- function(package = NULL, include_cran = FALSE, desc = NULL) {
  maybe_desc <- purrr::possibly(desc::desc, otherwise = NULL)
  desc_from_cran <- FALSE

  if (is.null(desc)) {
    if (is.null(package)) {
      desc <- maybe_desc(file = proj_get())
      if (is.null(desc)) {
        return()
      }
    } else {
      desc <- maybe_desc(package = package)
      if (is.null(desc)) {
        cran_desc_url <-
          glue("https://cran.rstudio.com/web/packages/{package}/DESCRIPTION")
        suppressWarnings(
          desc <- maybe_desc(text = readLines(cran_desc_url))
        )
        if (is.null(desc)) {
          return()
        }
        desc_from_cran <- TRUE
      }
    }
  }

  url <- desc$get_urls()
  bug_reports <- desc$get_field("BugReports", default = character())
  cran <-
    if (include_cran && desc_from_cran) cran_home(package) else character()
  dat <- data.frame(
    desc_field = c(
      rep_len("URL", length.out = length(url)),
      rep_len("BugReports", length.out = length(bug_reports)),
      rep_len(NA, length.out = length(cran))
    ),
    url = c(url, bug_reports, cran),
    stringsAsFactors = FALSE
  )
  dat <- cbind(dat, re_match(dat$url, github_remote_regex))
  # TODO: could have a more sophisticated understanding of GitHub deployments
  dat$is_github <- !is.na(dat$.match) & grepl("github", dat$host)
  dat[c("url", "desc_field", "is_github")]
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/ci.R ---
#' Continuous integration setup and badges
#'
#' @description
#' `r lifecycle::badge("questioning")`
#'
#' These functions are not actively used by the tidyverse team, and may not
#' currently work. Use at your own risk.
#'
#' Sets up third-party continuous integration (CI) services for an R package
#' on GitLab or CircleCI. These functions:
#'
#' * Add service-specific configuration files and add them to `.Rbuildignore`.
#' * Activate a service or give the user a detailed prompt.
#' * Provide the markdown to insert a badge into README.
#'
#' @section `use_gitlab_ci()`:
#' Adds a basic `.gitlab-ci.yml` to the top-level directory of a package. This
#' is a configuration file for the [GitLab
#' CI/CD](https://docs.gitlab.com/ee/ci/) continuous integration service.
#' @export
use_gitlab_ci <- function() {
  check_uses_git()
  new <- use_template(
    "gitlab-ci.yml",
    ".gitlab-ci.yml",
    ignore = TRUE
  )
  if (!new) {
    return(invisible(FALSE))
  }

  invisible(TRUE)
}

#' @section `use_circleci()`:
#' Adds a basic `.circleci/config.yml` to the top-level directory of a package.
#' This is a configuration file for the [CircleCI](https://circleci.com/)
#' continuous integration service.
#' @param browse Open a browser window to enable automatic builds for the
#'   package.
#' @param image The Docker image to use for build. Must be available on
#'   [DockerHub](https://hub.docker.com). The
#'   [rocker/verse](https://hub.docker.com/r/rocker/verse) image includes
#'   TeXLive, pandoc, and the tidyverse packages. For a minimal image, try
#'   [rocker/r-ver](https://hub.docker.com/r/rocker/r-ver). To specify a version
#'   of R, change the tag from `latest` to the version you want, e.g.
#'   `rocker/r-ver:3.5.3`.
#' @export
#' @rdname use_gitlab_ci
use_circleci <- function(
  browse = rlang::is_interactive(),
  image = "rocker/verse:latest"
) {
  repo_spec <- target_repo_spec()
  use_directory(".circleci", ignore = TRUE)
  new <- use_template(
    "circleci-config.yml",
    ".circleci/config.yml",
    data = list(package = project_name(), image = image),
    ignore = TRUE
  )
  if (!new) {
    return(invisible(FALSE))
  }

  use_circleci_badge(repo_spec)
  circleci_activate(spec_owner(repo_spec), browse)

  invisible(TRUE)
}

#' @section `use_circleci_badge()`:
#' Only adds the [Circle CI](https://circleci.com/) badge. Use for a project
#'  where Circle CI is already configured.
#' @rdname use_gitlab_ci
#' @eval param_repo_spec()
#' @export
use_circleci_badge <- function(repo_spec = NULL) {
  repo_spec <- repo_spec %||% target_repo_spec()
  url <- glue("https://circleci.com/gh/{repo_spec}")
  img <- glue("{url}.svg?style=svg")
  use_badge("CircleCI build status", url, img)
}

circleci_activate <- function(owner, browse = is_interactive()) {
  url <- glue("https://circleci.com/add-projects/gh/{owner}")
  ui_bullets(c(
    "_" = "Turn on CircleCI for your repo at {.url {url}}."
  ))
  if (browse) {
    utils::browseURL(url)
  }
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/citation.R ---
#' Create a CITATION template
#'
#' Use this if you want to encourage users of your package to cite an
#' article or book.
#'
#' @export
use_citation <- function() {
  check_is_package()

  use_directory("inst")
  use_template(
    "citation-template.R",
    path("inst", "CITATION"),
    open = TRUE
  )
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/code-of-conduct.R ---
#' Add a code of conduct
#'
#' Adds a `CODE_OF_CONDUCT.md` file to the active project and lists in
#' `.Rbuildignore`, in the case of a package. The goal of a code of conduct is
#' to foster an environment of inclusiveness, and to explicitly discourage
#' inappropriate behaviour. The template comes from
#' <https://www.contributor-covenant.org>, version 2.1:
#' <https://www.contributor-covenant.org/version/2/1/code_of_conduct/>.
#'
#' If your package is going to CRAN, the link to the CoC in your README must
#' be an absolute link to a rendered website as `CODE_OF_CONDUCT.md` is not
#' included in the package sent to CRAN. `use_code_of_conduct()` will
#' automatically generate this link if (1) you use pkgdown and (2) have set the
#' `url` field in `_pkgdown.yml`; otherwise it will link to a copy of the CoC
#' on <https://www.contributor-covenant.org>.
#'
#' @param contact Contact details for making a code of conduct report.
#'   Usually an email address.
#' @param path Path of the directory to put `CODE_OF_CONDUCT.md` in, relative to
#'   the active project. Passed along to [use_directory()]. Default is to locate
#'   at top-level, but `.github/` is also common.
#'
#' @export
use_code_of_conduct <- function(contact, path = NULL) {
  if (missing(contact)) {
    ui_abort(
      "
      {.fun use_code_of_conduct} requires contact details in first argument."
    )
  }

  new <- use_coc(contact = contact, path = path)

  href <- pkgdown_url(pedantic = TRUE) %||%
    "https://contributor-covenant.org/version/2/1"
  href <- sub("/$", "", href)
  href <- paste0(href, "/CODE_OF_CONDUCT.html")

  ui_bullets(c(
    "_" = "You may also want to describe the code of conduct in your README:"
  ))
  ui_code_snippet(
    "
    ## Code of Conduct

    Please note that the {project_name()} project is released with a \\
    [Contributor Code of Conduct]({href}). By contributing to this project, \\
    you agree to abide by its terms.",
    language = ""
  )

  invisible(new)
}

use_coc <- function(contact, path = NULL) {
  if (!is.null(path)) {
    use_directory(path, ignore = is_package())
  }
  save_as <- path_join(c(path, "CODE_OF_CONDUCT.md"))

  use_template(
    "CODE_OF_CONDUCT.md",
    save_as = save_as,
    data = list(contact = contact),
    ignore = is_package() && is.null(path)
  )
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/course.R ---
## see end of file for some cURL notes

#' Download and unpack a ZIP file
#'
#' Functions to download and unpack a ZIP file into a local folder of files,
#' with very intentional default behaviour. Useful in pedagogical settings or
#' anytime you need a large audience to download a set of files quickly and
#' actually be able to find them. After download, the new folder is opened in
#' a new session of the user's IDE, if possible, or in the default file manager
#' provided by the operating system. The underlying helpers are documented in
#' [use_course_details].
#'
#' @param url Link to a ZIP file containing the materials. To reduce the chance
#'   of typos in live settings, these shorter forms are accepted:
#'
#'   * GitHub repo spec: "OWNER/REPO". Equivalent to
#'     `https://github.com/OWNER/REPO/DEFAULT_BRANCH.zip`.
#'   * bit.ly, pos.it, or rstd.io shortlinks: "bit.ly/xxx-yyy-zzz",
#'     "pos.it/foofy" or "rstd.io/foofy". The instructor must then arrange for
#'     the shortlink to point to a valid download URL for the target ZIP file.
#'     The helper [create_download_url()] helps to create such URLs for GitHub,
#'     DropBox, and Google Drive.
#' @param destdir Destination for the new folder. Defaults to the location
#'   stored in the global option `usethis.destdir`, if defined, or to the user's
#'   Desktop or similarly conspicuous place otherwise.
#' @param cleanup Whether to delete the original ZIP file after unpacking its
#'   contents. In an interactive setting, `NA` leads to a menu where user can
#'   approve the deletion (or decline).
#'
#' @return Path to the new directory holding the unpacked ZIP file, invisibly.
#' @name zip-utils
#' @examples
#' \dontrun{
#' # download the source of usethis from GitHub, behind a bit.ly shortlink
#' use_course("bit.ly/usethis-shortlink-example")
#' use_course("http://bit.ly/usethis-shortlink-example")
#'
#' # download the source of rematch2 package from CRAN
#' use_course("https://cran.r-project.org/bin/windows/contrib/4.5/rematch2_2.1.2.zip")
#'
#' # download the source of rematch2 package from GitHub, 4 ways
#' use_course("r-lib/rematch2")
#' use_course("https://api.github.com/repos/r-lib/rematch2/zipball/HEAD")
#' use_course("https://api.github.com/repos/r-lib/rematch2/zipball/main")
#' use_course("https://github.com/r-lib/rematch2/archive/main.zip")
#' }
NULL

#' @describeIn zip-utils
#'
#'  Designed with live workshops in mind. Includes intentional friction to
#'  highlight the download destination. Workflow:
#' * User executes, e.g., `use_course("bit.ly/xxx-yyy-zzz")`.
#' * User is asked to notice and confirm the location of the new folder. Specify
#'   `destdir` or configure the `"usethis.destdir"` option to prevent this.
#' * User is asked if they'd like to delete the ZIP file.
#' * If possible, the new folder is launched in a new session of the user's IDE.
#'   Otherwise, the folder is opened in the file manager, e.g. Finder on macOS
#'   or File Explorer on Windows.
#' @export
use_course <- function(url, destdir = getOption("usethis.destdir")) {
  url <- normalize_url(url)
  destdir_not_specified <- is.null(destdir)
  destdir <- user_path_prep(destdir %||% conspicuous_place())
  check_path_is_directory(destdir)

  if (destdir_not_specified && is_interactive()) {
    ui_bullets(c(
      "i" = "Downloading into {.path {pth(destdir)}}.",
      "_" = "Prefer a different location? Cancel, try again, and specify
             {.arg destdir}."
    ))
    if (ui_nah("OK to proceed?")) {
      ui_bullets(c(x = "Cancelling download."))
      return(invisible())
    }
  }

  ui_bullets(c("v" = "Downloading from {.url {url}}."))
  zipfile <- tidy_download(url, destdir)
  ui_bullets(c("v" = "Download stored in {.path {pth(zipfile)}}."))
  check_is_zip(attr(zipfile, "content-type"))
  tidy_unzip(zipfile, cleanup = NA)
}

#' @describeIn zip-utils
#'
#' More useful in day-to-day work. Downloads in current working directory, by
#' default, and allows `cleanup` behaviour to be specified.
#' @export
use_zip <- function(
  url,
  destdir = getwd(),
  cleanup = if (rlang::is_interactive()) NA else FALSE
) {
  url <- normalize_url(url)
  check_path_is_directory(destdir)
  ui_bullets(c("v" = "Downloading from {.url {url}}."))
  zipfile <- tidy_download(url, destdir)
  ui_bullets(c("v" = "Download stored in {.path {pth(zipfile)}}."))
  check_is_zip(attr(zipfile, "content-type"))
  tidy_unzip(zipfile, cleanup)
}

#' Helpers to download and unpack a ZIP file
#'
#' @description
#' Details on the internal and helper functions that power [use_course()] and
#' [use_zip()]. Only `create_download_url()` is exported.
#'
#' @name use_course_details
#' @keywords internal
#' @usage
#' tidy_download(url, destdir = getwd())
#' tidy_unzip(zipfile, cleanup = FALSE)
#'
#' @aliases tidy_download tidy_unzip

#' @param url A GitHub, DropBox, or Google Drive URL.
#' * For `create_download_url()`: A URL copied from a web browser.
#' * For `tidy_download()`: A download link for a ZIP file, possibly behind a
#'   shortlink or other redirect. `create_download_url()` can be helpful for
#'   creating this URL from typical browser URLs.
#' @param destdir Path to existing local directory where the ZIP file will be
#'   stored. Defaults to current working directory, but note that [use_course()]
#'   has different default behavior.
#' @param zipfile Path to local ZIP file.
#' @param cleanup Whether to delete the ZIP file after unpacking. In an
#'   interactive session, `cleanup = NA` leads to asking the user if they
#'   want to delete or keep the ZIP file.

#' @section tidy_download():
#'
#' ```
#' # how it's used inside use_course()
#' tidy_download(
#'   # url has been processed with internal helper normalize_url()
#'   url,
#'   # conspicuous_place() = `getOption('usethis.destdir')` or desktop or home
#'   # directory or working directory
#'   destdir = destdir %||% conspicuous_place()
#' )
#' ```
#'
#' Special-purpose function to download a ZIP file and automatically determine
#' the file name, which often determines the folder name after unpacking.
#' Developed with DropBox and GitHub as primary targets, possibly via
#' shortlinks. Both platforms offer a way to download an entire folder or repo
#' as a ZIP file, with information about the original folder or repo transmitted
#' in the `Content-Disposition` header. In the absence of this header, a
#' filename is generated from the input URL. In either case, the filename is
#' sanitized. Returns the path to downloaded ZIP file, invisibly.
#'
#' `tidy_download()` is setup to retry after a download failure. In an
#' interactive session, it asks for user's consent. All retries use a longer
#' connect timeout.
#'
#' ## DropBox
#'
#' To make a folder available for ZIP download, create a shared link for it:
#' * <https://help.dropbox.com/share/create-and-share-link>
#'
#' A shared link will have this form:
#' ```
#' https://www.dropbox.com/sh/12345abcde/6789wxyz?dl=0
#' ```
#' Replace the `dl=0` at the end with `dl=1` to create a download link:
#' ```
#' https://www.dropbox.com/sh/12345abcde/6789wxyz?dl=1
#' ```
#' You can use `create_download_url()` to do this conversion.
#'
#' This download link (or a shortlink that points to it) is suitable as input
#' for `tidy_download()`. After one or more redirections, this link will
#' eventually lead to a download URL. For more details, see
#' <https://help.dropbox.com/share/force-download> and
#' <https://help.dropbox.com/sync/download-entire-folders>.
#'
#' ## GitHub
#'
#' Click on the repo's "Clone or download" button, to reveal a "Download ZIP"
#' button. Capture this URL, which will have this form:
#' ```
#' https://github.com/r-lib/usethis/archive/main.zip
#' ```
#' This download link (or a shortlink that points to it) is suitable as input
#' for `tidy_download()`. After one or more redirections, this link will
#' eventually lead to a download URL. Here are other links that also lead to
#' ZIP download, albeit with a different filenaming scheme (REF could be a
#' branch name, a tag, or a SHA):
#' ```
#' https://github.com/github.com/r-lib/usethis/zipball/HEAD
#' https://api.github.com/repos/r-lib/rematch2/zipball/REF
#' https://api.github.com/repos/r-lib/rematch2/zipball/HEAD
#' https://api.github.com/repos/r-lib/usethis/zipball/REF
#' ```
#'
#' You can use `create_download_url()` to create the "Download ZIP" URL from
#' a typical GitHub browser URL.
#'
#' ## Google Drive
#'
#' To our knowledge, it is not possible to download a Google Drive folder as a
#' ZIP archive. It is however possible to share a ZIP file stored on Google
#' Drive. To get its URL, click on "Get the shareable link" (within the "Share"
#' menu). This URL doesn't allow for direct download, as it's designed to be
#' processed in a web browser first. Such a sharing link looks like:
#'
#' ```
#' https://drive.google.com/open?id=123456789xxyyyzzz
#' ```
#'
#' To be able to get the URL suitable for direct download, you need to extract
#' the "id" element from the URL and include it in this URL format:
#'
#' ```
#' https://drive.google.com/uc?export=download&id=123456789xxyyyzzz
#' ```
#'
#' Use `create_download_url()` to perform this transformation automatically.
#'
#' @section tidy_unzip():
#'
#' Special-purpose function to unpack a ZIP file and (attempt to) create the
#' directory structure most people want. When unpacking an archive, it is easy
#' to get one more or one less level of nesting than you expected.
#'
#' It's especially important to finesse the directory structure here: we want
#' the same local result when unzipping the same content from either GitHub or
#' DropBox ZIP files, which pack things differently. Here is the intent:
#' * If the ZIP archive `foo.zip` does not contain a single top-level directory,
#' i.e. it is packed as "loose parts", unzip into a directory named `foo`.
#' Typical of DropBox ZIP files.
#' * If the ZIP archive `foo.zip` has a single top-level directory (which, by
#' the way, is not necessarily called "foo"), unpack into said directory.
#' Typical of GitHub ZIP files.
#'
#' Returns path to the directory holding the unpacked files, invisibly.
#'
#' **DropBox:**
#' The ZIP files produced by DropBox are special. The file list tends to contain
#' a spurious directory `"/"`, which we ignore during unzip. Also, if the
#' directory is a Git repo and/or RStudio Project, we unzip-ignore various
#' hidden files, such as `.RData`, `.Rhistory`, and those below `.git/` and
#' `.Rproj.user`.
#'
#' @examples
#' \dontrun{
#' tidy_download("https://github.com/r-lib/rematch2/archive/main.zip")
#' tidy_unzip("rematch2-main.zip")
#' }
NULL

# 1. downloads from `url`
# 2. calls a retry-capable helper to download the ZIP file
# 3. determines filename from content-description header (with fallbacks)
# 4. returned path has content-type and content-description as attributes
tidy_download <- function(url, destdir = getwd()) {
  check_path_is_directory(destdir)
  tmp <- file_temp("tidy-download-")

  h <- download_url(url, destfile = tmp)
  cli::cat_line()

  cd <- content_disposition(h)
  base_name <- make_filename(cd, fallback = path_file(url))
  full_path <- path(destdir, base_name)

  if (!can_overwrite(full_path)) {
    ui_abort(
      "
      Cancelling download, to avoid overwriting {.path {pth(full_path)}}."
    )
  }
  attr(full_path, "content-type") <- content_type(h)
  attr(full_path, "content-disposition") <- cd

  file_move(tmp, full_path)
  invisible(full_path)
}

download_url <- function(
  url,
  destfile,
  handle = curl::new_handle(),
  n_tries = 3,
  retry_connecttimeout = 40L
) {
  handle_options <- list(noprogress = FALSE, progressfunction = progress_fun)
  curl::handle_setopt(handle, .list = handle_options)

  we_should_retry <- function(i, n_tries, status) {
    if (i >= n_tries) {
      FALSE
    } else if (inherits(status, "error")) {
      # TODO: find a way to detect a (connect) timeout more specifically?
      # https://github.com/jeroen/curl/issues/154
      # https://ec.haxx.se/usingcurl/usingcurl-timeouts
      # "Failing to connect within the given time will cause curl to exit with a
      # timeout exit code (28)."
      # (however, note that all timeouts lead to this same exit code)
      # https://ec.haxx.se/usingcurl/usingcurl-returns
      # "28. Operation timeout. The specified time-out period was reached
      # according to the conditions. curl offers several timeouts, and this exit
      # code tells one of those timeout limits were reached."
      # https://github.com/curl/curl/blob/272282a05416e42d2cc4a847a31fd457bc6cc827/lib/strerror.c#L143-L144
      # "Timeout was reached" <-- actual message we could potentially match
      TRUE
    } else {
      FALSE
    }
  }

  status <- try_download(url, destfile, handle = handle)
  if (inherits(status, "error") && is_interactive()) {
    ui_bullets(c("x" = status$message))
    if (
      ui_nah(c(
        "!" = "Download failed :(",
        "i" = "See above for everything we know about why it failed.",
        " " = "Shall we try a couple more times, with a longer timeout?"
      ))
    ) {
      n_tries <- 1
    }
  }

  i <- 1
  # invariant: we have made i download attempts
  while (we_should_retry(i, n_tries, status)) {
    if (i == 1) {
      curl::handle_setopt(
        handle,
        .list = c(connecttimeout = retry_connecttimeout)
      )
    }
    i <- i + 1
    ui_bullets(c("i" = "Retrying download ... attempt {i}."))
    status <- try_download(url, destfile, handle = handle)
  }

  if (inherits(status, "error")) {
    stop(status)
  }

  invisible(handle)
}

try_download <- function(url, destfile, quiet = FALSE, mode = "wb", handle) {
  tryCatch(
    curl::curl_download(
      url = url,
      destfile = destfile,
      quiet = quiet,
      mode = mode,
      handle = handle
    ),
    error = function(e) e
  )
}

tidy_unzip <- function(zipfile, cleanup = FALSE) {
  base_path <- path_dir(zipfile)

  filenames <- utils::unzip(zipfile, list = TRUE)[["Name"]]

  ## deal with DropBox's peculiar habit of including "/" as a file --> drop it
  filenames <- filenames[filenames != "/"]

  ## DropBox ZIP files often include lots of hidden R, RStudio, and Git files
  filenames <- filenames[keep_lgl(filenames)]

  parents <- path_before_slash(filenames)
  unique_parents <- unique(parents)
  if (length(unique_parents) == 1 && unique_parents != "") {
    target <- path(base_path, unique_parents)
    utils::unzip(zipfile, files = filenames, exdir = base_path)
  } else {
    # there is no parent; archive contains loose parts
    target <- path_ext_remove(zipfile)
    utils::unzip(zipfile, files = filenames, exdir = target)
  }
  ui_bullets(c(
    "v" = "Unpacking ZIP file into {.path {pth(target, base_path)}}
           ({length(filenames)} file{?s} extracted)."
  ))

  if (isNA(cleanup)) {
    cleanup <- is_interactive() &&
      ui_yep(
        "Shall we delete the ZIP file ({.path {pth(zipfile, base_path)}})?"
      )
  }

  if (isTRUE(cleanup)) {
    ui_bullets(c("v" = "Deleting {.path {pth(zipfile, base_path)}}."))
    file_delete(zipfile)
  }

  if (is_interactive()) {
    proj_root <- proj_find(target)
    if (rstudio_available() && rstudioapi::hasFun("openProject")) {
      if (is.null(proj_root)) {
        file_create(path(target, ".here"))
      }
      ui_bullets(c(
        "v" = "Opening {.path {pth(target, base = NA)}} in a new session."
      ))
      rstudioapi::openProject(target, newSession = TRUE)
    } else if (!in_rstudio_server()) {
      ui_bullets(c(
        "v" = "Opening {.path {pth(target, base_path)}} in the file manager."
      ))
      utils::browseURL(path_real(target))
    }
  }

  invisible(unclass(target))
}

#' @rdname use_course_details
#' @examples
#' # GitHub
#' create_download_url("https://github.com/r-lib/usethis")
#' create_download_url("https://github.com/r-lib/usethis/issues")
#'
#' # DropBox
#' create_download_url("https://www.dropbox.com/sh/12345abcde/6789wxyz?dl=0")
#'
#' # Google Drive
#' create_download_url("https://drive.google.com/open?id=123456789xxyyyzzz")
#' create_download_url("https://drive.google.com/open?id=123456789xxyyyzzz/view")
#' @export
create_download_url <- function(url) {
  check_name(url)
  stopifnot(grepl("^http[s]?://", url))

  switch(
    classify_url(url),
    drive = modify_drive_url(url),
    dropbox = modify_dropbox_url(url),
    github = modify_github_url(url),
    hopeless_url(url)
  )
}

classify_url <- function(url) {
  if (grepl("drive.google.com", url)) {
    return("drive")
  }
  if (grepl("dropbox.com/sh", url)) {
    return("dropbox")
  }
  if (grepl("github.com", url)) {
    return("github")
  }
  "unknown"
}

modify_drive_url <- function(url) {
  # id-isolating approach taken from the gargle / googleverse
  id_loc <- regexpr("/d/([^/])+|/folders/([^/])+|id=([^/])+", url)
  if (id_loc == -1) {
    return(hopeless_url(url))
  }
  id <- gsub("/d/|/folders/|id=", "", regmatches(url, id_loc))
  glue_chr("https://drive.google.com/uc?export=download&id={id}")
}

modify_dropbox_url <- function(url) {
  gsub("dl=0", "dl=1", url)
}

modify_github_url <- function(url) {
  # TO CONSIDER: one could use the API for this, which might be more proper and
  # would work if auth is needed
  # https://docs.github.com/en/free-pro-team@latest/rest/reference/repos#download-a-repository-archive-zip
  # https://api.github.com/repos/OWNER/REPO/zipball/
  # but then, in big workshop settings, we might see rate limit problems or
  # get blocked because of too many token-free requests from same IP
  parsed <- parse_github_remotes(url)
  glue_data_chr(
    parsed,
    "{protocol}://{host}/{repo_owner}/{repo_name}/zipball/HEAD"
  )
}

hopeless_url <- function(url) {
  ui_bullets(c(
    "!" = "URL does not match a recognized form for Google Drive or DropBox;
           no change made."
  ))
  url
}

normalize_url <- function(url) {
  check_name(url)
  has_scheme <- grepl("^http[s]?://", url)

  if (has_scheme) {
    return(url)
  }

  if (!is_shortlink(url)) {
    url <- tryCatch(
      expand_github(url),
      error = function(e) url
    )
  }

  paste0("https://", url)
}

is_shortlink <- function(url) {
  shortlink_hosts <- c("rstd\\.io", "bit\\.ly", "pos\\.it")
  any(map_lgl(shortlink_hosts, grepl, x = url))
}

expand_github <- function(url) {
  # mostly to handle errors in the spec
  repo_spec <- parse_repo_spec(url)
  glue_data_chr(repo_spec, "github.com/{owner}/{repo}/zipball/HEAD")
}

conspicuous_place <- function() {
  destdir_opt <- getOption("usethis.destdir")
  if (!is.null(destdir_opt)) {
    return(path_tidy(destdir_opt))
  }

  Filter(
    dir_exists,
    c(
      path_home("Desktop"),
      path_home(),
      path_home_r(),
      path_tidy(getwd())
    )
  )[[1]]
}

keep_lgl <- function(
  file,
  ignores = c(
    ".Rproj.user",
    ".rproj.user",
    ".Rhistory",
    ".RData",
    ".git",
    "__MACOSX",
    ".DS_Store"
  )
) {
  ignores <- paste0(
    "((\\/|\\A)",
    gsub("\\.", "[.]", ignores),
    "(\\/|\\Z))",
    collapse = "|"
  )
  !grepl(ignores, file, perl = TRUE)
}

path_before_slash <- function(filepath) {
  f <- function(x) {
    parts <- strsplit(x, "/", fixed = TRUE)[[1]]
    if (length(parts) > 1 || grepl("/", x)) {
      parts[1]
    } else {
      ""
    }
  }
  purrr::map_chr(filepath, f)
}

content_type <- function(h) {
  headers <- curl::parse_headers_list(curl::handle_data(h)$headers)
  headers[["content-type"]]
}

content_disposition <- function(h) {
  headers <- curl::parse_headers_list(curl::handle_data(h)$headers)
  cd <- headers[["content-disposition"]]
  if (is.null(cd)) {
    return()
  }
  parse_content_disposition(cd)
}

check_is_zip <- function(ct) {
  # "https://www.fueleconomy.gov/feg/epadata/16data.zip" comes with
  # MIME type "application/x-zip-compressed"
  # see https://github.com/r-lib/usethis/issues/573
  allowed <- c("application/zip", "application/x-zip-compressed")
  if (!ct %in% allowed) {
    ui_abort(c(
      "Download does not have MIME type {.val application/zip}.",
      "Instead it's {.val {ct}}."
    ))
  }
  invisible(ct)
}

## https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Disposition
## https://tools.ietf.org/html/rfc6266
## DropBox eg: "attachment; filename=\"foo.zip\"; filename*=UTF-8''foo.zip\"
##  GitHub eg: "attachment; filename=foo-main.zip"
# https://stackoverflow.com/questions/30193569/get-content-disposition-parameters
# http://test.greenbytes.de/tech/tc2231/
parse_content_disposition <- function(cd) {
  if (!grepl("^attachment;", cd)) {
    ui_abort(c(
      "{.code Content-Disposition} header doesn't start with {.val attachment}.",
      "Actual header: {.val cd}"
    ))
  }

  cd <- sub("^attachment;\\s*", "", cd, ignore.case = TRUE)
  cd <- strsplit(cd, "\\s*;\\s*")[[1]]
  cd <- strsplit(cd, "=")
  stats::setNames(
    vapply(cd, `[[`, character(1), 2),
    vapply(cd, `[[`, character(1), 1)
  )
}

progress_fun <- function(down, up) {
  total <- down[[1]]
  now <- down[[2]]
  pct <- if (length(total) && total > 0) {
    paste0("(", round(now / total * 100), "%)")
  } else {
    ""
  }
  if (now > 10000) {
    cat("\rDownloaded:", sprintf("%.2f", now / 2^20), "MB ", pct)
  }
  TRUE
}

make_filename <- function(cd, fallback = path_file(file_temp())) {
  ## TO DO(jennybc): the element named 'filename*' is preferred but I'm not
  ## sure how to parse it yet, so targeting 'filename' for now
  ## https://tools.ietf.org/html/rfc6266
  cd <- cd[["filename"]]
  if (is.null(cd) || is.na(cd)) {
    check_name(fallback)
    return(path_sanitize(fallback))
  }

  ## I know I could use regex and lookahead but this is easier for me to
  ## maintain
  cd <- sub("^\"(.+)\"$", "\\1", cd)

  path_sanitize(cd)
}

## https://stackoverflow.com/questions/21322614/use-curl-to-download-a-dropbox-folder-via-shared-link-not-public-link
## lesson: if using cURL, you'd want these options
## -L, --location (follow redirects)
## -O, --remote-name (name local file like the file part of remote name)
## -J, --remote-header-name (tells -O option to consult Content-Disposition
##   instead of the URL)
## https://curl.haxx.se/docs/manpage.html#OPTIONS


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/coverage.R ---
#' Test coverage
#'
#' Adds test coverage reporting to a package, using either Codecov
#' (`https://codecov.io`) or Coveralls (`https://coveralls.io`).
#'
#' @param type Which web service to use.
#' @eval param_repo_spec()
#' @export
use_coverage <- function(type = c("codecov", "coveralls"), repo_spec = NULL) {
  repo_spec <- repo_spec %||% target_repo_spec()

  type <- match.arg(type)
  if (type == "codecov") {
    new <- use_template("codecov.yml", ignore = TRUE)
    if (new) {
      ui_bullets(c(
        "!" = "If test coverage uploads do not succeed, you probably need to
               configure {.env CODECOV_TOKEN} as a repository or organization
               secret:
               {.url https://docs.codecov.com/docs/adding-the-codecov-token}."
      ))
    } else {
      return(invisible(FALSE))
    }
  } else if (type == "coveralls") {
    ui_bullets(c(
      "_" = "Turn on coveralls for this repo at {.url https://coveralls.io/repos/new}."
    ))
  }

  switch(
    type,
    codecov = use_codecov_badge(repo_spec),
    coveralls = use_coveralls_badge(repo_spec)
  )

  ui_bullets(c(
    "_" = "Call {.run [use_github_action(\"test-coverage\")](usethis::use_github_action(\"test-coverage\"))}
           to continuously monitor test coverage."
  ))

  invisible(TRUE)
}

#' @export
#' @rdname use_coverage
#' @param files Character vector of file globs.
use_covr_ignore <- function(files) {
  use_build_ignore(".covrignore")
  write_union(proj_path(".covrignore"), files)
}

use_codecov_badge <- function(repo_spec) {
  url <- glue("https://app.codecov.io/gh/{repo_spec}")
  img <- glue("https://codecov.io/gh/{repo_spec}/graph/badge.svg")
  use_badge("Codecov test coverage", url, img)
}

use_coveralls_badge <- function(repo_spec) {
  default_branch <- git_default_branch()
  url <- glue("https://coveralls.io/r/{repo_spec}?branch={default_branch}")
  img <- glue("https://coveralls.io/repos/github/{repo_spec}/badge.svg")
  use_badge("Coveralls test coverage", url, img)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/cpp11.R ---
#' Use C++ via the cpp11 package
#'
#' Adds infrastructure needed to use the [cpp11](https://cpp11.r-lib.org)
#' package, a header-only R package that helps R package developers handle R
#' objects with C++ code:
#'   * Creates `src/`
#'   * Adds cpp11 to `DESCRIPTION`
#'   * Creates `src/code.cpp`, an initial placeholder `.cpp` file
#'
#' @export
use_cpp11 <- function() {
  check_is_package("use_cpp11()")
  check_installed("cpp11")
  check_uses_roxygen("use_cpp11()")
  check_has_package_doc("use_cpp11()")
  use_src()

  use_dependency("cpp11", "LinkingTo")

  use_template(
    "code-cpp11.cpp",
    path("src", "code.cpp"),
    open = is_interactive()
  )

  check_cpp_register_deps()

  invisible()
}

get_cpp_register_deps <- function() {
  desc <- desc::desc(package = "cpp11")
  desc$get_list("Config/Needs/cpp11/cpp_register")[[1]]
}

check_cpp_register_deps <- function() {
  cpp_register_deps <- get_cpp_register_deps()
  installed <- map_lgl(cpp_register_deps, is_installed)

  if (!all(installed)) {
    ui_bullets(c(
      "_" = "Now install {.pkg {cpp_register_deps[!installed]}} to use {.pkg cpp11}."
    ))
  }
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/cran.R ---
#' CRAN submission comments
#'
#' Creates `cran-comments.md`, a template for your communications with CRAN when
#' submitting a package. The goal is to clearly communicate the steps you have
#' taken to check your package on a wide range of operating systems. If you are
#' submitting an update to a package that is used by other packages, you also
#' need to summarize the results of your [reverse dependency
#' checks][use_revdep].
#'
#' @export
#' @inheritParams use_template
use_cran_comments <- function(open = rlang::is_interactive()) {
  check_is_package("use_cran_comments()")
  use_template(
    "cran-comments.md",
    ignore = TRUE,
    open = open
  )
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/create.R ---
#' Create a package or project
#'
#' @description
#' These functions create an R project:
#'   * `create_package()` creates an R package.
#'   * `create_project()` creates a non-package project, i.e. a data analysis
#'     project.
#'   * `r lifecycle::badge("experimental")` `create_quarto_project()` creates a
#'     Quarto project. It is a simplified convenience wrapper around
#'     [quarto::quarto_create_project()], which you should call directly for
#'     more advanced usage.
#'
#' These functions work best when creating a project *de novo*, but
#' `create_package()` and `create_project()` can be called on an existing
#' project; you will be asked before any existing files are changed.
#'
#' @inheritParams use_description
#' @param fields A named list of fields to add to `DESCRIPTION`, potentially
#'   overriding default values. See [use_description()] for how you can set
#'   personalized defaults using package options.
#' @param path A path. If it exists, it is used. If it does not exist, it is
#'   created, provided that the parent path exists.
#' @param roxygen Do you plan to use roxygen2 to document your package?
#' @param rstudio If `TRUE`, calls [use_rstudio()] to make the new package or
#'   project into an [RStudio
#'   Project](https://r-pkgs.org/workflow101.html#sec-workflow101-rstudio-projects).
#'
#'   If `FALSE`, the goal is to ensure that the directory can be recognized as
#'   a project by, for example, the [here](https://here.r-lib.org) package. If
#'   the project is neither an R package nor a Quarto project, a sentinel
#'   `.here` file is placed to mark the project root.
#' @param open If `TRUE`, [activates][proj_activate()] the new project:
#'
#'   * If using RStudio or Positron, the new project is opened in a new session,
#'     window, or browser tab, depending on the product (RStudio or Positron)
#'     and context (desktop or server).
#'   * Otherwise, the working directory and active project of the current R
#'     session are changed to the new project.
#' @param type The type of Quarto project to create. See
#'   `?quarto::quarto_create_project` for the most up-to-date list, but
#'   `"website"`, `"blog"`, `"book"`, and `"manuscript"` are common choices.
#'
#' @returns Path to the newly created project or package, invisibly.
#' @seealso [create_tidy_package()] is a convenience function that extends
#'   `create_package()` by immediately applying as many of the tidyverse
#'   development conventions as possible.
#' @export
create_package <- function(
  path,
  fields = list(),
  rstudio = rstudioapi::isAvailable(),
  roxygen = TRUE,
  check_name = TRUE,
  open = rlang::is_interactive()
) {
  path <- user_path_prep(path)
  check_path_is_directory(path_dir(path))

  name <- path_file(path_abs(path))
  if (check_name) {
    check_package_name(name)
  }
  challenge_nested_project(path_dir(path), name)
  challenge_home_directory(path)

  create_directory(path)
  local_project(path, force = TRUE)

  use_directory("R")
  proj_desc_create(name, fields, roxygen)
  use_namespace(roxygen = roxygen)

  if (rstudio) {
    use_rstudio()
  }

  if (open) {
    if (proj_activate(proj_get())) {
      # working directory/active project already set; clear the scheduled
      # restoration of the original project
      withr::deferred_clear()
    }
  }

  invisible(proj_get())
}

#' @export
#' @rdname create_package
create_project <- function(
  path,
  rstudio = rstudioapi::isAvailable(),
  open = rlang::is_interactive()
) {
  path <- user_path_prep(path)
  check_path_is_directory(path_dir(path))

  name <- path_file(path_abs(path))
  challenge_nested_project(path_dir(path), name)
  challenge_home_directory(path)

  create_directory(path)
  local_project(path, force = TRUE)

  use_directory("R")

  if (rstudio) {
    use_rstudio()
  } else {
    ui_bullets(c(
      "v" = "Writing a sentinel file {.path .here}.",
      "_" = "Build robust paths within your project via {.fun here::here}.",
      "i" = "Learn more at {.url https://here.r-lib.org}."
    ))
    file_create(proj_path(".here"))
  }

  if (open) {
    if (proj_activate(proj_get())) {
      # working directory/active project already set; clear the scheduled
      # restoration of the original project
      withr::deferred_clear()
    }
  }

  invisible(proj_get())
}

#' @rdname create_package
#' @export
create_quarto_project <- function(
  path,
  type = "default",
  rstudio = rstudioapi::isAvailable(),
  open = rlang::is_interactive()
) {
  check_installed("quarto")

  if (!quarto::quarto_available(error = FALSE)) {
    ui_abort(c(
      "x" = "The Quarto CLI must be available to create a Quarto project.",
      "i" = "See {.url https://quarto.org/docs/get-started/}."
    ))
  }

  path <- user_path_prep(path)
  parent_dir <- path_dir(path)
  check_path_is_directory(parent_dir)

  name <- path_file(path_abs(path))
  challenge_nested_project(parent_dir, name)
  challenge_home_directory(path)

  create_directory(path)
  local_project(path, force = TRUE)

  if (rstudio) {
    use_rstudio()
  }

  res <- quarto::quarto_create_project(
    name = name,
    dir = parent_dir,
    type = type,
    no_prompt = TRUE,
    quiet = getOption("usethis.quiet", default = FALSE)
  )

  if (open) {
    if (proj_activate(proj_get())) {
      # working directory/active project already set; clear the scheduled
      # restoration of the original project
      withr::deferred_clear()
    }
  }

  invisible(proj_get())
}

#' Create a project from a GitHub repo
#'
#' @description
#' Creates a new local project and Git repository from a repo on GitHub, by
#' either cloning or
#' [fork-and-cloning](https://docs.github.com/en/get-started/quickstart/fork-a-repo).
#' In the fork-and-clone case, `create_from_github()` also does additional
#' remote and branch setup, leaving you in the perfect position to make a pull
#' request with [pr_init()], one of several [functions for working with pull
#' requests][pull-requests].
#'
#' `create_from_github()` works best when your GitHub credentials are
#' discoverable. See below for more about authentication.
#'
#' @template double-auth
#'
#' @seealso
#' * [use_github()] to go the opposite direction, i.e. create a GitHub repo
#'   from your local repo
#' * [git_protocol()] for background on `protocol` (HTTPS vs SSH)
#' * [use_course()] to download a snapshot of all files in a GitHub repo,
#'   without the need for any local or remote Git operations
#'
#' @inheritParams create_package
#' @param repo_spec A string identifying the GitHub repo in one of these forms:
#'   * Plain `OWNER/REPO` spec
#'   * Browser URL, such as `"https://github.com/OWNER/REPO"`
#'   * HTTPS Git URL, such as `"https://github.com/OWNER/REPO.git"`
#'   * SSH Git URL, such as `"git@github.com:OWNER/REPO.git"`
#' @param destdir Destination for the new folder, which will be named according
#'   to the `REPO` extracted from `repo_spec`. Defaults to the location stored
#'   in the global option `usethis.destdir`, if defined, or to the user's
#'   Desktop or similarly conspicuous place otherwise.
#' @param fork If `FALSE`, we clone `repo_spec`. If `TRUE`, we fork
#'   `repo_spec`, clone that fork, and do additional setup favorable for
#'   future pull requests:
#'   * The source repo, `repo_spec`, is configured as the `upstream` remote,
#'   using the indicated `protocol`.
#'   * The local `DEFAULT` branch is set to track `upstream/DEFAULT`, where
#'   `DEFAULT` is typically `main` or `master`. It is also immediately pulled,
#'   to cover the case of a pre-existing, out-of-date fork.
#'
#'   If `fork = NA` (the default), we check your permissions on `repo_spec`. If
#'   you can push, we set `fork = FALSE`, If you cannot, we set `fork = TRUE`.
#' @param host GitHub host to target, passed to the `.api_url` argument of
#'   [gh::gh()]. If `repo_spec` is a URL, `host` is extracted from that.
#'
#'   If unspecified, gh defaults to "https://api.github.com", although gh's
#'   default can be customised by setting the GITHUB_API_URL environment
#'   variable.
#'
#'   For a hypothetical GitHub Enterprise instance, either
#'   "https://github.acme.com/api/v3" or "https://github.acme.com" is
#'   acceptable.
#' @param rstudio Initiate an [RStudio
#'   Project](https://r-pkgs.org/workflow101.html#sec-workflow101-rstudio-projects)?
#'   Defaults to `TRUE` if in an RStudio session and project has no
#'   pre-existing `.Rproj` file. Defaults to `FALSE` otherwise (but note that
#'   the cloned repo may already be an RStudio Project, i.e. may already have a
#'   `.Rproj` file).
#' @inheritParams use_github
#'
#' @export
#' @examples
#' \dontrun{
#' create_from_github("r-lib/usethis")
#'
#' # repo_spec can be a URL
#' create_from_github("https://github.com/r-lib/usethis")
#'
#' # a URL repo_spec also specifies the host (e.g. GitHub Enterprise instance)
#' create_from_github("https://github.acme.com/OWNER/REPO")
#' }
create_from_github <- function(
  repo_spec,
  destdir = NULL,
  fork = NA,
  rstudio = NULL,
  open = rlang::is_interactive(),
  protocol = git_protocol(),
  host = NULL
) {
  check_protocol(protocol)

  parsed_repo_spec <- parse_repo_url(repo_spec)
  if (!is.null(parsed_repo_spec$host)) {
    repo_spec <- parsed_repo_spec$repo_spec
    host <- parsed_repo_spec$host
  }

  whoami <- suppressMessages(gh::gh_whoami(.api_url = host))
  no_auth <- is.null(whoami)
  user <- if (no_auth) NULL else whoami$login
  hint <- code_hint_with_host("gh_token_help", host)

  if (no_auth && is.na(fork)) {
    ui_abort(c(
      "x" = "Unable to discover a GitHub personal access token.",
      "x" = "Therefore, can't determine your permissions on {.val {repo_spec}}.",
      "x" = "Therefore, can't decide if {.arg fork} should be {.code TRUE} or {.code FALSE}.",
      "",
      "i" = "You have two choices:",
      "_" = "Make your token available (if in doubt, DO THIS):",
      " " = "Call {.code {hint}} for instructions that should help.",
      "_" = "Call {.fun create_from_github} again, but with {.code fork = FALSE}.",
      " " = "Only do this if you are absolutely sure you don't want to fork.",
      " " = "Note you will NOT be in a position to make a pull request."
    ))
  }

  if (no_auth && isTRUE(fork)) {
    ui_abort(c(
      "x" = "Unable to discover a GitHub personal access token.",
      "i" = "A token is required in order to fork {.val {repo_spec}}.",
      "_" = "Call {.code {hint}} for help configuring a token."
    ))
  }
  # one of these is true:
  # - gh is discovering a token for `host`
  # - gh is NOT discovering a token, but `fork = FALSE`, so that's OK

  source_owner <- spec_owner(repo_spec)
  repo_name <- spec_repo(repo_spec)
  gh <- gh_tr(list(
    repo_owner = source_owner,
    repo_name = repo_name,
    api_url = host
  ))

  repo_info <- gh("GET /repos/{owner}/{repo}")
  # 2023-01-28 We're seeing the GitHub bug again around default branch in a
  # fresh fork. If I create a fork, the POST payload *sometimes* mis-reports the
  # default branch. I.e. it reports `main`, even though the actual default
  # branch is `master`. Therefore we're reverting to consulting the source repo
  # for this info
  default_branch <- repo_info$default_branch

  if (is.na(fork)) {
    fork <- !isTRUE(repo_info$permissions$push)
    fork_status <- glue("fork = {fork}")
    ui_bullets(c("v" = "Setting {.code {fork_status}}."))
  }
  # fork is either TRUE or FALSE

  if (fork && identical(user, repo_info$owner$login)) {
    ui_abort(
      "
      Can't fork, because the authenticated user {.val {user}} already owns the
      source repo {.val {repo_info$full_name}}."
    )
  }

  destdir <- user_path_prep(destdir %||% conspicuous_place())
  check_path_is_directory(destdir)
  challenge_nested_project(destdir, repo_name)
  repo_path <- path(destdir, repo_name)
  create_directory(repo_path)
  check_directory_is_empty(repo_path)

  if (fork) {
    ## https://developer.github.com/v3/repos/forks/#create-a-fork
    ui_bullets(c("v" = "Forking {.val {repo_info$full_name}}."))
    upstream_url <- switch(
      protocol,
      https = repo_info$clone_url,
      ssh = repo_info$ssh_url
    )
    repo_info <- gh("POST /repos/{owner}/{repo}/forks")
    ui_bullets(c("i" = "Waiting for the fork to finalize before cloning..."))
    Sys.sleep(3)
  }

  origin_url <- switch(
    protocol,
    https = repo_info$clone_url,
    ssh = repo_info$ssh_url
  )

  ui_bullets(c(
    "v" = "Cloning repo from {.val {origin_url}} into {.path {repo_path}}."
  ))
  gert::git_clone(origin_url, repo_path, verbose = FALSE)

  proj_path <- find_rstudio_root(repo_path)
  local_project(proj_path, force = TRUE) # schedule restoration of project

  # 2023-01-28 again, it would be more natural to trust the default branch of
  # the fork, but that cannot always be trusted. For now, we're still using
  # the default branch learned from the source repo.
  ui_bullets(c("i" = "Default branch is {.val {default_branch}}."))

  if (fork) {
    ui_bullets(c(
      "v" = "Adding {.val upstream} remote: {.val {upstream_url}}"
    ))
    use_git_remote("upstream", upstream_url)
    pr_merge_main()
    upstream_remref <- glue("upstream/{default_branch}")
    ui_bullets(c(
      "v" = "Setting remote tracking branch for local {.val {default_branch}}
             branch to {.val {upstream_remref}}."
    ))
    gert::git_branch_set_upstream(upstream_remref, repo = git_repo())
    config_key <- glue("remote.upstream.created-by")
    gert::git_config_set(
      config_key,
      "usethis::create_from_github",
      repo = git_repo()
    )
  }

  rstudio <- rstudio %||% rstudio_available()
  rstudio <- rstudio && !is_rstudio_project()
  if (rstudio) {
    use_rstudio(reformat = FALSE)
  }

  if (open) {
    if (proj_activate(proj_get())) {
      # Working directory/active project changed; so don't undo on exit
      withr::deferred_clear()
    }
  }

  invisible(proj_get())
}

# If there's a single directory containing an .Rproj file, use it.
# Otherwise work in the repo root
find_rstudio_root <- function(path) {
  rproj <- rproj_paths(path, recurse = TRUE)
  if (length(rproj) == 1) {
    path_dir(rproj)
  } else {
    path
  }
}

challenge_nested_project <- function(path, name) {
  enclosing_project <- proj_find(path)
  if (is.null(enclosing_project)) {
    return(invisible())
  }

  # creates an undocumented backdoor we can exploit when the interactive
  # approval is impractical, e.g. in tests
  if (isTRUE(getOption("usethis.allow_nested_project", FALSE))) {
    return(invisible())
  }

  ui_bullets(c(
    "!" = "New project {.path {path(path, name)}} would be nested inside an
           existing project {.path {pth(enclosing_project)}}, which is rarely a
           good idea.",
    "i" = "If this is unexpected, the {.pkg here} package has a function,
           {.fun here::dr_here} that reveals why a particular path is regarded
           as a project. To learn more, run {.fun here::dr_here} in a fresh R
           session that has {.path {pth(enclosing_project)}} as working
           directory."
  ))
  if (ui_nah("Do you want to create anyway?")) {
    ui_abort("Cancelling project creation.")
  }
  invisible()
}

challenge_home_directory <- function(path) {
  homes <- unique(c(path_home(), path_home_r()))
  if (!path %in% homes) {
    return(invisible())
  }

  qualification <- if (is_windows()) {
    glue("a special directory, i.e. some applications regard it as ")
  } else {
    ""
  }
  ui_bullets(c(
    "!" = "{.path {pth(path)}} is {qualification}your home directory.",
    "i" = "It is generally a bad idea to create a new project here.",
    "i" = "You should probably create your new project in a subdirectory."
  ))
  if (ui_nah("Do you want to create anyway?")) {
    ui_abort("Good move! Cancelling project creation.")
  }
  invisible()
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/data-table.R ---
#' Prepare for importing data.table
#'
#' `use_data_table()` imports the `data.table()` function from the data.table
#' package, as well as several important symbols: `:=`, `.SD`, `.BY`, `.N`,
#' `.I`, `.GRP`, `.NGRP`, `.EACHI`. This is a minimal setup and you can learn
#' much more in the "Importing data.table" vignette:
#' `https://rdatatable.gitlab.io/data.table/articles/datatable-importing.html`.
#' In addition to importing these functions, `use_data_table()` also blocks the
#' usage of data.table in the `Depends` field of the `DESCRIPTION` file;
#' `data.table` should be used as an _imported_ or _suggested_ package only. See
#' this [discussion](https://github.com/Rdatatable/data.table/issues/3076).
#'
#' @export
use_data_table <- function() {
  check_is_package("use_data_table()")
  check_installed("data.table")
  check_uses_roxygen("use_data_table()")

  desc <- proj_desc()
  deps <- desc$get_deps()

  if (any(deps$type == "Depends" & deps$package == "data.table")) {
    ui_bullets(c(
      "!" = "{.pkg data.table} should be in {.field Imports} or
             {.field Suggests}, not {.field Depends}!",
      "v" = "Removing {.pkg data.table} from {.field Depends}."
    ))
    desc$del_dep("data.table", "Depends")
    desc$write()
  }

  use_import_from(
    "data.table",
    c("data.table", ":=", ".SD", ".BY", ".N", ".I", ".GRP", ".NGRP", ".EACHI")
  )
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/data.R ---
#' Create package data
#'
#' `use_data()` makes it easy to save package data in the correct format. I
#' recommend you save scripts that generate package data in `data-raw`: use
#' `use_data_raw()` to set it up. You also need to document exported datasets.
#'
#' @param ... Unquoted names of existing objects to save.
#' @param internal If `FALSE`, saves each object in its own `.rda`
#'   file in the `data/` directory. These data files bypass the usual
#'   export mechanism and are available whenever the package is loaded
#'   (or via [data()] if `LazyData` is not true).
#'
#'   If `TRUE`, stores all objects in a single `R/sysdata.rda` file.
#'   Objects in this file follow the usual export rules. Note that this means
#'   they will be exported if you are using the common `exportPattern()`
#'   rule which exports all objects except for those that start with `.`.
#' @param overwrite By default, `use_data()` will not overwrite existing
#'   files. If you really want to do so, set this to `TRUE`.
#' @param compress Choose the type of compression used by [save()].
#'   Should be one of "gzip", "bzip2", or "xz".
#' @param version The serialization format version to use. The default, 3, can
#'   only be read by R versions 3.5.0 and higher. For R 1.4.0 to 3.5.3, use
#'   version 2.
#' @inheritParams base::save
#'
#' @seealso The [data chapter](https://r-pkgs.org/data.html) of [R
#'   Packages](https://r-pkgs.org).
#' @export
#' @examples
#' \dontrun{
#' x <- 1:10
#' y <- 1:100
#'
#' use_data(x, y) # For external use
#' use_data(x, y, internal = TRUE) # For internal use
#' }
use_data <- function(
  ...,
  internal = FALSE,
  overwrite = FALSE,
  compress = "bzip2",
  version = 3,
  ascii = FALSE
) {
  check_is_package("use_data()")

  objs <- get_objs_from_dots(dots(...))

  original_minimum_r_version <- pkg_minimum_r_version()
  serialization_minimum_r_version <- if (version < 3) "2.10" else "3.5"
  if (
    is.na(original_minimum_r_version) ||
      original_minimum_r_version < serialization_minimum_r_version
  ) {
    use_dependency("R", "depends", serialization_minimum_r_version)
  }

  if (internal) {
    use_directory("R")
    paths <- path("R", "sysdata.rda")
    objs <- list(objs)
  } else {
    use_directory("data")
    paths <- path("data", objs, ext = "rda")
    desc <- proj_desc()

    if (!desc$has_fields("LazyData")) {
      ui_bullets(c(
        "v" = "Setting {.field LazyData} to {.val true} in {.path DESCRIPTION}."
      ))
      desc$set(LazyData = "true")
      desc$write()
    }
  }
  check_files_absent(proj_path(paths), overwrite = overwrite)

  ui_bullets(c(
    "v" = "Saving {.val {unlist(objs)}} to {.val {paths}}."
  ))
  if (!internal) {
    ui_bullets(c(
      "_" = "Document your data (see {.url https://r-pkgs.org/data.html})."
    ))
  }

  envir <- parent.frame()
  mapply(
    save,
    list = objs,
    file = proj_path(paths),
    MoreArgs = list(
      envir = envir,
      compress = compress,
      version = version,
      ascii = ascii
    )
  )

  invisible()
}

get_objs_from_dots <- function(.dots) {
  if (length(.dots) == 0L) {
    ui_abort("Nothing to save.")
  }

  is_name <- vapply(.dots, is.symbol, logical(1))
  if (!all(is_name)) {
    ui_abort("Can only save existing named objects.")
  }

  objs <- vapply(.dots, as.character, character(1))
  duplicated_objs <- which(stats::setNames(duplicated(objs), objs))
  if (length(duplicated_objs) > 0L) {
    objs <- unique(objs)
    ui_bullets(c(
      "!" = "Saving duplicates only once: {.val {names(duplicated_objs)}}."
    ))
  }
  objs
}

check_files_absent <- function(paths, overwrite) {
  if (overwrite) {
    return()
  }

  ok <- !file_exists(paths)

  if (all(ok)) {
    return()
  }

  ui_abort(c(
    "{.path {pth(paths[!ok])}} already exist.",
    "Use {.code overwrite = TRUE} to overwrite."
  ))
}

#' @param name Name of the dataset to be prepared for inclusion in the package.
#' @inheritParams use_template
#' @rdname use_data
#' @export
#' @examples
#' \dontrun{
#' use_data_raw("daisy")
#' }
use_data_raw <- function(name = "DATASET", open = rlang::is_interactive()) {
  check_name(name)
  r_path <- path("data-raw", asciify(name), ext = "R")
  use_directory("data-raw", ignore = TRUE)

  use_template(
    "packagename-data-prep.R",
    save_as = r_path,
    data = list(name = name),
    ignore = FALSE,
    open = open
  )

  ui_bullets(c(
    "_" = "Finish writing the data preparation script in {.path {pth(r_path)}}.",
    "_" = "Use {.fun usethis::use_data} to add prepared data to package."
  ))
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/description.R ---
#' Create or modify a DESCRIPTION file
#'
#' @description
#'

#' `use_description()` creates a `DESCRIPTION` file. Although mostly associated
#' with R packages, a `DESCRIPTION` file can also be used to declare
#' dependencies for a non-package project. Within such a project,
#' `devtools::install_deps()` can then be used to install all the required
#' packages. Note that, by default, `use_decription()` checks for a
#' CRAN-compliant package name. You can turn this off with `check_name = FALSE`.
#'

#' usethis consults the following sources, in this order, to set `DESCRIPTION`
#' fields:
#' * `fields` argument of [create_package()] or `use_description()`
#' * `getOption("usethis.description")`
#' * Defaults built into usethis
#'
#' The fields discovered via options or the usethis package can be viewed with
#' `use_description_defaults()`.
#'
#' If you create a lot of packages, consider storing personalized defaults as a
#' named list in an option named `"usethis.description"`. Here's an example of
#' code to include in `.Rprofile`, which can be opened via [edit_r_profile()]:
#'
#' ```
#' options(
#'   usethis.description = list(
#'     "Authors@R" = utils::person(
#'       "Jane", "Doe",
#'       email = "jane@example.com",
#'       role = c("aut", "cre"),
#'       comment = c(ORCID = "YOUR-ORCID-ID")
#'     ),
#'     Language =  "es",
#'     License = "MIT + file LICENSE"
#'   )
#' )
#' ```
#'
#' Prior to usethis v2.0.0, `getOption("devtools.desc")` was consulted for
#' backwards compatibility, but now only the `"usethis.description"` option is
#' supported.
#'
#' @param fields A named list of fields to add to `DESCRIPTION`, potentially
#'   overriding default values. Default values are taken from the
#'   `"usethis.description"` option or the usethis package (in that order), and
#'   can be viewed with `use_description_defaults()`.
#' @param check_name Whether to check if the name is valid for CRAN and throw an
#'   error if not.
#' @param roxygen If `TRUE`, sets `RoxygenNote` to current roxygen2 version
#' @seealso The [description chapter](https://r-pkgs.org/description.html)
#'   of [R Packages](https://r-pkgs.org)
#' @export
#' @examples
#' \dontrun{
#' use_description()
#'
#' use_description(fields = list(Language = "es"))
#'
#' use_description_defaults()
#' }
use_description <- function(
  fields = list(),
  check_name = TRUE,
  roxygen = TRUE
) {
  name <- project_name()
  if (check_name) {
    check_package_name(name)
  }

  proj_desc_create(name = name, fields = fields, roxygen = roxygen)
}

#' @rdname use_description
#' @param package Package name
#' @export
use_description_defaults <- function(
  package = NULL,
  roxygen = TRUE,
  fields = list()
) {
  fields <- fields %||% list()
  check_is_named_list(fields)

  usethis <- usethis_description_defaults(package)

  if (roxygen) {
    if (is_installed("roxygen2")) {
      roxygen_note <- utils::packageVersion("roxygen2")
    } else {
      roxygen_note <- "7.0.0" # version doesn't really matter
    }
    usethis$Roxygen <- "list(markdown = TRUE)"
    usethis$RoxygenNote <- roxygen_note
  }

  options <- getOption("usethis.description") %||% list()

  # A `person` object in Authors@R is not patched in by modifyList()
  modify_this <- function(orig, patch) {
    out <- utils::modifyList(orig, patch)
    if (inherits(patch$`Authors@R`, "person")) {
      #if (has_name(patch, "Authors@R")) {
      out$`Authors@R` <- patch$`Authors@R`
    }
    out
  }

  defaults <- modify_this(usethis, options)
  defaults <- modify_this(defaults, fields)

  # Ensure each element is a single string
  if (inherits(defaults$`Authors@R`, "person")) {
    defaults$`Authors@R` <- format(defaults$`Authors@R`, style = "R")
    defaults$`Authors@R` <- paste0(defaults$`Authors@R`, collapse = "\n")
  }
  defaults <- lapply(defaults, paste, collapse = "")

  compact(defaults)
}

usethis_description_defaults <- function(package = NULL) {
  list(
    Package = package %||% "valid.package.name.goes.here",
    Version = "0.0.0.9000",
    Title = "What the Package Does (One Line, Title Case)",
    Description = "What the package does (one paragraph).",
    "Authors@R" = 'person("First", "Last", email = "first.last@example.com", role = c("aut", "cre"))',
    License = "`use_mit_license()`, `use_gpl3_license()` or friends to pick a license",
    Encoding = "UTF-8"
  )
}

check_package_name <- function(name) {
  if (!valid_package_name(name)) {
    ui_abort(c(
      "x" = "{.val {name}} is not a valid package name. To be allowed on CRAN, it should:",
      "*" = "Contain only ASCII letters, numbers, and '.'.",
      "*" = "Have at least two characters.",
      "*" = "Start with a letter.",
      "*" = "Not end with '.'."
    ))
  }
}

valid_package_name <- function(x) {
  grepl("^[a-zA-Z][a-zA-Z0-9.]+$", x) && !grepl("\\.$", x)
}

tidy_desc <- function(desc) {
  desc$set("Encoding" = "UTF-8")

  # Normalize all fields (includes reordering)
  # Wrap in a try() so it always succeeds, even if user options are malformed
  try(desc$normalize(), silent = TRUE)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/directory.R ---
#' Use a directory
#'
#' `use_directory()` creates a directory (if it does not already exist) in the
#' project's top-level directory. This function powers many of the other `use_`
#' functions such as [use_data()] and [use_vignette()].
#'
#' @param path Path of the directory to create, relative to the project.
#' @inheritParams use_template
#'
#' @export
#' @examples
#' \dontrun{
#' use_directory("inst")
#' }
use_directory <- function(path, ignore = FALSE) {
  create_directory(proj_path(path))
  if (ignore) {
    use_build_ignore(path)
  }

  invisible(TRUE)
}

create_directory <- function(path) {
  if (dir_exists(path)) {
    return(invisible(FALSE))
  } else if (file_exists(path)) {
    ui_abort("{.path {pth(path)}} exists but is not a directory.")
  }

  dir_create(path, recurse = TRUE)
  ui_bullets(c("v" = "Creating {.path {pth(path)}}."))
  invisible(TRUE)
}

check_path_is_directory <- function(path) {
  if (!file_exists(path)) {
    ui_abort("Directory {.path {pth(path)}} does not exist.")
  }

  if (!is_dir(path)) {
    ui_abort("{.path {pth(path)}} is not a directory.")
  }
}

count_directory_files <- function(x) {
  length(dir_ls(x))
}

directory_has_files <- function(x) {
  count_directory_files(x) >= 1
}

check_directory_is_empty <- function(x) {
  if (directory_has_files(x)) {
    ui_abort("{.path {pth(x)}} exists and is not an empty directory.")
  }
  invisible(x)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/documentation.R ---
#' Package-level documentation
#'
#' Adds a dummy `.R` file that will cause roxygen2 to generate basic
#' package-level documentation. If your package is named "foo", this will make
#' help available to the user via `?foo` or `package?foo`. Once you call
#' `devtools::document()`, roxygen2 will flesh out the `.Rd` file using data
#' from the `DESCRIPTION`. That ensures you don't need to repeat (and remember
#' to update!) the same information in multiple places. This `.R` file is also a
#' good place for roxygen directives that apply to the whole package (vs. a
#' specific function), such as global namespace tags like `@importFrom`.
#'
#' @seealso The [documentation chapter](https://r-pkgs.org/man.html) of [R
#'   Packages](https://r-pkgs.org)
#' @inheritParams use_template
#' @export
use_package_doc <- function(open = rlang::is_interactive()) {
  check_is_package("use_package_doc()")
  use_directory("R")
  use_template(
    "packagename-package.R",
    package_doc_path(),
    open = open
  )
  ui_bullets(c(
    "_" = "Run {.run devtools::document()} to update package-level documentation."
  ))
}

package_doc_path <- function() {
  path("R", paste0(project_name(), "-package"), ext = "R")
}

has_package_doc <- function() {
  file_exists(proj_path(package_doc_path()))
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/edit.R ---
#' Open file for editing
#'
#' Opens a file for editing in RStudio, if that is the active environment, or
#' via [utils::file.edit()] otherwise. If the file does not exist, it is
#' created. If the parent directory does not exist, it is also created.
#' `edit_template()` specifically opens templates in `inst/templates` for use
#' with [use_template()].
#'
#' @param path Path to target file.
#' @param open Whether to open the file for interactive editing.
#' @return Target path, invisibly.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' edit_file("DESCRIPTION")
#' edit_file("~/.gitconfig")
#' }
edit_file <- function(path, open = rlang::is_interactive()) {
  open <- open && is_interactive()
  path <- user_path_prep(path)
  create_directory(path_dir(path))
  file_create(path)

  if (!open) {
    ui_bullets(c("_" = "Edit {.path {pth(path)}}."))
    return(invisible(path))
  }

  ui_bullets(c("_" = "Modify {.path {pth(path)}}."))
  if (rstudio_available() && rstudioapi::hasFun("navigateToFile")) {
    rstudioapi::navigateToFile(path)
  } else {
    utils::file.edit(path)
  }
  invisible(path)
}

#' @param template The target template file. If not specified, existing template
#'  files are offered for interactive selection.
#' @export
#' @rdname edit_file
edit_template <- function(template = NULL, open = rlang::is_interactive()) {
  check_is_package("edit_template()")

  if (is.null(template)) {
    ui_bullets(c(
      "!" = "No template specified ... checking {.path {pth('inst/templates')}}."
    ))
    template <- choose_template()
  }

  if (is_empty(template)) {
    return(invisible())
  }

  path <- proj_path("inst", "templates", template)
  edit_file(path, open)
}

choose_template <- function() {
  if (!is_interactive()) {
    return(character())
  }
  templates <- path_file(dir_ls(proj_path("inst", "templates"), type = "file"))
  if (is_empty(templates)) {
    return(character())
  }

  choice <- utils::menu(
    choices = templates,
    title = "Which template do you want to edit? (0 to exit)"
  )

  templates[choice]
}

#' Open configuration files
#'
#' * `edit_r_profile()` opens `.Rprofile`
#' * `edit_r_environ()` opens `.Renviron`
#' * `edit_r_makevars()` opens `.R/Makevars`
#' * `edit_git_config()` opens `.gitconfig` or `.git/config`
#' * `edit_git_ignore()` opens global (user-level) gitignore file and ensures
#'   its path is declared in your global Git config.
#' * `edit_pkgdown_config` opens the pkgdown YAML configuration file for the
#'   current Project.
#' * `edit_rstudio_snippets()` opens RStudio's snippet config for the given type.
#' * `edit_rstudio_prefs()` opens [RStudio's preference file][use_rstudio_preferences()].
#'
#' The `edit_r_*()` functions consult R's notion of user's home directory.
#' The `edit_git_*()` functions (and \pkg{usethis} in general) inherit home
#' directory behaviour from the \pkg{fs} package, which differs from R itself
#' on Windows. The \pkg{fs} default is more conventional in terms of the
#' location of user-level Git config files. See [fs::path_home()] for more
#' details.
#'
#' Files created by `edit_rstudio_snippets()` will *mask*, not supplement,
#' the built-in default snippets. If you like the built-in snippets, copy them
#' and include with your custom snippets.
#'
#' @return Path to the file, invisibly.
#'
#' @param scope Edit globally for the current __user__, or locally for the
#'   current __project__
#' @name edit
NULL

#' @export
#' @rdname edit
edit_r_profile <- function(scope = c("user", "project")) {
  path <- scoped_path_r(scope, ".Rprofile", envvar = "R_PROFILE_USER")
  edit_file(path)
  ui_bullets(c("_" = "Restart R for changes to take effect."))
  invisible(path)
}

#' @export
#' @rdname edit
edit_r_environ <- function(scope = c("user", "project")) {
  path <- scoped_path_r(scope, ".Renviron", envvar = "R_ENVIRON_USER")
  edit_file(path)
  ui_bullets(c("_" = "Restart R for changes to take effect."))
  invisible(path)
}

#' @export
#' @rdname edit
edit_r_buildignore <- function() {
  check_is_package("edit_r_buildignore()")
  edit_file(proj_path(".Rbuildignore"))
}

#' @export
#' @rdname edit
edit_r_makevars <- function(scope = c("user", "project")) {
  path <- scoped_path_r(scope, ".R", "Makevars")
  edit_file(path)
}

#' @export
#' @rdname edit
#' @param type Snippet type (case insensitive text).
edit_rstudio_snippets <- function(
  type = c(
    "r",
    "markdown",
    "c_cpp",
    "css",
    "html",
    "java",
    "javascript",
    "python",
    "sql",
    "stan",
    "tex",
    "yaml"
  )
) {
  type <- tolower(type)
  type <- match.arg(type)
  file <- path_ext_set(type, "snippets")

  # Snippet location changed in 1.3:
  # https://blog.rstudio.com/2020/02/18/rstudio-1-3-preview-configuration/
  new_rstudio <- !rstudioapi::isAvailable() ||
    rstudioapi::getVersion() >= "1.3.0"
  old_path <- path_home_r(".R", "snippets", file)
  new_path <- rstudio_config_path("snippets", file)

  # Mimic RStudio behaviour: copy to new location if you edit
  if (new_rstudio && file_exists(old_path) && !file_exists(new_path)) {
    create_directory(path_dir(new_path))
    file_copy(old_path, new_path)
    ui_bullets(c(
      "v" = "Copying snippets file to {.path {pth(new_path)}}."
    ))
  }

  path <- if (new_rstudio) new_path else old_path
  if (!file_exists(path)) {
    ui_bullets(c(
      "v" = "New snippet file at {.path {pth(path)}}.",
      "i" = "This masks the default snippets for {.field {type}}.",
      "i" = "Delete this file and restart RStudio to restore the default snippets."
    ))
  }
  edit_file(path)
}

#' @export
#' @rdname edit
edit_rstudio_prefs <- function() {
  path <- rstudio_config_path("rstudio-prefs.json")

  edit_file(path)
  ui_bullets(c("_" = "Restart RStudio for changes to take effect."))
  invisible(path)
}

scoped_path_r <- function(scope = c("user", "project"), ..., envvar = NULL) {
  scope <- match.arg(scope)

  # Try environment variable in user scopes
  if (scope == "user" && !is.null(envvar)) {
    env <- Sys.getenv(envvar, unset = "")
    if (!identical(env, "")) {
      return(user_path_prep(env))
    }
  }

  root <- switch(scope, user = path_home_r(), project = proj_get())
  path(root, ...)
}

# git paths ---------------------------------------------------------------
# Note that on windows R's definition of ~ is in a nonstandard place,
# so it is important to use path_home(), not path_home_r()

#' @export
#' @rdname edit
edit_git_config <- function(scope = c("user", "project")) {
  scope <- match.arg(scope)
  path <- switch(
    scope,
    user = path_home(".gitconfig"),
    project = proj_path(".git", "config")
  )
  invisible(edit_file(path))
}

#' @export
#' @rdname edit
edit_git_ignore <- function(scope = c("user", "project")) {
  scope <- match.arg(scope)
  if (scope == "user") {
    ensure_core_excludesFile()
  }
  file <- git_ignore_path(scope)

  if (scope == "user" && !file_exists(file)) {
    git_vaccinate()
  }

  invisible(edit_file(file))
}

git_ignore_path <- function(scope = c("user", "project")) {
  scope <- match.arg(scope)
  switch(
    scope,
    user = git_cfg_get("core.excludesFile", where = "global"),
    project = proj_path(".gitignore")
  )
}

# pkgdown ---------------------------------------------------------------
#' @export
#' @rdname edit
edit_pkgdown_config <- function() {
  path <- pkgdown_config_path()
  if (is.null(path)) {
    ui_bullets(c("x" = "No pkgdown config file found in current Project."))
  } else {
    invisible(edit_file(path))
  }
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/git-default-branch.R ---
#' Get or set the default Git branch
#'
#' @description

#' The `git_default_branch*()` functions put some structure around the somewhat
#' fuzzy (but definitely real) concept of the default branch. In particular,
#' they support new conventions around the Git default branch name, globally or
#' in a specific project / Git repository.
#'
#' @section Background on the default branch:
#'

#' Technically, Git has no official concept of the default branch. But in
#' reality, almost all Git repos have an *effective default branch*. If there's
#' only one branch, this is it! It is the branch that most bug fixes and
#' features get merged in to. It is the branch you see when you first visit a
#' repo on a site such as GitHub. On a Git remote, it is the branch that `HEAD`
#' points to.
#'
#' Historically, `master` has been the most common name for the default branch,
#' but `main` is an increasingly popular choice.
#'

#' @section `git_default_branch_configure()`:

#' This configures `init.defaultBranch` at the global (a.k.a user) level. This
#' setting determines the name of the branch that gets created when you make the
#' first commit in a new Git repo. `init.defaultBranch` only affects the local
#' Git repos you create in the future.
#'

#' @section `git_default_branch()`:

#' This figures out the default branch of the current Git repo, integrating
#' information from the local repo and, if applicable, the `upstream` or
#' `origin` remote. If there is a local vs. remote mismatch,
#' `git_default_branch()` throws an error with advice to call
#' `git_default_branch_rediscover()` to repair the situation.
#'

#' For a remote repo, the default branch is the branch that `HEAD` points to.
#'

#' For the local repo, if there is only one branch, that must be the default!
#' Otherwise we try to identify the relevant local branch by looking for
#' specific branch names, in this order:
#' * whatever the default branch of `upstream` or `origin` is, if applicable
#' * `main`
#' * `master`
#' * the value of the Git option `init.defaultBranch`, with the usual deal where
#'   a local value, if present, takes precedence over a global (a.k.a.
#'   user-level) value
#'

#' @section `git_default_branch_rediscover()`:

#' This consults an external authority -- specifically, the remote **source
#' repo** on GitHub -- to learn the default branch of the current project /
#' repo. If that doesn't match the apparent local default branch (for example,
#' the project switched from `master` to `main`), we do the corresponding branch
#' renaming in your local repo and, if relevant, in your fork.

#'
#' See <https://happygitwithr.com/common-remote-setups.html> for more about
#' GitHub remote configurations and, e.g., what we mean by the source repo. This
#' function works for the configurations `"ours"`, `"fork"`, and `"theirs"`.

#' @section `git_default_branch_rename()`:

#' Note: this only works for a repo that you effectively own. In terms of
#' GitHub, you must own the **source repo** personally or, if
#' organization-owned, you must have `admin` permission on the **source repo**.
#'
#' This renames the default branch in the **source repo** on GitHub and then
#' calls `git_default_branch_rediscover()`, to make any necessary changes in the
#' local repo and, if relevant, in your personal fork.
#'
#' See <https://happygitwithr.com/common-remote-setups.html> for more about
#' GitHub remote configurations and, e.g., what we mean by the source repo. This
#' function works for the configurations `"ours"`, `"fork"`, and `"no_github"`.
#'
#' Regarding `"no_github"`: Of course, this function does what you expect for a
#' local repo with no GitHub remotes, but that is not the primary use case.

#' @return Name of the default branch.
#' @name git-default-branch
NULL

#' @export
#' @rdname git-default-branch
#' @examples
#' \dontrun{
#' git_default_branch()
#' }
git_default_branch <- function() {
  git_default_branch_(github_remote_config())
}

# If config is available, we can use it to avoid an additional lookup
# on the GitHub API
git_default_branch_ <- function(cfg) {
  repo <- git_repo()

  upstream <- git_default_branch_remote(cfg, "upstream")
  if (is.na(upstream$default_branch)) {
    origin <- git_default_branch_remote(cfg, "origin")
    if (is.na(origin$default_branch)) {
      db_source <- list()
    } else {
      db_source <- origin
    }
  } else {
    db_source <- upstream
  }

  db_local_with_source <- tryCatch(
    guess_local_default_branch(db_source$default_branch),
    error = function(e) NA_character_
  )

  # these error sub-classes and error data are for the benefit of git_sitrep()

  if (is.na(db_local_with_source)) {
    if (length(db_source)) {
      ui_abort(
        c(
          "x" = "Default branch mismatch between local repo and remote.",
          "i" = "The default branch of the {.val {db_source$name}} remote is
               {.val {db_source$default_branch}}.",
          " " = "But the local repo has no branch named
               {.val {db_source$default_branch}}.",
          "_" = "Call {.run [git_default_branch_rediscover()](usethis::git_default_branch_rediscover())} to resolve this."
        ),
        class = "error_default_branch",
        db_source = db_source
      )
    } else {
      ui_abort(
        "Can't determine the local repo's default branch.",
        class = "error_default_branch"
      )
    }
  }
  # we learned a default branch from the local repo

  if (
    is.null(db_source$default_branch) ||
      is.na(db_source$default_branch) ||
      identical(db_local_with_source, db_source$default_branch)
  ) {
    return(db_local_with_source)
  }
  # we learned a default branch from the source repo and it doesn't match
  # the local default branch

  ui_abort(
    c(
      "x" = "Default branch mismatch between local repo and remote.",
      "i" = "The default branch of the {.val {db_source$name}} remote is
           {.val {db_source$default_branch}}.",
      " " = "But the default branch of the local repo appears to be
           {.val {db_local_with_source}}.",
      "_" = "Call {.run [git_default_branch_rediscover()](usethis::git_default_branch_rediscover())} to resolve this."
    ),
    class = "error_default_branch",
    db_source = db_source,
    db_local = db_local_with_source
  )
}

# returns a whole data structure, because the caller needs the surrounding
# context to produce a helpful error message
git_default_branch_remote <- function(cfg, remote = "origin") {
  repo <- git_repo()
  out <- list(
    name = remote,
    is_configured = NA,
    url = NA_character_,
    repo_spec = NA_character_,
    default_branch = NA_character_
  )

  cfg_remote <- cfg[[remote]]
  if (!cfg_remote$is_configured) {
    out$is_configured <- FALSE
    return(out)
  }

  out$is_configured <- TRUE
  out$url <- cfg_remote$url

  if (!is.na(cfg_remote$default_branch)) {
    out$repo_spec <- cfg_remote$repo_spec
    out$default_branch <- cfg_remote$default_branch
    return(out)
  }

  # Fall back to pure git based approach
  out$default_branch <- tryCatch(
    {
      gert::git_fetch(remote = remote, repo = repo, verbose = FALSE)
      res <- gert::git_remote_ls(remote = remote, verbose = FALSE, repo = repo)
      path_file(res$symref[res$ref == "HEAD"])
    },
    error = function(e) NA_character_
  )

  out
}

default_branch_candidates <- function() {
  c(
    "main",
    "master",
    # we use `where = "de_facto"` so that one can configure init.defaultBranch
    # *locally* (which is unusual, but possible) in a repo that uses an
    # unconventional default branch name
    git_cfg_get("init.defaultBranch", where = "de_facto")
  )
}

# `prefer` is available if you want to inject external information, such as
# the default branch of a remote
guess_local_default_branch <- function(prefer = NULL, verbose = FALSE) {
  repo <- git_repo()

  gb <- gert::git_branch_list(local = TRUE, repo = repo)[["name"]]
  if (length(gb) == 0) {
    ui_abort(c(
      "x" = "Can't find any local branches.",
      " " = "Do you need to make your first commit?"
    ))
  }

  candidates <- c(prefer, default_branch_candidates())
  first_matched <- function(x, table) table[min(match(x, table), na.rm = TRUE)]

  if (length(gb) == 1) {
    db <- gb
  } else if (any(gb %in% candidates)) {
    db <- first_matched(gb, candidates)
  } else {
    # TODO: perhaps this should be classed, so I can catch it and distinguish
    # from the ui_abort() above, where there are no local branches.
    ui_abort(
      "
      Unable to guess which existing local branch plays the role of the default."
    )
  }

  if (verbose) {
    ui_bullets(c(
      "i" = "Local branch {.val {db}} appears to play the role of the default
             branch."
    ))
  }
  db
}

#' @export
#' @rdname git-default-branch
#' @param name Default name for the initial branch in new Git repositories.
#' @examples
#' \dontrun{
#' git_default_branch_configure()
#' }
git_default_branch_configure <- function(name = "main") {
  check_string(name)
  ui_bullets(c(
    "v" = "Configuring {.field init.defaultBranch} as {.val {name}}.",
    "i" = "Remember: this only affects repos you create in the future!"
  ))
  use_git_config(scope = "user", `init.defaultBranch` = name)
  invisible(name)
}

#' @export
#' @rdname git-default-branch
#' @param current_local_default Name of the local branch that is currently
#'   functioning as the default branch. If unspecified, this can often be
#'   inferred.
#' @examples
#' \dontrun{
#' git_default_branch_rediscover()
#'
#' # you can always explicitly specify the local branch that's been playing the
#' # role of the default
#' git_default_branch_rediscover("unconventional_default_branch_name")
#' }
git_default_branch_rediscover <- function(current_local_default = NULL) {
  rediscover_default_branch(old_name = current_local_default)
}

#' @export
#' @rdname git-default-branch
#' @param from Name of the branch that is currently functioning as the default
#'   branch.
#' @param to New name for the default branch.
#' @examples
#' \dontrun{
#' git_default_branch_rename()
#'
#' # you can always explicitly specify one or both branch names
#' git_default_branch_rename(from = "this", to = "that")
#' }
git_default_branch_rename <- function(from = NULL, to = "main") {
  repo <- git_repo()
  maybe_name(from)
  check_name(to)

  if (
    !is.null(from) &&
      !gert::git_branch_exists(from, local = TRUE, repo = repo)
  ) {
    ui_abort("Can't find existing branch named {.val {from}}.")
  }

  cfg <- github_remote_config(github_get = TRUE)
  check_for_config(cfg, ok_configs = c("ours", "fork", "no_github"))

  if (cfg$type == "no_github") {
    from <- from %||% guess_local_default_branch(verbose = TRUE)
    if (from == to) {
      ui_bullets(c(
        "i" = "Local repo already has {.val {from}} as its default branch."
      ))
    } else {
      ui_bullets(c(
        "v" = "Moving local {.val {from}} branch to {.val {to}}."
      ))
      gert::git_branch_move(branch = from, new_branch = to, repo = repo)
      rstudio_git_tickle()
      report_fishy_files(old_name = from, new_name = to)
    }
    return(invisible(to))
  }
  # cfg is now either fork or ours

  tr <- target_repo(cfg, role = "source", ask = FALSE)
  old_source_db <- tr$default_branch

  if (!isTRUE(tr$can_admin)) {
    ui_abort(
      "
      You don't seem to have {.field admin} permissions for the source repo
      {.val {tr$repo_spec}}, which is required to rename the default branch."
    )
  }

  old_local_db <- from %||%
    guess_local_default_branch(old_source_db, verbose = FALSE)

  if (old_local_db != old_source_db) {
    ui_bullets(c(
      "!" = "It's weird that the current default branch for your local repo and
             the source repo are different:",
      " " = "{.val {old_local_db}} (local) != {.val {old_source_db}} (source)"
    ))
    if (
      ui_nah(
        "Are you sure you want to proceed?",
        yes = "yes",
        no = "no",
        shuffle = FALSE
      )
    ) {
      ui_bullets(c("x" = "Cancelling."))
      return(invisible())
    }
  }

  source_update <- old_source_db != to
  if (source_update) {
    gh <- gh_tr(tr)
    gh(
      "POST /repos/{owner}/{repo}/branches/{from}/rename",
      from = old_source_db,
      new_name = to
    )
  }

  if (source_update) {
    ui_bullets(c(
      "v" = "Default branch of the source repo {.val {tr$repo_spec}} has moved:",
      " " = "{.val {old_source_db}} {cli::symbol$arrow_right} {.val {to}}"
    ))
  } else {
    ui_bullets(c(
      "i" = "Default branch of source repo {.val {tr$repo_spec}} is
             {.val {to}}. Nothing to be done."
    ))
  }

  report_fishy_files(old_name = old_local_db, new_name = to)

  rediscover_default_branch(old_name = old_local_db, report_on_source = FALSE)
}

rediscover_default_branch <- function(
  old_name = NULL,
  report_on_source = TRUE
) {
  maybe_name(old_name)

  # GitHub's official TODOs re: manually updating local environments
  # after a source repo renames the default branch:

  # git branch -m OLD-BRANCH-NAME NEW-BRANCH-NAME
  # git fetch origin
  # git branch -u origin/NEW-BRANCH-NAME NEW-BRANCH-NAME
  # git remote set-head origin -a

  # optionally
  # git remote prune origin

  # Note: they are assuming the relevant repo is known as origin, but it could
  # just as easily be, e.g., upstream.

  repo <- git_repo()
  if (
    !is.null(old_name) &&
      !gert::git_branch_exists(old_name, local = TRUE, repo = repo)
  ) {
    ui_abort("Can't find existing local branch named {.val {old_name}}.")
  }

  cfg <- github_remote_config(github_get = TRUE)
  check_for_config(cfg)

  tr <- target_repo(cfg, role = "source", ask = FALSE)
  db <- tr$default_branch

  # goal, in Git-speak: git remote set-head <remote> -a
  # goal, for humans: learn and record the default branch (i.e. the target of
  # the symbolic-ref refs/remotes/<remote>/HEAD) for the named remote
  # https://git-scm.com/docs/git-remote#Documentation/git-remote.txt-emset-headem
  # for very stale repos, a fetch is a necessary pre-requisite
  # I provide `refspec = db` to avoid fetching all refs, which can be VERY slow
  # for a repo like ggplot2 (several minutes, with no progress reporting)
  gert::git_fetch(remote = tr$name, refspec = db, verbose = FALSE, repo = repo)
  gert::git_remote_ls(remote = tr$name, verbose = FALSE, repo = repo)

  old_name <- old_name %||% guess_local_default_branch(db, verbose = FALSE)

  local_update <- old_name != db
  if (local_update) {
    # goal, in Git-speak: git branch -m <old_name> <db>
    gert::git_branch_move(branch = old_name, new_branch = db, repo = repo)
    rstudio_git_tickle()
  }

  # goal, in Git-speak: git branch -u <remote>/<db> <db>
  gert::git_branch_set_upstream(
    branch = db,
    upstream = glue("{tr$name}/{db}"),
    repo = repo
  )

  # goal: get rid of old remote tracking branch, e.g. origin/master
  # goal, in Git-speak: git remote prune origin
  # I provide a refspec to avoid fetching all refs, which can be VERY slow
  # for a repo like ggplot2 (several minutes, with no progress reporting)
  gert::git_fetch(
    remote = tr$name,
    refspec = glue("refs/heads/{old_name}:refs/remotes/{tr$name}/{old_name}"),
    verbose = FALSE,
    repo = repo,
    prune = TRUE
  )

  # for "ours" and "theirs", the source repo is the only remote on our radar and
  # we're done ingesting the default branch from the source repo
  # but for "fork", we also need to update
  #   the fork = the user's primary repo = the remote known as origin
  if (cfg$type == "fork") {
    old_name_fork <- cfg$origin$default_branch
    fork_update <- old_name_fork != db
    if (fork_update) {
      gh <- gh_tr(cfg$origin)
      gh(
        "POST /repos/{owner}/{repo}/branches/{from}/rename",
        from = old_name_fork,
        new_name = db
      )
      gert::git_fetch(
        remote = "origin",
        refspec = db,
        verbose = FALSE,
        repo = repo
      )
      gert::git_remote_ls(remote = "origin", verbose = FALSE, repo = repo)
      gert::git_fetch(
        remote = "origin",
        refspec = glue("refs/heads/{old_name}:refs/remotes/origin/{old_name}"),
        verbose = FALSE,
        repo = repo,
        prune = TRUE
      )
    }
  }

  if (report_on_source) {
    ui_bullets(c(
      "i" = "Default branch of the source repo {.val {tr$repo_spec}} is
             {.val {db}}."
    ))
  }

  if (local_update) {
    ui_bullets(c(
      "v" = "Default branch of local repo has moved:
             {.val {old_name}} {cli::symbol$arrow_right} {.val {db}}"
    ))
  } else {
    ui_bullets(c(
      "i" = "Default branch of local repo is {.val {db}}. Nothing to be done."
    ))
  }

  if (cfg$type == "fork") {
    if (fork_update) {
      ui_bullets(c(
        "v" = "Default branch of your fork has moved:
               {.val {old_name_fork}} {cli::symbol$arrow_right} {.val {db}}"
      ))
    } else {
      ui_bullets(c(
        "i" = "Default branch of your fork is {.val {db}}. Nothing to be done."
      ))
    }
  }

  invisible(db)
}

challenge_non_default_branch <- function(
  details = "Are you sure you want to proceed?",
  default_branch = NULL
) {
  actual <- git_branch()
  default_branch <- default_branch %||% git_default_branch()
  if (actual != default_branch) {
    if (
      ui_nah(c(
        "!" = "Current branch ({.val {actual}}) is not repo's default branch
             ({.val {default_branch}}).",
        " " = details
      ))
    ) {
      ui_abort("Cancelling. Not on desired branch.")
    }
  }
  invisible()
}

report_fishy_files <- function(old_name = "master", new_name = "main") {
  ui_bullets(c(
    "_" = "Be sure to update files that refer to the default branch by name.",
    " " = "Consider searching within your project for {.val {old_name}}."
  ))
  # I don't want failure of a fishy file check to EVER cause
  # git_default_branch_rename() to fail and prevent the call to
  # git_default_branch_rediscover()
  # using a simple try() wrapper because these hints are just "nice to have"
  try(fishy_github_actions(new_name = new_name), silent = TRUE)
  try(fishy_badges(old_name = old_name), silent = TRUE)
  try(fishy_bookdown_config(old_name = old_name), silent = TRUE)
}

# good test cases: downlit, purrr, pkgbuild, zealot, glue, bench,
# textshaping, scales
fishy_github_actions <- function(new_name = "main") {
  if (!uses_github_actions()) {
    return(invisible(character()))
  }
  workflow_dir <- proj_path(".github", "workflows")
  workflows <- dir_ls(workflow_dir, regexp = "[.]ya?ml$")

  f <- function(pth, new_name) {
    x <- yaml::read_yaml(pth)
    x_unlisted <- unlist(x)
    locs <- grep("branches", re_match(names(x_unlisted), "[^//.]+$")$.match)
    branches <- x_unlisted[locs]
    length(branches) == 0 || new_name %in% branches
  }

  includes_branch_name <- map_lgl(workflows, f, new_name = new_name)
  paths <- proj_rel_path(workflows[!includes_branch_name])

  if (length(paths) == 0) {
    return(invisible(character()))
  }

  paths <- sort(paths)
  ui_paths <- map_chr(paths, ui_path_impl)

  # TODO: the ui_paths used to be a nested bullet list
  # if that ever becomes possible/easier with cli, go back to that
  ui_bullets(c(
    "x" = "{cli::qty(length(ui_paths))}{?No/This/These} GitHub Action file{?s}
           {?/doesn't/don't} mention the new default branch {.val {new_name}}:",
    " " = "{.path {ui_paths}}"
  ))

  invisible(paths)
}

fishy_badges <- function(old_name = "master") {
  path <- find_readme()
  if (is.null(path)) {
    return(invisible(character()))
  }

  readme_lines <- read_utf8(path)
  badge_lines_range <- block_find(
    readme_lines,
    block_start = badge_start,
    block_end = badge_end
  )
  if (length(badge_lines_range) != 2) {
    return(invisible(character()))
  }
  badge_lines <- readme_lines[badge_lines_range[1]:badge_lines_range[2]]

  if (!any(grepl(old_name, badge_lines))) {
    return(invisible(character()))
  }

  ui_bullets(c(
    "x" = "Some badges appear to refer to the old default branch
           {.val {old_name}}.",
    "_" = "Check and correct, if needed, in this file: {.path {pth(path)}}"
  ))

  invisible(path)
}

fishy_bookdown_config <- function(old_name = "master") {
  # https://github.com/dncamp/shift/blob/a12a3fb0cd30ae864525f7a9f1f907a05f15f9a3/_bookdown.yml
  # https://github.com/Jattan08/Wonderland/blob/b9e7ddc694871d1d13a2a02abe2d3b4a944c4653/_bookdown.yml
  # edit: https://github.com/dncamp/shift/edit/master/%s
  # view: https://github.com/dncamp/shift/blob/master/%s
  # history: https://github.com/YOUR GITHUB USERNAME/YOUR REPO NAME/commits/master/%s
  bookdown_config <- dir_ls(
    proj_get(),
    regexp = "_bookdown[.]ya?ml$",
    recurse = TRUE
  )
  if (length(bookdown_config) == 0) {
    return(invisible(character()))
  }
  # I am (very weakly) worried about more than 1 match, hence the [[1]]
  bookdown_config <- purrr::discard(bookdown_config, \(x) grepl("revdep", x))[[
    1
  ]]

  bookdown_config_lines <- read_utf8(bookdown_config)
  linky_lines <- grep(
    "^(edit|view|history)",
    bookdown_config_lines,
    value = TRUE
  )

  if (!any(grepl(old_name, linky_lines))) {
    return(invisible(character()))
  }

  ui_bullets(c(
    "x" = "The bookdown configuration file may refer to the old default branch
           {.val {old_name}}.",
    "_" = "Check and correct, if needed, in this file:
           {.path {pth(bookdown_config)}}"
  ))

  invisible(path)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/git.R ---
#' Initialise a git repository
#'
#' `use_git()` initialises a Git repository and adds important files to
#' `.gitignore`. If user consents, it also makes an initial commit.
#'
#' @param message Message to use for first commit.
#' @family git helpers
#' @export
#' @examples
#' \dontrun{
#' use_git()
#' }
use_git <- function(message = "Initial commit") {
  needs_init <- !uses_git()
  if (needs_init) {
    ui_bullets(c("v" = "Initialising Git repo."))
    git_init()
    # hacky but helps prevent a pop-up in Positron, where early attempts to
    # interact with a newly created repo lead to:
    # Git: There are no available repositories
    # https://github.com/r-lib/usethis/pull/2011#issue-2380380721
    if (is_positron()) {
      Sys.sleep(1)
    }
  }

  use_git_ignore(git_ignore_lines)
  if (git_uncommitted(untracked = TRUE)) {
    git_ask_commit(message, untracked = TRUE)
  }

  if (needs_init && !is_positron()) {
    restart_rstudio(
      "A restart of RStudio is required to activate the Git pane."
    )
  }

  invisible(TRUE)
}

#' Add a git hook
#'
#' Sets up a git hook using the specified script. Creates a hook directory if
#' needed, and sets correct permissions on hook.
#'
#' @param hook Hook name. One of "pre-commit", "prepare-commit-msg",
#'   "commit-msg", "post-commit", "applypatch-msg", "pre-applypatch",
#'   "post-applypatch", "pre-rebase", "post-rewrite", "post-checkout",
#'   "post-merge", "pre-push", "pre-auto-gc".
#' @param script Text of script to run
#' @family git helpers
#' @export
use_git_hook <- function(hook, script) {
  check_uses_git()

  hook_path <- proj_path(".git", "hooks", hook)
  create_directory(path_dir(hook_path))

  write_over(hook_path, script)
  file_chmod(hook_path, "0744")

  invisible()
}

#' Tell Git to ignore files
#'
#' @param ignores Character vector of ignores, specified as file globs.
#' @param directory Directory relative to active project to set ignores
#' @family git helpers
#' @export
use_git_ignore <- function(ignores, directory = ".") {
  write_union(proj_path(directory, ".gitignore"), ignores)
  rstudio_git_tickle()
}

#' Configure Git
#'
#' Sets Git options, for either the user or the project ("global" or "local", in
#' Git terminology). Wraps [gert::git_config_set()] and
#' [gert::git_config_global_set()]. To inspect Git config, see
#' [gert::git_config()].
#'
#' @param ... Name-value pairs, processed as
#'   <[`dynamic-dots`][rlang::dyn-dots]>.
#'
#' @return Invisibly, the previous values of the modified components, as a named
#'   list.
#' @inheritParams edit
#'
#' @family git helpers
#' @export
#' @examples
#' \dontrun{
#' # set the user's global user.name and user.email
#' use_git_config(user.name = "Jane", user.email = "jane@example.org")
#'
#' # set the user.name and user.email locally, i.e. for current repo/project
#' use_git_config(
#'   scope = "project",
#'   user.name = "Jane",
#'   user.email = "jane@example.org"
#' )
#' }
use_git_config <- function(scope = c("user", "project"), ...) {
  scope <- match.arg(scope)

  dots <- list2(...)
  stopifnot(is_dictionaryish(dots))

  orig <- stats::setNames(
    vector(mode = "list", length = length(dots)),
    names(dots)
  )
  for (i in seq_along(dots)) {
    nm <- names(dots)[[i]]
    vl <- dots[[i]]
    if (scope == "user") {
      orig[nm] <- git_cfg_get(nm, "global") %||% list(NULL)
      gert::git_config_global_set(nm, vl)
    } else {
      check_uses_git()
      orig[nm] <- git_cfg_get(nm, "local") %||% list(NULL)
      gert::git_config_set(nm, vl, repo = git_repo())
    }
  }

  invisible(orig)
}

#' See or set the default Git protocol
#'
#' @description
#' Git operations that address a remote use a so-called "transport protocol".
#' usethis supports HTTPS and SSH. The protocol dictates the Git URL format used
#' when usethis needs to configure the first GitHub remote for a repo:
#' * `protocol = "https"` implies `https://github.com/<OWNER>/<REPO>.git`
#' * `protocol = "ssh"` implies `git@@github.com:<OWNER>/<REPO>.git`
#'
#' Two helper functions are available:
#'   * `git_protocol()` reveals the protocol "in force". As of usethis v2.0.0,
#'     this defaults to "https". You can change this for the duration of the
#'     R session with `use_git_protocol()`. Change the default for all R
#'     sessions with code like this in your `.Rprofile` (easily editable via
#'     [edit_r_profile()]):
#'     ```
#'     options(usethis.protocol = "ssh")
#'     ```
#'   * `use_git_protocol()` sets the Git protocol for the current R session
#'
#' This protocol only affects the Git URL for newly configured remotes. All
#' existing Git remote URLs are always respected, whether HTTPS or SSH.
#'
#' @param protocol One of "https" or "ssh"
#'
#' @return The protocol, either "https" or "ssh"
#' @export
#'
#' @examples
#' \dontrun{
#' git_protocol()
#'
#' use_git_protocol("ssh")
#' git_protocol()
#'
#' use_git_protocol("https")
#' git_protocol()
#' }
git_protocol <- function() {
  protocol <- tolower(getOption("usethis.protocol", "unset"))
  if (identical(protocol, "unset")) {
    ui_bullets(c("i" = "Defaulting to {.val https} Git protocol."))
    protocol <- "https"
  } else {
    check_protocol(protocol)
  }
  options("usethis.protocol" = protocol)
  getOption("usethis.protocol")
}

#' @rdname git_protocol
#' @export
use_git_protocol <- function(protocol) {
  options("usethis.protocol" = protocol)
  invisible(git_protocol())
}

check_protocol <- function(protocol) {
  if (
    !is_string(protocol) ||
      !(tolower(protocol) %in% c("https", "ssh"))
  ) {
    options(usethis.protocol = NULL)
    ui_abort("{.arg protocol} must be either {.val https} or {.val ssh}.")
  }
  invisible()
}

#' Configure and report Git remotes
#'
#' Two helpers are available:
#'   * `use_git_remote()` sets the remote associated with `name` to `url`.
#'   * `git_remotes()` reports the configured remotes, similar to
#'     `git remote -v`.
#'
#' @param name A string giving the short name of a remote.
#' @param url A string giving the url of a remote.
#' @param overwrite Logical. Controls whether an existing remote can be
#'   modified.
#'
#' @return Named list of Git remotes.
#' @export
#'
#' @examples
#' \dontrun{
#' # see current remotes
#' git_remotes()
#'
#' # add new remote named 'foo', a la `git remote add <name> <url>`
#' use_git_remote(name = "foo", url = "https://github.com/<OWNER>/<REPO>.git")
#'
#' # remove existing 'foo' remote, a la `git remote remove <name>`
#' use_git_remote(name = "foo", url = NULL, overwrite = TRUE)
#'
#' # change URL of remote 'foo', a la `git remote set-url <name> <newurl>`
#' use_git_remote(
#'   name = "foo",
#'   url = "https://github.com/<OWNER>/<REPO>.git",
#'   overwrite = TRUE
#' )
#'
#' # Scenario: Fix remotes when you cloned someone's repo, but you should
#' # have fork-and-cloned (in order to make a pull request).
#'
#' # Store origin = main repo's URL, e.g., "git@github.com:<OWNER>/<REPO>.git"
#' upstream_url <- git_remotes()[["origin"]]
#'
#' # IN THE BROWSER: fork the main GitHub repo and get your fork's remote URL
#' my_url <- "git@github.com:<ME>/<REPO>.git"
#'
#' # Rotate the remotes
#' use_git_remote(name = "origin", url = my_url)
#' use_git_remote(name = "upstream", url = upstream_url)
#' git_remotes()
#'
#' # Scenario: Add upstream remote to a repo that you fork-and-cloned, so you
#' # can pull upstream changes.
#' # Note: If you fork-and-clone via `usethis::create_from_github()`, this is
#' # done automatically!
#'
#' # Get URL of main GitHub repo, probably in the browser
#' upstream_url <- "git@github.com:<OWNER>/<REPO>.git"
#' use_git_remote(name = "upstream", url = upstream_url)
#' }
use_git_remote <- function(name = "origin", url, overwrite = FALSE) {
  check_name(name)
  maybe_name(url)
  check_bool(overwrite)

  remotes <- git_remotes()
  repo <- git_repo()

  if (name %in% names(remotes) && !overwrite) {
    ui_abort(c(
      "Remote {.val {name}} already exists.",
      "Use {.code overwrite = TRUE} to edit it anyway."
    ))
  }

  if (name %in% names(remotes)) {
    if (is.null(url)) {
      gert::git_remote_remove(remote = name, repo = repo)
    } else {
      gert::git_remote_set_url(url = url, remote = name, repo = repo)
    }
  } else if (!is.null(url)) {
    gert::git_remote_add(url = url, name = name, repo = repo)
  }

  invisible(git_remotes())
}

#' @rdname use_git_remote
#' @export
git_remotes <- function() {
  x <- gert::git_remote_list(repo = git_repo())
  if (nrow(x) == 0) {
    return(NULL)
  }
  stats::setNames(as.list(x$url), x$name)
}

# unexported function to improve my personal quality of life
git_clean <- function() {
  if (!is_interactive() || !uses_git()) {
    return(invisible())
  }

  st <- gert::git_status(staged = FALSE, repo = git_repo())
  paths <- st[st$status == "new", ][["file"]]
  n <- length(paths)
  if (n == 0) {
    ui_bullets(c("i" = "Found no untracked files."))
    return(invisible())
  }

  paths <- sort(paths)
  ui_paths <- map_chr(paths, ui_path_impl)
  ui_bullets(c(
    "i" = "{cli::qty(n)}There {?is/are} {n} untracked file{?s}:",
    bulletize(usethis_map_cli(ui_paths, template = "{.file <<x>>}"))
  ))

  if (
    ui_yep(
      "{cli::qty(n)}Do you want to remove {?it/them}?",
      yes = "yes",
      no = "no",
      shuffle = FALSE
    )
  ) {
    file_delete(paths)
    ui_bullets(c("v" = "{n} file{?s} deleted."))
  }
  rstudio_git_tickle()
  invisible()
}

#' Git/GitHub sitrep
#'
#' Get a situation report on your current Git/GitHub status. Useful for
#' diagnosing problems. The default is to report all values; provide values
#' for `tool` or `scope` to be more specific.
#'
#' @param tool Report for __git__, or __github__
#' @param scope Report globally for the current __user__, or locally for the
#'   current __project__
#'
#' @export
#' @examples
#' \dontrun{
#' # report all
#' git_sitrep()
#'
#' # report git for current user
#' git_sitrep("git", "user")
#' }
git_sitrep <- function(
  tool = c("git", "github"),
  scope = c("user", "project")
) {
  tool <- rlang::arg_match(tool, multiple = TRUE)
  scope <- rlang::arg_match(scope, multiple = TRUE)

  ui_silence(try(proj_get(), silent = TRUE))

  # git (global / user) --------------------------------------------------------
  init_default_branch <- git_cfg_get("init.defaultBranch", where = "global")
  if ("git" %in% tool && "user" %in% scope) {
    cli::cli_h3("Git global (user)")
    git_user_sitrep("user")
    kv_line(
      "Global (user-level) gitignore file",
      I("{.path {git_ignore_path('user')}}")
    )
    vaccinated <- git_vaccinated()
    kv_line("Vaccinated", vaccinated)
    if (!vaccinated) {
      ui_bullets(c("i" = "See {.fun usethis::git_vaccinate} to learn more."))
    }
    kv_line("Default Git protocol", ui_silence(git_protocol()))
    kv_line("Default initial branch name", init_default_branch)
  }

  # github (global / user) -----------------------------------------------------
  default_gh_host <- get_hosturl(default_api_url())
  if ("github" %in% tool && "user" %in% scope) {
    cli::cli_h3("GitHub user")
    kv_line("Default GitHub host", default_gh_host)
    pat_sitrep(default_gh_host, scope = "user")
  }

  # git and github for active project ------------------------------------------
  if (!"project" %in% scope) {
    return(invisible())
  }

  if (!proj_active()) {
    ui_bullets(c("i" = "No active usethis project."))
    return(invisible())
  }
  cli::cli_h2("Active usethis project: {.val {proj_get()}}")

  if (!uses_git()) {
    ui_bullets(c("i" = "Active project is not a Git repo."))
    return(invisible())
  }

  # current branch -------------------------------------------------------------
  branch <- tryCatch(git_branch(), error = function(e) NULL)
  tracking_branch <- if (is.null(branch)) {
    NA_character_
  } else {
    git_branch_tracking()
  }
  if (is.null(branch)) {
    branch <- cli::format_inline(ui_special())
  } else {
    branch <- cli::format_inline("{.val {branch}}")
  }
  if (is.na(tracking_branch)) {
    tracking_branch <- cli::format_inline(ui_special())
  } else {
    tracking_branch <- cli::format_inline("{.val {tracking_branch}}")
  }

  # local git config -----------------------------------------------------------
  if ("git" %in% tool) {
    cli::cli_h3("Git local (project)")
    git_user_sitrep("project")

    # default branch -------------------------------------------------------------
    default_branch_sitrep()

    # vertical alignment would make this nicer, but probably not worth it
    ui_bullets(c(
      "*" = "Current local branch {cli::symbol$arrow_right} remote tracking
             branch:",
      " " = "{branch} {cli::symbol$arrow_right} {tracking_branch}"
    ))
  }

  # GitHub remote config -------------------------------------------------------
  if ("github" %in% tool) {
    cli::cli_h3("GitHub project")

    cfg <- github_remote_config()

    if (cfg$type == "no_github") {
      ui_bullets(c("i" = "Project does not use GitHub."))
      return(invisible())
    }

    repo_host <- cfg$host_url
    if (!is.na(repo_host) && repo_host != default_gh_host) {
      cli::cli_text("Host:")
      kv_line("Non-default GitHub host", repo_host)
      pat_sitrep(repo_host, scope = "project", scold_for_renviron = FALSE)
      cli::cli_text("Project:")
    }

    ui_bullets(format(cfg))
  }

  invisible()
}

git_user_sitrep <- function(scope = c("user", "project")) {
  scope <- rlang::arg_match(scope)

  where <- where_from_scope(scope)

  user <- git_user_get(where)
  user_local <- git_user_get("local")

  if (scope == "project" && !all(map_lgl(user_local, is.null))) {
    ui_bullets(c("i" = "This repo has a locally configured user."))
  }

  kv_line("Name", user$name)
  kv_line("Email", user$email)

  git_user_check(user)

  invisible(NULL)
}

git_user_check <- function(user) {
  if (all(map_lgl(user, is.null))) {
    hint <-
      'use_git_config(user.name = "<your name>", user.email = "<your email>")'
    ui_bullets(c(
      "x" = "Git user's name and email are not set.",
      "i" = "Configure using {.code {hint}}."
    ))
    return(invisible(NULL))
  }

  if (is.null(user$name)) {
    hint <- 'use_git_config(user.name = "<your name>")'
    ui_bullets(c(
      "x" = "Git user's name is not set.",
      "i" = "Configure using {.code {hint}}."
    ))
  }

  if (is.null(user$email)) {
    hint <- 'use_git_config(user.email = "<your email>")'
    ui_bullets(c(
      "x" = "Git user's email is not set.",
      "i" = "Configure using {.code {hint}}."
    ))
  }
}

default_branch_sitrep <- function() {
  tryCatch(
    kv_line("Default branch", git_default_branch()),
    error_default_branch = function(e) {
      if (has_name(e, "db_local")) {
        # FYI existence of db_local implies existence of db_source
        ui_bullets(c(
          "x" = "Default branch mismatch between local repo and remote.",
          "i" = "The default branch of the {.val {e$db_source$name}} remote is
                 {.val {e$db_source$default_branch}}.",
          "!" = "The local repo has no branch named
                 {.val {e$db_source$default_branch}}.",
          "_" = "Call {.run [git_default_branch_rediscover()](usethis::git_default_branch_rediscover())} to resolve this."
        ))
      } else if (has_name(e, "db_source")) {
        ui_bullets(c(
          "x" = "Default branch mismatch between local repo and remote.",
          "i" = "The default branch of the {.val {e$db_source$name}} remote is
                 {.val {e$db_source$default_branch}}.",
          "!" = "The local repo has no branch by that name, nor any other
                 obvious candidates.",
          "_" = "Call {.run [git_default_branch_rediscover()](usethis::git_default_branch_rediscover())} to resolve this."
        ))
      } else {
        ui_bullets(c("Default branch cannot be determined."))
      }
    }
  )
}

# Vaccination -------------------------------------------------------------

#' Vaccinate your global gitignore file
#'
#' Adds `.Rproj.user`, `.Rhistory`, `.Rdata`, `.httr-oauth`, `.DS_Store`, and
#' `.quarto` to your global (a.k.a. user-level) `.gitignore`. This is good
#' practice as it decreases the chance that you will accidentally leak
#' credentials to GitHub. `git_vaccinate()` also tries to detect and fix the
#' situation where you have a global gitignore file, but it's missing from your
#' global Git config.
#'
#' @export
git_vaccinate <- function() {
  ensure_core_excludesFile()
  path <- git_ignore_path(scope = "user")
  if (!file_exists(path)) {
    ui_bullets(c(
      "v" = "Creating the global (user-level) gitignore: {.path {pth(path)}}"
    ))
  }
  write_union(path, git_ignore_lines)
}

git_vaccinated <- function() {
  path <- git_ignore_path("user")
  if (is.null(path) || !file_exists(path)) {
    return(FALSE)
  }
  # on Windows, if ~/ is present, take care to expand it the fs way
  lines <- read_utf8(user_path_prep(path))
  all(git_ignore_lines %in% lines)
}

git_ignore_lines <- c(
  ".Rproj.user",
  ".Rhistory",
  ".RData",
  ".httr-oauth",
  ".DS_Store",
  ".quarto"
)


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/github-actions.R ---
#' Set up a GitHub Actions workflow
#'
#' @description
#' Sets up continuous integration (CI) for an R package that is developed on
#' GitHub using [GitHub Actions](https://github.com/features/actions) (GHA). CI
#' can be used to trigger various operations for each push or pull request, e.g.
#' running `R CMD check` or building and deploying a pkgdown site.
#'
#' ## Core workflows
#'
#' There are three particularly important workflows that are used by many
#' packages:
#'
#' * `check-standard`: Run `R CMD check` using R-latest on Linux, Mac, and
#'    Windows, and using R-devel and R-oldrel on Linux. This is a good baseline
#'    if you plan on submitting your package to CRAN.
#' * `test-coverage`: Compute test coverage and report to
#'    <https://about.codecov.io> by calling [covr::codecov()].
#' * `pkgdown`: Automatically build and publish a pkgdown website.
#'    But we recommend instead calling [use_pkgdown_github_pages()], which
#'    sets up the `pkgdown` workflow AND performs other important set up.
#'
#' If you call `use_github_action()` without arguments, you'll get a choice of
#' some recommended workflows. Otherwise you can specify the name of any
#' workflow provided by `r-lib/actions`, which are listed at
#' <https://github.com/r-lib/actions/tree/v2/examples>. Finally you can supply
#' the full `url` to any workflow on GitHub.
#'
#' ## Other workflows
#' Other specific workflows are worth mentioning:
#' * `format-suggest` or `format-check` from
#'   [Air](https://posit-dev.github.io/air/):
#'   `r lifecycle::badge("experimental")` Either of these workflows is a great
#'   way to keep your code well-formatted once you adopt Air in a project
#'   (possibly via [use_air()]). Here's how to set them up:
#'
#'   ```
#'   use_github_action(url = "https://github.com/posit-dev/setup-air/blob/main/examples/format-suggest.yaml")
#'   use_github_action(url = "https://github.com/posit-dev/setup-air/blob/main/examples/format-check.yaml")
#'   ```
#'
#'   Learn more from
#'   [Air's documentation of its GHA integrations](https://posit-dev.github.io/air/integration-github-actions.html).
#' * `pr-commands`: `r lifecycle::badge("superseded")` Enables the use of two
#'    R-specific commands in pull request issue comments: `/document` to run
#'    `roxygen2::roxygenise()` and `/style` to run `styler::style_pkg()`. Both
#'    will update the PR with any changes once they're done.
#'
#'    We don't recommend new adoption of the `pr-commands` workflow. For
#'    code formatting, the Air workflows described above are preferred. We
#'    plan to re-implement documentation updates using a similar approach.
#'
#' @param name Name of one of the example workflows from
#'   <https://github.com/r-lib/actions/tree/v2/examples> (with or without
#'   extension), e.g. `"pkgdown"`, `"check-standard.yaml"`.
#'
#'   If the `name` starts with `check-`, `save_as` defaults to
#'   `R-CMD-check.yaml` and `badge` defaults to `TRUE`.
#' @param ref Desired Git reference, usually the name of a tag (`"v2"`) or
#'   branch (`"main"`). Other possibilities include a commit SHA (`"d1c516d"`)
#'   or `"HEAD"` (meaning "tip of remote's default branch"). If not specified,
#'   defaults to the latest published release of `r-lib/actions`
#'   (<https://github.com/r-lib/actions/releases>).
#' @param url The full URL to a `.yaml` file on GitHub. See more details in
#'   [use_github_file()].
#' @param save_as Name of the local workflow file. Defaults to `name` or
#'   `fs::path_file(url)`. Do not specify any other part of the path; the parent
#'   directory will always be `.github/workflows`, within the active project.
#' @param readme The full URL to a `README` file that provides more details
#'   about the workflow. Ignored when `url` is `NULL`.
#' @param badge Should we add a badge to the `README`?
#' @inheritParams use_template
#'
#' @examples
#' \dontrun{
#' use_github_action()
#'
#' use_github_action("check-standard")
#'
#' use_github_action("pkgdown")
#'
#' use_github_action(
#'   url = "https://github.com/posit-dev/setup-air/blob/main/examples/format-suggest.yaml"
#' )
#' }
#' @export
use_github_action <- function(
  name = NULL,
  ref = NULL,
  url = NULL,
  save_as = NULL,
  readme = NULL,
  ignore = TRUE,
  open = FALSE,
  badge = NULL
) {
  maybe_name(name)
  maybe_name(ref)
  maybe_name(url)
  maybe_name(save_as)
  maybe_name(readme)
  check_bool(ignore)
  check_bool(open)
  check_bool(badge, allow_null = TRUE)

  if (is.null(url)) {
    name <- name %||% choose_gha_workflow()

    if (path_ext(name) == "") {
      name <- path_ext_set(name, "yaml")
    }

    ref <- ref %||% latest_release()
    url <- glue(
      "https://raw.githubusercontent.com/r-lib/actions/{ref}/examples/{name}"
    )
    readme <- glue(
      "https://github.com/r-lib/actions/blob/{ref}/examples/README.md"
    )
  }

  withr::defer(rstudio_git_tickle())

  use_dot_github(ignore = ignore)

  if (is.null(save_as)) {
    if (is_check_action(url)) {
      save_as <- "R-CMD-check.yaml"
    } else {
      save_as <- path_file(url)
    }
  }

  save_as <- path(".github", "workflows", save_as)
  create_directory(path_dir(proj_path(save_as)))

  if (grepl("^http", url)) {
    # `ignore = FALSE` because we took care of this at directory level, above
    new <- use_github_file(url, save_as = save_as, ignore = FALSE, open = open)
  } else {
    # local file case, https://github.com/r-lib/usethis/issues/1548
    contents <- read_utf8(url)
    new <- write_over(proj_path(save_as), contents)
  }

  if (!is.null(readme)) {
    ui_bullets(c("_" = "Learn more at {.url {readme}}."))
  }

  if (badge %||% is_check_action(url)) {
    use_github_actions_badge(path_file(save_as))
  }
  if (badge %||% is_coverage_action(url)) {
    use_codecov_badge(target_repo_spec())
  }

  invisible(new)
}

choose_gha_workflow <- function(error_call = caller_env()) {
  if (!is_interactive()) {
    cli::cli_abort(
      "{.arg name} is absent and must be supplied",
      call = error_call
    )
  }

  prompt <- cli::format_inline(
    "Which action do you want to add? (0 to exit)\n",
    "(See {.url https://github.com/r-lib/actions/tree/v2/examples} for other options)"
  )
  # Any changes here also need to be reflected in documentation
  workflows <- c(
    "check-standard" = "Run `R CMD check` on Linux, macOS, and Windows",
    "test-coverage" = "Compute test coverage and report to https://about.codecov.io"
  )
  options <- paste0(cli::style_bold(names(workflows)), ": ", workflows)

  choice <- utils::menu(
    title = prompt,
    choices = options
  )
  if (choice == 0) {
    cli::cli_abort("Selection terminated", call = error_call)
  }

  names(workflows)[choice]
}

is_check_action <- function(url) {
  grepl("^check-", path_file(url))
}

is_coverage_action <- function(url) {
  grepl("test-coverage", path_file(url))
}

#' Generates a GitHub Actions badge
#'
#' Generates a GitHub Actions badge and that's all. This exists primarily for
#' internal use.
#'
#' @keywords internal
#' @param name Name of the workflow's YAML configuration file (with or without
#'   extension), e.g. `"R-CMD-check"`, `"R-CMD-check.yaml"`.
#' @inheritParams use_github_action
#' @export
use_github_actions_badge <- function(
  name = "R-CMD-check.yaml",
  repo_spec = NULL
) {
  if (path_ext(name) == "") {
    name <- path_ext_set(name, "yaml")
  }
  repo_spec <- repo_spec %||% target_repo_spec()
  enc_name <- utils::URLencode(name)
  img <- glue(
    "https://github.com/{repo_spec}/actions/workflows/{enc_name}/badge.svg"
  )
  url <- glue("https://github.com/{repo_spec}/actions/workflows/{enc_name}")

  use_badge(path_ext_remove(name), url, img)
}

# tidyverse GHA setup ----------------------------------------------------------

#' @details
#' * `use_tidy_github_actions()`: Sets up the following workflows using [GitHub
#' Actions](https://github.com/features/actions):
#'   - Run `R CMD check` on the current release, devel, and four previous
#'     versions of R. The build matrix also ensures `R CMD check` is run at
#'     least once on each of the three major operating systems (Linux, macOS,
#'     and Windows).
#'   - Report test coverage.
#'   - Build and deploy a pkgdown site.
#'   - Check the formatting of incoming pull requests with Air and suggest
#'     fixes as necessary.
#'
#'     This is how the tidyverse team checks its packages, but it is overkill
#'     for less widely used packages. For `R CMD check`, consider using the more
#'     streamlined workflow set up by
#'     [`use_github_action("check-standard")`][use_github_action].
#' @export
#' @rdname tidyverse
#' @inheritParams use_github_action
use_tidy_github_actions <- function(ref = NULL) {
  repo_spec <- target_repo_spec()

  use_github_action("check-full.yaml", ref = ref, badge = TRUE)

  use_github_action("pkgdown", ref = ref)

  use_coverage(repo_spec = repo_spec)
  use_github_action("test-coverage", ref = ref)

  if (!uses_air()) {
    ui_bullets(c(
      "!" = "Can't find an {.file air.toml} file. Do you need to run
             {.run [use_air()](usethis::use_air())}?"
    ))
  }
  use_github_action(
    url = "https://github.com/posit-dev/setup-air/blob/main/examples/format-suggest.yaml"
  )

  # TODO: give `pr-commands` similar treatment once we have a full replacement,
  # i.e. the aspirational `document-suggest`
  old_configs <- proj_path(c(".travis.yml", "appveyor.yml"))
  has_appveyor_travis <- file_exists(old_configs)

  if (any(has_appveyor_travis)) {
    if (
      ui_yep("Remove existing {.path .travis.yml} and {.path appveyor.yml}?")
    ) {
      file_delete(old_configs[has_appveyor_travis])
      ui_bullets(c("_" = "Remove old badges from README."))
    }
  }

  invisible(TRUE)
}

# GHA helpers ------------------------------------------------------------------

uses_github_actions <- function() {
  path <- proj_path(".github", "workflows")
  file_exists(path)
}

check_uses_github_actions <- function() {
  if (uses_github_actions()) {
    return(invisible())
  }

  ui_abort(c(
    "Cannot detect that package {.pkg {project_name()}} already uses GitHub Actions.",
    "Do you need to run {.run [use_github_action()](usethis::use_github_action())}?"
  ))
}

latest_release <- function(repo_spec = "https://github.com/r-lib/actions") {
  parsed <- parse_repo_url(repo_spec)
  # https://docs.github.com/en/rest/reference/releases#list-releases
  raw_releases <- gh::gh(
    "/repos/{owner}/{repo}/releases",
    owner = spec_owner(parsed$repo_spec),
    repo = spec_repo(parsed$repo_spec),
    .api_url = parsed$host,
    .limit = Inf
  )
  tag_names <- purrr::discard(
    map_chr(raw_releases, "tag_name"),
    map_lgl(raw_releases, "prerelease")
  )
  pick_tag(tag_names)
}

# 1) filter to releases in the latest major version series
# 2) return the max, according to R's numeric_version logic
pick_tag <- function(nm) {
  dat <- data.frame(nm = nm, stringsAsFactors = FALSE)
  dat$version <- numeric_version(sub("^[^0-9]*", "", dat$nm))
  dat <- dat[dat$version == max(dat$version), ]
  dat$nm[1]
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/github-labels.R ---
#' Manage GitHub issue labels
#'
#' @description
#' `use_github_labels()` can create new labels, update colours and descriptions,
#' and optionally delete GitHub's default labels (if `delete_default = TRUE`).
#' It will never delete labels that have associated issues.
#'
#' `use_tidy_github_labels()` calls `use_github_labels()` with tidyverse
#' conventions powered by `tidy_labels()`, `tidy_labels_rename()`,
#' `tidy_label_colours()` and `tidy_label_descriptions()`.
#'
#' ## tidyverse label usage
#' Labels are used as part of the issue-triage process, designed to minimise the
#' time spent re-reading issues. The absence of a label indicates that an issue
#' is new, and has yet to be triaged.
#'
#' There are four mutually exclusive labels that indicate the overall "type" of
#' issue:
#'
#' * `bug`: an unexpected problem or unintended behavior.
#' * `documentation`: requires changes to the docs.
#' * `feature`: feature requests and enhancement.
#' * `upkeep`: general package maintenance work that makes future development
#'   easier.
#'
#' Then there are five labels that are needed in most repositories:
#'
#' * `breaking change`: issue/PR will requires a breaking change so should
#'   be not be included in patch releases.
#' * `reprex` indicates that an issue does not have a minimal reproducible
#'   example, and that a reply has been sent requesting one from the user.
#' * `good first issue` indicates a good issue for first-time contributors.
#' * `help wanted` indicates that a maintainer wants help on an issue.
#' * `wip` indicates that someone is working on it or has promised to.
#'
#' Finally most larger repos will accumulate their own labels for specific
#' areas of functionality. For example, usethis has labels like "description",
#' "paths", "readme", because time has shown these to be common sources of
#' problems. These labels are helpful for grouping issues so that you can
#' tackle related problems at the same time.
#'
#' Repo-specific issues should have a grey background (`#eeeeee`) and an emoji.
#' This keeps the issue page visually harmonious while still giving enough
#' variation to easily distinguish different types of label.
#'
#' @param labels A character vector giving labels to add.
#' @param rename A named vector with names giving old names and values giving
#'   new names.
#' @param colours,descriptions Named character vectors giving hexadecimal
#'   colours (like `e02a2a`) and longer descriptions. The names should match
#'   label names, and anything unmatched will be left unchanged. If you create a
#'   new label, and don't supply colours, it will be given a random colour.
#' @param delete_default If `TRUE`, removes GitHub default labels that do not
#'   appear in the `labels` vector and that do not have associated issues.
#'
#' @export
#' @examples
#' \dontrun{
#' # typical use in, e.g., a new tidyverse project
#' use_github_labels(delete_default = TRUE)
#'
#' # create labels without changing colours/descriptions
#' use_github_labels(
#'   labels = c("foofy", "foofier", "foofiest"),
#'   colours = NULL,
#'   descriptions = NULL
#' )
#'
#' # change descriptions without changing names/colours
#' use_github_labels(
#'   labels = NULL,
#'   colours = NULL,
#'   descriptions = c("foofiest" = "the foofiest issue you ever saw")
#' )
#' }
use_github_labels <- function(
  labels = character(),
  rename = character(),
  colours = character(),
  descriptions = character(),
  delete_default = FALSE
) {
  # Want to ensure we always have the latest label info
  withr::local_options(gh_cache = FALSE)

  tr <- target_repo(github_get = TRUE, ok_configs = c("ours", "fork"))
  check_can_push(tr = tr, "to modify labels")

  gh <- gh_tr(tr)

  cur_labels <- gh("GET /repos/{owner}/{repo}/labels")
  label_attr <- function(x, l, mapper = map_chr) {
    mapper(l, x, .default = NA)
  }

  # Rename existing labels
  cur_label_names <- label_attr("name", cur_labels)
  to_rename <- intersect(cur_label_names, names(rename))
  if (length(to_rename) > 0) {
    dat <- data.frame(from = to_rename, to = rename[to_rename])
    delta <- glue_data(
      dat,
      "{.val <<from>>} {cli::symbol$arrow_right} {.val <<to>>}",
      .open = "<<",
      .close = ">>"
    )
    ui_bullets(c(
      "v" = "Renaming labels:",
      bulletize(delta)
    ))

    # Can't do this at label level, i.e. "old_label_name --> new_label_name"
    # Fails if "new_label_name" already exists
    # https://github.com/r-lib/usethis/issues/551
    # Must first PATCH issues, then sort out labels
    issues <- map(
      to_rename,
      \(x) gh("GET /repos/{owner}/{repo}/issues", labels = x)
    )
    issues <- purrr::flatten(issues)
    number <- map_int(issues, "number")
    old_labels <- map(issues, "labels")
    df <- data.frame(
      number = rep.int(number, lengths(old_labels))
    )
    df$labels <- purrr::flatten(old_labels)
    df$labels <- map_chr(df$labels, "name")

    # enact relabelling
    m <- match(df$labels, names(rename))
    df$labels[!is.na(m)] <- rename[m[!is.na(m)]]
    df <- df[!duplicated(df), ]
    new_labels <- split(df$labels, df$number)
    purrr::iwalk(
      new_labels,
      \(x, y) {
        gh(
          "PATCH /repos/{owner}/{repo}/issues/{issue_number}",
          issue_number = y,
          labels = I(x)
        )
      }
    )

    # issues have correct labels now; safe to edit labels themselves
    purrr::walk(
      to_rename,
      \(x) gh("DELETE /repos/{owner}/{repo}/labels/{name}", name = x)
    )
    labels <- union(labels, setdiff(rename, cur_label_names))
  } else {
    ui_bullets(c("i" = "No labels need renaming."))
  }

  cur_labels <- gh("GET /repos/{owner}/{repo}/labels")
  cur_label_names <- label_attr("name", cur_labels)

  # Add missing labels
  if (all(labels %in% cur_label_names)) {
    ui_bullets(c("i" = "No new labels needed."))
  } else {
    to_add <- setdiff(labels, cur_label_names)
    ui_bullets(c(
      "v" = "Adding missing labels:",
      bulletize(usethis_map_cli(to_add))
    ))

    for (label in to_add) {
      gh(
        "POST /repos/{owner}/{repo}/labels",
        name = label,
        color = purrr::pluck(colours, label, .default = random_colour()),
        description = purrr::pluck(descriptions, label, .default = "")
      )
    }
  }

  cur_labels <- gh("GET /repos/{owner}/{repo}/labels")
  cur_label_names <- label_attr("name", cur_labels)

  # Update colours
  cur_label_colours <- set_names(
    label_attr("color", cur_labels),
    cur_label_names
  )
  if (identical(cur_label_colours[names(colours)], colours)) {
    ui_bullets(c("i" = "Label colours are up-to-date."))
  } else {
    to_update <- intersect(cur_label_names, names(colours))
    ui_bullets(c(
      "v" = "Updating colours:",
      bulletize(usethis_map_cli(to_update))
    ))

    for (label in to_update) {
      gh(
        "PATCH /repos/{owner}/{repo}/labels/{name}",
        name = label,
        color = colours[[label]]
      )
    }
  }

  # Update descriptions
  cur_label_descriptions <- set_names(
    label_attr("description", cur_labels),
    cur_label_names
  )
  if (identical(cur_label_descriptions[names(descriptions)], descriptions)) {
    ui_bullets(c("i" = "Label descriptions are up-to-date."))
  } else {
    to_update <- intersect(cur_label_names, names(descriptions))
    ui_bullets(c(
      "v" = "Updating descriptions:",
      bulletize(usethis_map_cli(to_update))
    ))

    for (label in to_update) {
      gh(
        "PATCH /repos/{owner}/{repo}/labels/{name}",
        name = label,
        description = descriptions[[label]]
      )
    }
  }

  # Delete unused default labels
  if (delete_default) {
    default <- map_lgl(cur_labels, "default")
    to_remove <- setdiff(cur_label_names[default], labels)

    if (length(to_remove) > 0) {
      ui_bullets(c(
        "v" = "Removing default labels:",
        bulletize(usethis_map_cli(to_remove))
      ))

      for (label in to_remove) {
        issues <- gh("GET /repos/{owner}/{repo}/issues", labels = label)
        if (length(issues) > 0) {
          ui_bullets(c(
            "_" = "Delete {.val {label}} label manually; it has associated issues."
          ))
        } else {
          gh("DELETE /repos/{owner}/{repo}/labels/{name}", name = label)
        }
      }
    }
  }
}

#' @export
#' @rdname use_github_labels
use_tidy_github_labels <- function() {
  use_github_labels(
    labels = tidy_labels(),
    rename = tidy_labels_rename(),
    colours = tidy_label_colours(),
    descriptions = tidy_label_descriptions(),
    delete_default = TRUE
  )
}

#' @rdname use_github_labels
#' @export
tidy_labels <- function() {
  names(tidy_label_colours())
}

#' @rdname use_github_labels
#' @export
tidy_labels_rename <- function() {
  c(
    # before           = after
    "enhancement" = "feature",
    "question" = "reprex",
    "good first issue" = "good first issue :heart:",
    "help wanted" = "help wanted :heart:",
    "docs" = "documentation"
  )
}


#' @rdname use_github_labels
#' @export
tidy_label_colours <- function() {
  # http://tristen.ca/hcl-picker/#/hlc/5/0.26/E0B3A2/E1B996
  c(
    "breaking change :skull_and_crossbones:" = "E0B3A2",
    "bug" = "E0B3A2",
    "documentation" = "CBBAB8",
    "feature" = "B4C3AE",
    "upkeep" = "C2ACC0",
    "wip" = "E1B996",
    "good first issue :heart:" = "CBBAB8",
    "help wanted :heart:" = "C5C295",
    "reprex" = "C5C295",
    "tidy-dev-day :nerd_face:" = "CBBAB8"
  )
}

#' @rdname use_github_labels
#' @export
tidy_label_descriptions <- function() {
  c(
    "bug" = "an unexpected problem or unintended behavior",
    "feature" = "a feature request or enhancement",
    "upkeep" = "maintenance, infrastructure, and similar",
    "reprex" = "needs a minimal reproducible example",
    "wip" = "work in progress",
    "documentation" = "",
    "good first issue :heart:" = "good issue for first-time contributors",
    "help wanted :heart:" = "we'd love your help!",
    "breaking change :skull_and_crossbones:" = "API change likely to affect existing code",
    "tidy-dev-day :nerd_face:" = "Tidyverse Developer Day"
  )
}

random_colour <- function() {
  format(as.hexmode(sample(256 * 256 * 256 - 1, 1)), width = 6)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/github-pages.R ---
#' Configure a GitHub Pages site
#'
#' Activates or reconfigures a GitHub Pages site for a project hosted on GitHub.
#' This function anticipates two specific usage modes:
#' * Publish from the root directory of a `gh-pages` branch, which is assumed to
#'   be only (or at least primarily) a remote branch. Typically the `gh-pages`
#'   branch is managed by an automatic "build and deploy" job, such as the one
#'   configured by [`use_github_action("pkgdown")`][use_github_action()].
#' * Publish from the `"/docs"` directory of a "regular" branch, probably the
#'   repo's default branch. The user is assumed to have a plan for how they will
#'   manage the content below `"/docs"`.
#'

#' @param branch,path Branch and path for the site source. The default of
#'   `branch = "gh-pages"` and `path = "/"` reflects strong GitHub support for
#'   this configuration: when a `gh-pages` branch is first created, it is
#'   *automatically* published to Pages, using the source found in `"/"`. If a
#'   `gh-pages` branch does not yet exist on the host, `use_github_pages()`
#'   creates an empty, orphan remote branch.
#'
#'   The most common alternative is to use the repo's default branch, coupled
#'   with `path = "/docs"`. It is the user's responsibility to ensure that this
#'   `branch` pre-exists on the host.
#'
#'   Note that GitHub does not support an arbitrary `path` and, at the time of
#'   writing, only `"/"` or `"/docs"` are accepted.

#' @param cname Optional, custom domain name. The `NA` default means "don't set
#'   or change this", whereas a value of `NULL` removes any previously
#'   configured custom domain.
#'
#'   Note that this *can* add or modify a CNAME file in your repository. If you
#'   are using Pages to host a pkgdown site, it is better to specify its URL in
#'   the pkgdown config file and let pkgdown manage CNAME.
#'

#' @seealso
#' * [use_pkgdown_github_pages()] combines `use_github_pages()` with other
#' functions to fully configure a pkgdown site
#' * <https://docs.github.com/en/pages>
#' * <https://docs.github.com/en/rest/pages>

#' @return Site metadata returned by the GitHub API, invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' use_github_pages()
#' use_github_pages(branch = git_default_branch(), path = "/docs")
#' }
use_github_pages <- function(branch = "gh-pages", path = "/", cname = NA) {
  check_name(branch)
  check_name(path)
  check_string(cname, allow_empty = FALSE, allow_na = TRUE, allow_null = TRUE)
  tr <- target_repo(github_get = TRUE, ok_configs = c("ours", "fork"))
  check_can_push(tr = tr, "to turn on GitHub Pages")

  gh <- gh_tr(tr)
  safe_gh <- purrr::safely(gh)

  if (branch == "gh-pages") {
    new_branch <- create_gh_pages_branch(tr, branch = "gh-pages")
    if (new_branch) {
      # merely creating gh-pages branch automatically activates publishing
      # BUT we need to give the servers time to sync up before a new GET
      # retrieves accurate info... ask me how I know
      Sys.sleep(2)
    }
  }

  site <- safe_gh("GET /repos/{owner}/{repo}/pages")[["result"]]

  if (is.null(site)) {
    ui_bullets(c(
      "v" = "Activating GitHub Pages for {.val {tr$repo_spec}}."
    ))
    site <- gh(
      "POST /repos/{owner}/{repo}/pages",
      source = list(branch = branch, path = path),
      .accept = "application/vnd.github.switcheroo-preview+json"
    )
  }

  need_update <-
    site$source$branch != branch ||
    site$source$path != path ||
    (is.null(cname) && !is.null(site$cname)) ||
    (is_string(cname) && (is.null(site$cname) || cname != site$cname))

  if (need_update) {
    args <- list(
      endpoint = "PUT /repos/{owner}/{repo}/pages",
      source = list(branch = branch, path = path)
    )
    if (is.null(cname) && !is.null(site$cname)) {
      # this goes out as a JSON `null`, which is necessary to clear cname
      args$cname <- NA
    }
    if (is_string(cname) && (is.null(site$cname) || cname != site$cname)) {
      args$cname <- cname
    }
    Sys.sleep(2)
    exec(gh, !!!args)
    Sys.sleep(2)
    site <- safe_gh("GET /repos/{owner}/{repo}/pages")[["result"]]
  }

  ui_bullets(c("v" = "GitHub Pages is publishing from:"))
  if (!is.null(site$cname)) {
    kv_line("Custom domain", site$cname)
  }
  kv_line("URL", site$html_url)
  kv_line("Branch", site$source$branch)
  kv_line("Path", site$source$path)

  invisible(site)
}

# returns FALSE if it does NOT create the branch (because it already exists)
# returns TRUE if it does create the branch
create_gh_pages_branch <- function(tr, branch = "gh-pages") {
  gh <- gh_tr(tr)
  safe_gh <- purrr::safely(gh)

  branch_GET <- safe_gh(
    "GET /repos/{owner}/{repo}/branches/{branch}",
    branch = branch
  )

  if (!inherits(branch_GET$error, "http_error_404")) {
    return(FALSE)
  }

  ui_bullets(c(
    "v" = "Initializing empty, orphan branch {.val {branch}} in GitHub repo
           {.val {tr$repo_spec}}."
  ))

  # GitHub no longer allows you to directly create an empty tree
  # hence this roundabout method of getting an orphan branch with no files
  tree <- gh(
    "POST /repos/{owner}/{repo}/git/trees",
    tree = list(list(
      path = "_temp_file_ok_to_delete",
      mode = "100644",
      type = "blob",
      content = ""
    ))
  )
  commit <- gh(
    "POST /repos/{owner}/{repo}/git/commits",
    message = "Init orphan branch",
    tree = tree$sha
  )
  ref <- gh(
    "POST /repos/{owner}/{repo}/git/refs",
    ref = glue("refs/heads/{branch}"),
    sha = commit$sha
  )
  # this should succeed, but if somehow it does not, it's not worth failing and
  # leaving pkgdown + GitHub Pages setup half-done --> why I use safe_gh()
  safe_gh(
    "DELETE /repos/{owner}/{repo}/contents/_temp_file_ok_to_delete",
    message = "Remove temp file",
    sha = purrr::pluck(tree, "tree", 1, "sha"),
    branch = branch
  )

  TRUE
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/github.R ---
#' Connect a local repo with GitHub
#'
#' @description
#' `use_github()` takes a local project and:
#' * Checks that the initial state is good to go:
#'   - Project is already a Git repo
#'   - Current branch is the default branch, e.g. `main` or `master`
#'   - No uncommitted changes
#'   - No pre-existing `origin` remote
#' * Creates an associated repo on GitHub
#' * Adds that GitHub repo to your local repo as the `origin` remote
#' * Makes an initial push to GitHub
#' * Calls [use_github_links()], if the project is an R package
#' * Configures `origin/DEFAULT` to be the upstream branch of the local
#'   `DEFAULT` branch, e.g. `main` or `master`
#'
#' See below for the authentication setup that is necessary for all of this to
#' work.
#'
#' @template double-auth
#'
#' @param organisation If supplied, the repo will be created under this
#'   organisation, instead of the login associated with the GitHub token
#'   discovered for this `host`. The user's role and the token's scopes must be
#'   such that you have permission to create repositories in this
#'   `organisation`.
#' @param private If `TRUE`, creates a private repository.
#' @param visibility Only relevant for organisation-owned repos associated with
#'   certain GitHub Enterprise products. The special "internal" `visibility`
#'   grants read permission to all organisation members, i.e. it's intermediate
#'   between "private" and "public", within GHE. When specified, `visibility`
#'   takes precedence over `private = TRUE/FALSE`.
#' @inheritParams git_protocol
#' @param host GitHub host to target, passed to the `.api_url` argument of
#'   [gh::gh()]. If unspecified, gh defaults to "https://api.github.com",
#'   although gh's default can be customised by setting the GITHUB_API_URL
#'   environment variable.
#'
#'   For a hypothetical GitHub Enterprise instance, either
#'   "https://github.acme.com/api/v3" or "https://github.acme.com" is
#'   acceptable.
#'
#' @export
#' @examples
#' \dontrun{
#' pkgpath <- file.path(tempdir(), "testpkg")
#' create_package(pkgpath)
#'
#' ## now, working inside "testpkg", initialize git repository
#' use_git()
#'
#' ## create github repository and configure as git remote
#' use_github()
#' }
use_github <- function(
  organisation = NULL,
  private = FALSE,
  visibility = c("public", "private", "internal"),
  protocol = git_protocol(),
  host = NULL
) {
  visibility_specified <- !missing(visibility)
  visibility <- match.arg(visibility)
  check_protocol(protocol)
  check_uses_git()
  default_branch <- guess_local_default_branch()
  check_current_branch(
    is = default_branch,
    # glue-ing happens inside check_current_branch(), where `gb` gives the
    # current branch
    message = c(
      "x" = "Must be on the default branch {.val {is}}, not {.val {gb}}."
    )
  )
  challenge_uncommitted_changes(
    msg = "
    There are uncommitted changes and we're about to create and push to a new \\
    GitHub repo"
  )
  check_no_origin()

  if (is.null(organisation)) {
    if (visibility_specified) {
      ui_abort(
        "
        The {.arg visibility} setting is only relevant for organisation-owned
        repos, within the context of certain GitHub Enterprise products."
      )
    }
    visibility <- if (private) "private" else "public"
  }

  if (!is.null(organisation) && !visibility_specified) {
    visibility <- if (private) "private" else "public"
  }

  whoami <- suppressMessages(gh::gh_whoami(.api_url = host))
  if (is.null(whoami)) {
    ui_abort(c(
      "x" = "Unable to discover a GitHub personal access token.",
      "i" = "A token is required in order to create and push to a new repo.",
      "_" = "Call {.run usethis::gh_token_help()} for help configuring a token."
    ))
  }
  empirical_host <- parse_github_remotes(glue("{whoami$html_url}/REPO"))$host
  if (empirical_host != "github.com") {
    ui_bullets(c("i" = "Targeting the GitHub host {.val {empirical_host}}."))
  }

  owner <- organisation %||% whoami$login
  repo_name <- project_name()
  check_no_github_repo(owner, repo_name, host)

  repo_desc <- if (is_package()) proj_desc()$get_field("Title") %||% "" else ""
  repo_desc <- gsub("\n", " ", repo_desc)
  repo_spec <- glue("{owner}/{repo_name}")

  visibility_string <- if (visibility == "public") "" else glue("{visibility} ")
  ui_bullets(c(
    "v" = "Creating {visibility_string}GitHub repository {.val {repo_spec}}."
  ))
  if (is.null(organisation)) {
    create <- gh::gh(
      "POST /user/repos",
      name = repo_name,
      description = repo_desc,
      private = private,
      .api_url = host
    )
  } else {
    create <- gh::gh(
      "POST /orgs/{org}/repos",
      org = organisation,
      name = repo_name,
      description = repo_desc,
      visibility = visibility,
      # this is necessary to set `visibility` in GHE 2.22 (but not in 3.2)
      # hopefully it's harmless when not needed
      .accept = "application/vnd.github.nebula-preview+json",
      .api_url = host
    )
  }

  origin_url <- switch(
    protocol,
    https = create$clone_url,
    ssh = create$ssh_url
  )
  withr::defer(view_url(create$html_url))

  ui_bullets(c("v" = "Setting remote {.val origin} to {.val {origin_url}}."))
  use_git_remote("origin", origin_url)

  if (is_package()) {
    # we tryCatch(), because we can't afford any failure here to result in not
    # doing the first push and configuring the default branch
    # such an incomplete setup is hard to diagnose / repair post hoc
    tryCatch(
      use_github_links(),
      error = function(e) NULL
    )
  }

  git_push_first(default_branch, "origin")

  repo <- git_repo()
  gbl <- gert::git_branch_list(local = TRUE, repo = repo)
  if (nrow(gbl) > 1) {
    ui_bullets(c(
      "v" = "Setting {.val {default_branch}} as default branch on GitHub."
    ))
    gh::gh(
      "PATCH /repos/{owner}/{repo}",
      owner = owner,
      repo = repo_name,
      default_branch = default_branch,
      .api_url = host
    )
  }

  invisible()
}

#' Use GitHub links in URL and BugReports
#'
#' @description
#' Populates the `URL` and `BugReports` fields of a GitHub-using R package with
#' appropriate links. The GitHub repo to link to is determined from the current
#' project's GitHub remotes:
#' * If we are not working with a fork, this function expects `origin` to be a
#'   GitHub remote and the links target that repo.
#' * If we are working in a fork, this function expects to find two GitHub
#'   remotes: `origin` (the fork) and `upstream` (the fork's parent) remote. In
#'   an interactive session, the user can confirm which repo to use for the
#'   links. In a noninteractive session, links are formed using `upstream`.
#'
#' @param overwrite By default, `use_github_links()` will not overwrite existing
#'   fields. Set to `TRUE` to overwrite existing links.
#' @export
#' @examples
#' \dontrun{
#' use_github_links()
#' }
#'
use_github_links <- function(overwrite = FALSE) {
  check_is_package("use_github_links()")

  gh_url <- github_url_from_git_remotes()

  proj_desc_field_update("URL", gh_url, overwrite = overwrite, append = TRUE)

  proj_desc_field_update(
    "BugReports",
    glue("{gh_url}/issues"),
    overwrite = overwrite
  )

  git_ask_commit(
    "Add GitHub links to DESCRIPTION",
    untracked = TRUE,
    paths = "DESCRIPTION"
  )

  invisible()
}

has_github_links <- function(target_repo = NULL) {
  url <- if (is.null(target_repo)) NULL else target_repo$url
  github_url <- github_url_from_git_remotes(url)
  if (is.null(github_url)) {
    return(FALSE)
  }

  desc <- proj_desc()

  has_github_url <- github_url %in% desc$get_urls()

  bug_reports <- desc$get_field("BugReports", default = character())
  has_github_issues <- glue("{github_url}/issues") %in% bug_reports

  has_github_url && has_github_issues
}

check_no_origin <- function() {
  remotes <- git_remotes()
  if ("origin" %in% names(remotes)) {
    ui_abort(c(
      "x" = "This repo already has an {.val origin} remote, with value
             {.val {remotes[['origin']]}}.",
      "i" = "You can remove this setting with:",
      " " = '{.code usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)}'
    ))
  }
  invisible()
}

check_no_github_repo <- function(owner, repo, host) {
  spec <- glue("{owner}/{repo}")
  repo_found <- tryCatch(
    {
      repo_info <- gh::gh("/repos/{spec}", spec = spec, .api_url = host)
      # when does repo_info$full_name != the spec we sent?
      # this happens if you reuse the original name of a repo that has since
      # been renamed
      # there's no 404, because of the automatic redirect, but you CAN create
      # a new repo with this name
      # https://github.com/r-lib/usethis/issues/1893
      repo_info$full_name == spec
    },
    "http_error_404" = function(err) FALSE
  )
  if (!repo_found) {
    return(invisible())
  }
  empirical_host <- parse_github_remotes(repo_info$html_url)$host
  ui_abort("Repo {.val {spec}} already exists on {.val {empirical_host}}.")
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/github_token.R ---
#' Get help with GitHub personal access tokens
#'
#' @description

#' A [personal access
#' token](https://docs.github.com/articles/creating-a-personal-access-token-for-the-command-line)
#' (PAT) is needed for certain tasks usethis does via the GitHub API, such as
#' creating a repository, a fork, or a pull request. If you use HTTPS remotes,
#' your PAT is also used when interacting with GitHub as a conventional Git
#' remote. These functions help you get and manage your PAT:

#' * `gh_token_help()` guides you through token troubleshooting and setup.
#' * `create_github_token()` opens a browser window to the GitHub form to
#'   generate a PAT, with suggested scopes pre-selected. It also offers advice
#'   on storing your PAT.
#' * `gitcreds::gitcreds_set()` helps you register your PAT with the Git
#'   credential manager used by your operating system. Later, other packages,
#'   such as usethis, gert, and gh can automatically retrieve that PAT and use
#'   it to work with GitHub on your behalf.
#'
#' Usually, the first time the PAT is retrieved in an R session, it is cached in
#' an environment variable, for easier reuse for the duration of that R session.
#' After initial acquisition and storage, all of this should happen
#' automatically in the background. GitHub is encouraging the use of PATs that
#' expire after, e.g., 30 days, so prepare yourself to re-generate and re-store
#' your PAT periodically.
#'
#' Git/GitHub credential management is covered in a dedicated article: [Managing
#' Git(Hub)
#' Credentials](https://usethis.r-lib.org/articles/articles/git-credentials.html)
#'
#' @details
#' `create_github_token()` has previously gone by some other names:
#' `browse_github_token()` and `browse_github_pat()`.
#'
#' @param scopes Character vector of token scopes, pre-selected in the web form.
#'   Final choices are made in the GitHub form. Read more about GitHub API
#'   scopes at
#'   <https://docs.github.com/apps/building-oauth-apps/understanding-scopes-for-oauth-apps/>.
#' @param description Short description or nickname for the token. You might
#'   (eventually) have multiple tokens on your GitHub account and a label can
#'   help you keep track of what each token is for.
#' @inheritParams use_github
#'
#' @seealso [gh::gh_whoami()] for information on an existing token and
#'   `gitcreds::gitcreds_set()` and `gitcreds::gitcreds_get()` for a secure way
#'   to store and retrieve your PAT.
#'
#' @return Nothing
#' @name github-token
NULL

#' @export
#' @rdname github-token
#' @examples
#' \dontrun{
#' create_github_token()
#' }
create_github_token <- function(
  scopes = c("repo", "user", "gist", "workflow"),
  description = "DESCRIBE THE TOKEN'S USE CASE",
  host = NULL
) {
  scopes <- glue_collapse(scopes, ",")
  host <- get_hosturl(host %||% default_api_url())
  url <- glue(
    "{host}/settings/tokens/new?scopes={scopes}&description={description}"
  )
  withr::defer(view_url(url))

  hint <- code_hint_with_host("gitcreds::gitcreds_set", host)
  message <- c(
    "_" = "Call {.run {hint}} to register this token in the local Git
           credential store."
  )
  if (is_linux()) {
    message <- c(
      message,
      "!" = "On Linux, it can be tricky to store credentials persistently.",
      "i" = "Read more in the {.href ['Managing Git(Hub) Credentials' article](https://usethis.r-lib.org/articles/articles/git-credentials.html)}."
    )
  }
  message <- c(
    message,
    "i" = "It is also a great idea to store this token in any
           password-management software that you use."
  )
  ui_bullets(message)
  invisible()
}

#' @inheritParams use_github
#' @export
#' @rdname github-token
#' @examples
#' \dontrun{
#' gh_token_help()
#' }
gh_token_help <- function(host = NULL) {
  host_url <- get_hosturl(host %||% default_api_url())
  kv_line("GitHub host", host_url)

  pat_sitrep(host_url, scope = "project")
}

code_hint_with_host <- function(function_name, host = NULL, arg_name = NULL) {
  arg_hint <- function(host, arg_name) {
    if (is.null(host) || is_github_dot_com(host)) {
      return("")
    }
    if (is_null(arg_name)) {
      glue('"{host}"')
    } else {
      glue('{arg_name} = "{host}"')
    }
  }

  glue_chr("{function_name}({arg_hint(host, arg_name)})")
}

# workhorse behind gh_token_help() and called, possibly twice, in git_sitrep()
# hence the need for `scold_for_renviron = TRUE/FALSE`
# scope determines if "global" or "de_facto" email is checked
pat_sitrep <- function(
  host = "https://github.com",
  scope = c("user", "project"),
  scold_for_renviron = TRUE
) {
  scope <- rlang::arg_match(scope)

  if (scold_for_renviron) {
    scold_for_renviron()
  }

  maybe_pat <- purrr::safely(gh::gh_token)(api_url = host)
  if (is.null(maybe_pat$result)) {
    ui_bullets(c(
      "x" = "The PAT discovered for {.url {host}} has the wrong structure."
    ))
    ui_bullets(c("i" = maybe_pat$error))
    return(invisible(FALSE))
  }
  pat <- maybe_pat$result
  have_pat <- pat != ""

  if (!have_pat) {
    kv_line("Personal access token for {.val {host}}", NULL)
    hint <- code_hint_with_host("usethis::create_github_token", host, "host")
    ui_bullets(c(
      "_" = "To create a personal access token, call {.run {hint}}."
    ))
    hint <- code_hint_with_host("gitcreds::gitcreds_set", host)
    url <- "https://usethis.r-lib.org/articles/articles/git-credentials.html"
    ui_bullets(c(
      "_" = "To store a token for current and future use, call {.run {hint}}.",
      "i" = "Read more in the {.href [Managing Git(Hub) Credentials]({url})} article."
    ))
    return(invisible(FALSE))
  }
  kv_line("Personal access token for {.val {host}}", ui_special("discovered"))

  online <- is_online(host)
  if (!online) {
    ui_bullets(c(
      "x" = "Host is not reachable.",
      " " = "No further vetting of the personal access token is possible.",
      "_" = "Try again when {.val {host}} can be reached."
    ))
    return(invisible())
  }

  maybe_who <- purrr::safely(gh::gh_whoami)(.token = pat, .api_url = host)
  if (is.null(maybe_who$result)) {
    message <- c("x" = "Can't get user information for this token.")
    if (inherits(maybe_who$error, "http_error_401")) {
      message <- c(
        message,
        "i" = "The token may no longer be valid or perhaps it lacks the
               {.val user} scope."
      )
    }
    message <- c(
      message,
      "i" = maybe_who$error$message
    )
    ui_bullets(message)
    return(invisible(FALSE))
  }
  who <- maybe_who$result

  kv_line("GitHub user", who$login)
  scopes <- strsplit(who$scopes, ", ")[[1]]
  kv_line("Token scopes", scopes)
  scold_for_scopes(scopes)

  maybe_emails <-
    purrr::safely(gh::gh)("/user/emails", .token = pat, .api_url = host)
  if (is.null(maybe_emails$result)) {
    ui_bullets(c(
      "x" = "Can't retrieve registered email addresses from GitHub.",
      "i" = "Consider re-creating your PAT with the {.val user} (or at least
             {.val user:email}) scope."
    ))
  } else {
    emails <- maybe_emails$result
    addresses <- map_chr(
      emails,
      \(x) if (x$primary) glue_data(x, "{email} (primary)") else x[["email"]]
    )
    kv_line("Email(s)", addresses)
    ui_silence(
      user <- git_user_get(where_from_scope(scope))
    )
    git_user_check(user)
    if (!is.null(user$email) && !any(grepl(user$email, addresses))) {
      ui_bullets(c(
        "x" = "Git user's email ({.val {user$email}}) doesn't appear to be
               registered with GitHub host."
      ))
    }
  }

  invisible(TRUE)
}

scold_for_renviron <- function() {
  renviron_path <- scoped_path_r("user", ".Renviron", envvar = "R_ENVIRON_USER")
  if (!file_exists(renviron_path)) {
    return(invisible())
  }

  renviron_lines <- read_utf8(renviron_path)
  fishy_lines <- grep("^GITHUB_(PAT|TOKEN).*=.+", renviron_lines, value = TRUE)
  if (length(fishy_lines) == 0) {
    return(invisible())
  }

  fishy_keys <- re_match(fishy_lines, "^(?<key>.+)=.+")$key
  # TODO: when I switch to cli, this is a good place for `!`
  # in general, lots below is suboptimal, but good enough for now
  ui_bullets(c(
    "!" = "{.path {pth(renviron_path)}} defines{cli::qty(length(fishy_keys))}
           the environment variable{?s}:",
    bulletize(fishy_keys),
    "!" = "This can prevent your PAT from being retrieved from the Git
           credential store.",
    "i" = "If you are troubleshooting PAT problems, the root cause may be an
           old, invalid PAT defined in {.path {pth(renviron_path)}}.",
    "i" = "For most use cases, it is better to NOT define the PAT in
           {.file .Renviron}.",
    "_" = "Call {.run usethis::edit_r_environ()} to edit that file.",
    "_" = "Then call {.run gitcreds::gitcreds_set()} to put the PAT into
           the Git credential store."
  ))
  invisible()
}

scold_for_scopes <- function(scopes) {
  if (length(scopes) == 0) {
    ui_bullets(c(
      "x" = "Token has no scopes!",
      "i" = "Tokens initiated with {.fun create_github_token} default to the
             recommended scopes."
    ))
    return(invisible())
  }

  # https://docs.github.com/en/free-pro-team@latest/developers/apps/scopes-for-oauth-apps
  # why these checks?
  # previous defaults for create_github_token(): repo, gist, user:email
  # more recently: repo, user, gist, workflow
  # (gist scope is a very weak recommendation)
  has_repo <- "repo" %in% scopes
  has_workflow <- "workflow" %in% scopes
  has_user_email <- "user" %in% scopes || "user:email" %in% scopes

  if (has_repo && has_workflow && has_user_email) {
    return(invisible())
  }

  suggestions <- c(
    "*" = if (!has_repo) "{.val repo}: needed to fully access user's repos",
    "*" = if (!has_workflow) {
      "{.val workflow}: needed to manage GitHub Actions workflow files"
    },
    "*" = if (!has_user_email) {
      "{.val user:email}: needed to read user's email addresses"
    }
  )
  message <- c(
    "!" = "Token lacks recommended scopes:",
    suggestions,
    "i" = "Consider re-creating your PAT with the missing scopes.",
    "i" = "Tokens initiated with {.fun usethis::create_github_token} default to the
           recommended scopes."
  )
  ui_bullets(message)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/helpers.R ---
use_dependency <- function(package, type, min_version = NULL) {
  check_name(package)
  check_name(type)

  if (package != "R") {
    check_installed(package)
  }

  if (package == "R" && tolower(type) != "depends") {
    ui_abort('Set {.code type = "Depends"} when specifying an R version.')
  } else if (package == "R" && is.null(min_version)) {
    ui_abort('Specify {.arg min_version} when {.code package = "R"}.')
  }

  if (isTRUE(min_version) && package == "R") {
    min_version <- r_version()
  } else if (isTRUE(min_version)) {
    min_version <- utils::packageVersion(package)
  }
  version <- if (is.null(min_version) || isFALSE(min_version)) {
    "*"
  } else {
    glue(">= {min_version}")
  }

  types <- c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo")
  names(types) <- tolower(types)
  type <- types[[match.arg(tolower(type), names(types))]]

  desc <- proj_desc()
  deps <- desc$get_deps()
  deps <- deps[deps$package == package, ]

  new_linking_to <- type == "LinkingTo" && !"LinkingTo" %in% deps$type
  new_non_linking_to <- type != "LinkingTo" && identical(deps$type, "LinkingTo")

  changed <- FALSE

  # One of:
  # * No existing dependency on this package
  # * Adding existing non-LinkingTo dependency to LinkingTo
  # * First use of a LinkingTo package as a non-LinkingTo dependency
  # In all cases, we can can simply make the change.
  if (nrow(deps) == 0 || new_linking_to || new_non_linking_to) {
    ui_bullets(c(
      "v" = "Adding {.pkg {package}} to {.field {type}} field in DESCRIPTION."
    ))
    desc$set_dep(package, type, version = version)
    desc$write()
    changed <- TRUE
    return(invisible(changed))
  }

  if (type == "LinkingTo") {
    deps <- deps[deps$type == "LinkingTo", ]
  } else {
    deps <- deps[deps$type != "LinkingTo", ]
  }
  existing_type <- deps$type
  existing_version <- deps$version

  delta <- sign(match(existing_type, types) - match(type, types))
  if (delta < 0) {
    # don't downgrade
    ui_bullets(c(
      "!" = "Package {.pkg {package}} is already listed in
             {.field {existing_type}} in DESCRIPTION; no change made."
    ))
  } else if (
    delta == 0 && version_spec(version) != version_spec(existing_version)
  ) {
    if (version_spec(version) > version_spec(existing_version)) {
      direction <- "Increasing"
    } else {
      direction <- "Decreasing"
    }

    ui_bullets(c(
      "v" = "{direction} {.pkg {package}} version to {.val {version}} in
             DESCRIPTION."
    ))
    desc$set_dep(package, type, version = version)
    desc$write()
    changed <- TRUE
  } else if (delta > 0) {
    # moving from, e.g., Suggests to Imports
    ui_bullets(c(
      "v" = "Moving {.pkg {package}} from {.field {existing_type}} to
             {.field {type}} field in DESCRIPTION."
    ))
    desc$del_dep(package, existing_type)
    desc$set_dep(package, type, version = version)
    desc$write()
    changed <- TRUE
  }

  invisible(changed)
}

r_version <- function() {
  version <- getRversion()
  glue("{version$major}.{version$minor}")
}

version_spec <- function(x) {
  if (x == "*") {
    x <- "0"
  }
  x <- gsub("(<=|<|>=|>|==)\\s*", "", x)
  numeric_version(x)
}

view_url <- function(..., open = is_interactive()) {
  url <- paste(..., sep = "/")
  if (open) {
    ui_bullets(c("v" = "Opening URL {.url {url}}."))
    utils::browseURL(url)
  } else {
    ui_bullets(c("_" = "Open URL {.url {url}}."))
  }
  invisible(url)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/ignore.R ---
#' Add files to `.Rbuildignore`
#'
#' @description
#' `.Rbuildignore` has a regular expression on each line, but it's
#' usually easier to work with specific file names. By default,
#' `use_build_ignore()` will (crudely) turn a filename into a regular
#' expression that will only match that path. Repeated entries will be
#' silently removed.
#'
#' `use_build_ignore()` is designed to ignore *individual* files. If you
#' want to ignore *all* files with a given extension, consider providing
#' an "as-is" regular expression, using `escape = FALSE`; see examples.
#'
#' @param files Character vector of path names.
#' @param escape If `TRUE`, the default, will escape `.` to
#'   `\\.` and surround with `^` and `$`.
#' @export
#' @examples
#' \dontrun{
#' # ignore all Excel files
#' use_build_ignore("[.]xlsx$", escape = FALSE)
#' }
use_build_ignore <- function(files, escape = TRUE) {
  if (escape) {
    files <- escape_path(files)
  }

  write_union(proj_path(".Rbuildignore"), files)
}

escape_path <- function(x) {
  x <- gsub("\\.", "\\\\.", x)
  x <- gsub("/$", "", x)
  paste0("^", x, "$")
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/import-standalone-obj-type.R ---
# Standalone file: do not edit by hand
# Source: https://github.com/r-lib/rlang/blob/HEAD/R/standalone-obj-type.R
# Generated by: usethis::use_standalone("r-lib/rlang", "obj-type")
# ----------------------------------------------------------------------
#
# ---
# repo: r-lib/rlang
# file: standalone-obj-type.R
# last-updated: 2024-02-14
# license: https://unlicense.org
# imports: rlang (>= 1.1.0)
# ---
#
# ## Changelog
#
# 2024-02-14:
# - `obj_type_friendly()` now works for S7 objects.
#
# 2023-05-01:
# - `obj_type_friendly()` now only displays the first class of S3 objects.
#
# 2023-03-30:
# - `stop_input_type()` now handles `I()` input literally in `arg`.
#
# 2022-10-04:
# - `obj_type_friendly(value = TRUE)` now shows numeric scalars
#   literally.
# - `stop_friendly_type()` now takes `show_value`, passed to
#   `obj_type_friendly()` as the `value` argument.
#
# 2022-10-03:
# - Added `allow_na` and `allow_null` arguments.
# - `NULL` is now backticked.
# - Better friendly type for infinities and `NaN`.
#
# 2022-09-16:
# - Unprefixed usage of rlang functions with `rlang::` to
#   avoid onLoad issues when called from rlang (#1482).
#
# 2022-08-11:
# - Prefixed usage of rlang functions with `rlang::`.
#
# 2022-06-22:
# - `friendly_type_of()` is now `obj_type_friendly()`.
# - Added `obj_type_oo()`.
#
# 2021-12-20:
# - Added support for scalar values and empty vectors.
# - Added `stop_input_type()`
#
# 2021-06-30:
# - Added support for missing arguments.
#
# 2021-04-19:
# - Added support for matrices and arrays (#141).
# - Added documentation.
# - Added changelog.
#
# nocov start

#' Return English-friendly type
#' @param x Any R object.
#' @param value Whether to describe the value of `x`. Special values
#'   like `NA` or `""` are always described.
#' @param length Whether to mention the length of vectors and lists.
#' @return A string describing the type. Starts with an indefinite
#'   article, e.g. "an integer vector".
#' @noRd
obj_type_friendly <- function(x, value = TRUE) {
  if (is_missing(x)) {
    return("absent")
  }

  if (is.object(x)) {
    if (inherits(x, "quosure")) {
      type <- "quosure"
    } else {
      type <- class(x)[[1L]]
    }
    return(sprintf("a <%s> object", type))
  }

  if (!is_vector(x)) {
    return(.rlang_as_friendly_type(typeof(x)))
  }

  n_dim <- length(dim(x))

  if (!n_dim) {
    if (!is_list(x) && length(x) == 1) {
      if (is_na(x)) {
        return(switch(
          typeof(x),
          logical = "`NA`",
          integer = "an integer `NA`",
          double =
            if (is.nan(x)) {
              "`NaN`"
            } else {
              "a numeric `NA`"
            },
          complex = "a complex `NA`",
          character = "a character `NA`",
          .rlang_stop_unexpected_typeof(x)
        ))
      }

      show_infinites <- function(x) {
        if (x > 0) {
          "`Inf`"
        } else {
          "`-Inf`"
        }
      }
      str_encode <- function(x, width = 30, ...) {
        if (nchar(x) > width) {
          x <- substr(x, 1, width - 3)
          x <- paste0(x, "...")
        }
        encodeString(x, ...)
      }

      if (value) {
        if (is.numeric(x) && is.infinite(x)) {
          return(show_infinites(x))
        }

        if (is.numeric(x) || is.complex(x)) {
          number <- as.character(round(x, 2))
          what <- if (is.complex(x)) "the complex number" else "the number"
          return(paste(what, number))
        }

        return(switch(
          typeof(x),
          logical = if (x) "`TRUE`" else "`FALSE`",
          character = {
            what <- if (nzchar(x)) "the string" else "the empty string"
            paste(what, str_encode(x, quote = "\""))
          },
          raw = paste("the raw value", as.character(x)),
          .rlang_stop_unexpected_typeof(x)
        ))
      }

      return(switch(
        typeof(x),
        logical = "a logical value",
        integer = "an integer",
        double = if (is.infinite(x)) show_infinites(x) else "a number",
        complex = "a complex number",
        character = if (nzchar(x)) "a string" else "\"\"",
        raw = "a raw value",
        .rlang_stop_unexpected_typeof(x)
      ))
    }

    if (length(x) == 0) {
      return(switch(
        typeof(x),
        logical = "an empty logical vector",
        integer = "an empty integer vector",
        double = "an empty numeric vector",
        complex = "an empty complex vector",
        character = "an empty character vector",
        raw = "an empty raw vector",
        list = "an empty list",
        .rlang_stop_unexpected_typeof(x)
      ))
    }
  }

  vec_type_friendly(x)
}

vec_type_friendly <- function(x, length = FALSE) {
  if (!is_vector(x)) {
    abort("`x` must be a vector.")
  }
  type <- typeof(x)
  n_dim <- length(dim(x))

  add_length <- function(type) {
    if (length && !n_dim) {
      paste0(type, sprintf(" of length %s", length(x)))
    } else {
      type
    }
  }

  if (type == "list") {
    if (n_dim < 2) {
      return(add_length("a list"))
    } else if (is.data.frame(x)) {
      return("a data frame")
    } else if (n_dim == 2) {
      return("a list matrix")
    } else {
      return("a list array")
    }
  }

  type <- switch(
    type,
    logical = "a logical %s",
    integer = "an integer %s",
    numeric = ,
    double = "a double %s",
    complex = "a complex %s",
    character = "a character %s",
    raw = "a raw %s",
    type = paste0("a ", type, " %s")
  )

  if (n_dim < 2) {
    kind <- "vector"
  } else if (n_dim == 2) {
    kind <- "matrix"
  } else {
    kind <- "array"
  }
  out <- sprintf(type, kind)

  if (n_dim >= 2) {
    out
  } else {
    add_length(out)
  }
}

.rlang_as_friendly_type <- function(type) {
  switch(
    type,

    list = "a list",

    NULL = "`NULL`",
    environment = "an environment",
    externalptr = "a pointer",
    weakref = "a weak reference",
    S4 = "an S4 object",

    name = ,
    symbol = "a symbol",
    language = "a call",
    pairlist = "a pairlist node",
    expression = "an expression vector",

    char = "an internal string",
    promise = "an internal promise",
    ... = "an internal dots object",
    any = "an internal `any` object",
    bytecode = "an internal bytecode object",

    primitive = ,
    builtin = ,
    special = "a primitive function",
    closure = "a function",

    type
  )
}

.rlang_stop_unexpected_typeof <- function(x, call = caller_env()) {
  abort(
    sprintf("Unexpected type <%s>.", typeof(x)),
    call = call
  )
}

#' Return OO type
#' @param x Any R object.
#' @return One of `"bare"` (for non-OO objects), `"S3"`, `"S4"`,
#'   `"R6"`, or `"S7"`.
#' @noRd
obj_type_oo <- function(x) {
  if (!is.object(x)) {
    return("bare")
  }

  class <- inherits(x, c("R6", "S7_object"), which = TRUE)

  if (class[[1]]) {
    "R6"
  } else if (class[[2]]) {
    "S7"
  } else if (isS4(x)) {
    "S4"
  } else {
    "S3"
  }
}

#' @param x The object type which does not conform to `what`. Its
#'   `obj_type_friendly()` is taken and mentioned in the error message.
#' @param what The friendly expected type as a string. Can be a
#'   character vector of expected types, in which case the error
#'   message mentions all of them in an "or" enumeration.
#' @param show_value Passed to `value` argument of `obj_type_friendly()`.
#' @param ... Arguments passed to [abort()].
#' @inheritParams args_error_context
#' @noRd
stop_input_type <- function(x,
                            what,
                            ...,
                            allow_na = FALSE,
                            allow_null = FALSE,
                            show_value = TRUE,
                            arg = caller_arg(x),
                            call = caller_env()) {
  # From standalone-cli.R
  cli <- env_get_list(
    nms = c("format_arg", "format_code"),
    last = topenv(),
    default = function(x) sprintf("`%s`", x),
    inherit = TRUE
  )

  if (allow_na) {
    what <- c(what, cli$format_code("NA"))
  }
  if (allow_null) {
    what <- c(what, cli$format_code("NULL"))
  }
  if (length(what)) {
    what <- oxford_comma(what)
  }
  if (inherits(arg, "AsIs")) {
    format_arg <- identity
  } else {
    format_arg <- cli$format_arg
  }

  message <- sprintf(
    "%s must be %s, not %s.",
    format_arg(arg),
    what,
    obj_type_friendly(x, value = show_value)
  )

  abort(message, ..., call = call, arg = arg)
}

oxford_comma <- function(chr, sep = ", ", final = "or") {
  n <- length(chr)

  if (n < 2) {
    return(chr)
  }

  head <- chr[seq_len(n - 1)]
  last <- chr[n]

  head <- paste(head, collapse = sep)

  # Write a or b. But a, b, or c.
  if (n > 2) {
    paste0(head, sep, final, " ", last)
  } else {
    paste0(head, " ", final, " ", last)
  }
}

# nocov end


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/import-standalone-types-check.R ---
# Standalone file: do not edit by hand
# Source: https://github.com/r-lib/rlang/blob/HEAD/R/standalone-types-check.R
# Generated by: usethis::use_standalone("r-lib/rlang", "types-check")
# ----------------------------------------------------------------------
#
# ---
# repo: r-lib/rlang
# file: standalone-types-check.R
# last-updated: 2023-03-13
# license: https://unlicense.org
# dependencies: standalone-obj-type.R
# imports: rlang (>= 1.1.0)
# ---
#
# ## Changelog
#
# 2023-03-13:
# - Improved error messages of number checkers (@teunbrand)
# - Added `allow_infinite` argument to `check_number_whole()` (@mgirlich).
# - Added `check_data_frame()` (@mgirlich).
#
# 2023-03-07:
# - Added dependency on rlang (>= 1.1.0).
#
# 2023-02-15:
# - Added `check_logical()`.
#
# - `check_bool()`, `check_number_whole()`, and
#   `check_number_decimal()` are now implemented in C.
#
# - For efficiency, `check_number_whole()` and
#   `check_number_decimal()` now take a `NULL` default for `min` and
#   `max`. This makes it possible to bypass unnecessary type-checking
#   and comparisons in the default case of no bounds checks.
#
# 2022-10-07:
# - `check_number_whole()` and `_decimal()` no longer treat
#   non-numeric types such as factors or dates as numbers.  Numeric
#   types are detected with `is.numeric()`.
#
# 2022-10-04:
# - Added `check_name()` that forbids the empty string.
#   `check_string()` allows the empty string by default.
#
# 2022-09-28:
# - Removed `what` arguments.
# - Added `allow_na` and `allow_null` arguments.
# - Added `allow_decimal` and `allow_infinite` arguments.
# - Improved errors with absent arguments.
#
#
# 2022-09-16:
# - Unprefixed usage of rlang functions with `rlang::` to
#   avoid onLoad issues when called from rlang (#1482).
#
# 2022-08-11:
# - Added changelog.
#
# nocov start

# Scalars -----------------------------------------------------------------

.standalone_types_check_dot_call <- .Call

check_bool <- function(x,
                       ...,
                       allow_na = FALSE,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!missing(x) && .standalone_types_check_dot_call(ffi_standalone_is_bool_1.0.7, x, allow_na, allow_null)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    c("`TRUE`", "`FALSE`"),
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_string <- function(x,
                         ...,
                         allow_empty = TRUE,
                         allow_na = FALSE,
                         allow_null = FALSE,
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (!missing(x)) {
    is_string <- .rlang_check_is_string(
      x,
      allow_empty = allow_empty,
      allow_na = allow_na,
      allow_null = allow_null
    )
    if (is_string) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a single string",
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

.rlang_check_is_string <- function(x,
                                   allow_empty,
                                   allow_na,
                                   allow_null) {
  if (is_string(x)) {
    if (allow_empty || !is_string(x, "")) {
      return(TRUE)
    }
  }

  if (allow_null && is_null(x)) {
    return(TRUE)
  }

  if (allow_na && (identical(x, NA) || identical(x, na_chr))) {
    return(TRUE)
  }

  FALSE
}

check_name <- function(x,
                       ...,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!missing(x)) {
    is_string <- .rlang_check_is_string(
      x,
      allow_empty = FALSE,
      allow_na = FALSE,
      allow_null = allow_null
    )
    if (is_string) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a valid name",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

IS_NUMBER_true <- 0
IS_NUMBER_false <- 1
IS_NUMBER_oob <- 2

check_number_decimal <- function(x,
                                 ...,
                                 min = NULL,
                                 max = NULL,
                                 allow_infinite = TRUE,
                                 allow_na = FALSE,
                                 allow_null = FALSE,
                                 arg = caller_arg(x),
                                 call = caller_env()) {
  if (missing(x)) {
    exit_code <- IS_NUMBER_false
  } else if (0 == (exit_code <- .standalone_types_check_dot_call(
    ffi_standalone_check_number_1.0.7,
    x,
    allow_decimal = TRUE,
    min,
    max,
    allow_infinite,
    allow_na,
    allow_null
  ))) {
    return(invisible(NULL))
  }

  .stop_not_number(
    x,
    ...,
    exit_code = exit_code,
    allow_decimal = TRUE,
    min = min,
    max = max,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_number_whole <- function(x,
                               ...,
                               min = NULL,
                               max = NULL,
                               allow_infinite = FALSE,
                               allow_na = FALSE,
                               allow_null = FALSE,
                               arg = caller_arg(x),
                               call = caller_env()) {
  if (missing(x)) {
    exit_code <- IS_NUMBER_false
  } else if (0 == (exit_code <- .standalone_types_check_dot_call(
    ffi_standalone_check_number_1.0.7,
    x,
    allow_decimal = FALSE,
    min,
    max,
    allow_infinite,
    allow_na,
    allow_null
  ))) {
    return(invisible(NULL))
  }

  .stop_not_number(
    x,
    ...,
    exit_code = exit_code,
    allow_decimal = FALSE,
    min = min,
    max = max,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

.stop_not_number <- function(x,
                             ...,
                             exit_code,
                             allow_decimal,
                             min,
                             max,
                             allow_na,
                             allow_null,
                             arg,
                             call) {
  if (allow_decimal) {
    what <- "a number"
  } else {
    what <- "a whole number"
  }

  if (exit_code == IS_NUMBER_oob) {
    min <- min %||% -Inf
    max <- max %||% Inf

    if (min > -Inf && max < Inf) {
      what <- sprintf("%s between %s and %s", what, min, max)
    } else if (x < min) {
      what <- sprintf("%s larger than or equal to %s", what, min)
    } else if (x > max) {
      what <- sprintf("%s smaller than or equal to %s", what, max)
    } else {
      abort("Unexpected state in OOB check", .internal = TRUE)
    }
  }

  stop_input_type(
    x,
    what,
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_symbol <- function(x,
                         ...,
                         allow_null = FALSE,
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (!missing(x)) {
    if (is_symbol(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a symbol",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_arg <- function(x,
                      ...,
                      allow_null = FALSE,
                      arg = caller_arg(x),
                      call = caller_env()) {
  if (!missing(x)) {
    if (is_symbol(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "an argument name",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_call <- function(x,
                       ...,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!missing(x)) {
    if (is_call(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a defused call",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_environment <- function(x,
                              ...,
                              allow_null = FALSE,
                              arg = caller_arg(x),
                              call = caller_env()) {
  if (!missing(x)) {
    if (is_environment(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "an environment",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_function <- function(x,
                           ...,
                           allow_null = FALSE,
                           arg = caller_arg(x),
                           call = caller_env()) {
  if (!missing(x)) {
    if (is_function(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a function",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_closure <- function(x,
                          ...,
                          allow_null = FALSE,
                          arg = caller_arg(x),
                          call = caller_env()) {
  if (!missing(x)) {
    if (is_closure(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "an R function",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_formula <- function(x,
                          ...,
                          allow_null = FALSE,
                          arg = caller_arg(x),
                          call = caller_env()) {
  if (!missing(x)) {
    if (is_formula(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a formula",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


# Vectors -----------------------------------------------------------------

check_character <- function(x,
                            ...,
                            allow_null = FALSE,
                            arg = caller_arg(x),
                            call = caller_env()) {
  if (!missing(x)) {
    if (is_character(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a character vector",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_logical <- function(x,
                          ...,
                          allow_null = FALSE,
                          arg = caller_arg(x),
                          call = caller_env()) {
  if (!missing(x)) {
    if (is_logical(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a logical vector",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_data_frame <- function(x,
                             ...,
                             allow_null = FALSE,
                             arg = caller_arg(x),
                             call = caller_env()) {
  if (!missing(x)) {
    if (is.data.frame(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a data frame",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

# nocov end


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/issue.R ---
#' Helpers for GitHub issues
#'
#' @description
#' The `issue_*` family of functions allows you to perform common operations on
#' GitHub issues from within R. They're designed to help you efficiently deal
#' with large numbers of issues, particularly motivated by the challenges faced
#' by the tidyverse team.
#'
#' * `issue_close_community()` closes an issue, because it's not a bug report or
#'   feature request, and points the author towards Posit Community as a
#'   better place to discuss usage (<https://forum.posit.co>).
#'
#' * `issue_reprex_needed()` labels the issue with the "reprex" label and
#'   gives the author some advice about what is needed.
#'
#' @section Saved replies:
#'
#' Unlike GitHub's "saved replies", these functions can:
#' * Be shared between people
#' * Perform other actions, like labelling, or closing
#' * Have additional arguments
#' * Include randomness (like friendly gifs)
#'
#' @param number Issue number
#' @param reprex Does the issue also need a reprex?
#'
#' @examples
#' \dontrun{
#' issue_close_community(12, reprex = TRUE)
#'
#' issue_reprex_needed(241)
#' }
#' @name issue-this
NULL

#' @export
#' @rdname issue-this
issue_close_community <- function(number, reprex = FALSE) {
  tr <- target_repo(github_get = TRUE)
  if (!tr$can_push) {
    # https://docs.github.com/en/github/setting-up-and-managing-organizations-and-teams/repository-permission-levels-for-an-organization#repository-access-for-each-permission-level
    # I have not found a way to detect triage permission via API.
    # It seems you just have to try?
    ui_bullets(c(
      "!" = "You don't seem to have push access for {.val {tr$repo_spec}}.",
      "i" = "Unless you have triage permissions, you won't be allowed to close
             an issue."
    ))
    if (ui_nah("Do you want to try anyway?")) {
      ui_bullets(c("x" = "Cancelling."))
      return(invisible())
    }
  }

  info <- issue_info(number, tr)
  issue <- issue_details(info)
  ui_bullets(c(
    "v" = "Closing issue {.val {issue$shorthand}} ({.field {issue$author}}):
           {.val {issue$title}}."
  ))
  if (info$state == "closed") {
    ui_abort("Issue {.val {number}} is already closed.")
  }

  reprex_insert <- glue(
    "
    But before you ask there, I'd suggest that you create a \\
    [reprex](https://reprex.tidyverse.org/articles/reprex-dos-and-donts.htm), \\
    because that greatly increases your chances getting help."
  )

  message <- glue(
    "Hi {issue$author},\n",
    "\n",
    "This issue doesn't appear to be a bug report or a specific feature ",
    "request, so it's more suitable for ",
    "[RStudio Community](https://community.rstudio.com). ",
    if (reprex) reprex_insert else "",
    "\n\n",
    "Thanks!"
  )

  issue_comment_add(number, message = message, tr = tr)
  issue_edit(number, state = "closed", tr = tr)
}

#' @export
#' @rdname issue-this
issue_reprex_needed <- function(number) {
  tr <- target_repo(github_get = TRUE)
  if (!tr$can_push) {
    # https://docs.github.com/en/github/setting-up-and-managing-organizations-and-teams/repository-permission-levels-for-an-organization#repository-access-for-each-permission-level
    # I can't find anyway to detect triage permission via API.
    # It seems you just have to try?
    ui_bullets(c(
      "!" = "You don't seem to have push access for {.val {tr$repo_spec}}.",
      "i" = "Unless you have triage permissions, you won't be allowed to label
             an issue."
    ))
    if (ui_nah("Do you want to try anyway?")) {
      ui_bullets(c("x" = "Cancelling."))
      return(invisible())
    }
  }

  info <- issue_info(number, tr)
  labels <- map_chr(info$labels, "name")
  issue <- issue_details(info)
  if ("reprex" %in% labels) {
    ui_abort("Issue {.val {number}} already has {.val reprex} label.")
  }

  ui_bullets(c(
    "v" = "Labelling and commenting on issue {.val {issue$shorthand}}
           ({.field {issue$author}}): {.val {issue$title}}."
  ))

  message <- glue(
    "
    Can you please provide a minimal reproducible example using the \\
    [reprex](http://reprex.tidyverse.org) package?
    The goal of a reprex is to make it as easy as possible for me to \\
    recreate your problem so that I can fix it.
    If you've never made a minimal reprex before, there is lots of good advice \\
    [here](https://reprex.tidyverse.org/articles/reprex-dos-and-donts.html)."
  )
  issue_comment_add(number, message = message, tr = tr)
  issue_edit(number, labels = as.list(union(labels, "reprex")), tr = tr)
}

# low-level operations ----------------------------------------------------

issue_comment_add <- function(number, message, tr = NULL) {
  issue_gh(
    "POST /repos/{owner}/{repo}/issues/{issue_number}/comments",
    number = number,
    body = message,
    tr = tr
  )
}

issue_edit <- function(number, ..., tr = NULL) {
  issue_gh(
    "PATCH /repos/{owner}/{repo}/issues/{issue_number}",
    ...,
    number = number,
    tr = tr
  )
}

issue_info <- function(number, tr = NULL) {
  issue_gh(
    "GET /repos/{owner}/{repo}/issues/{issue_number}",
    number = number,
    tr = tr
  )
}

# Helpers -----------------------------------------------------------------

# Assumptions:
# * Issue number is called `issue_number`; make sure to tweak `endpoint` if
#   necessary.
# * The user-facing caller should pass information about the target repo,
#   because that is required to vet the GitHub remote config anyway.
#   The fallback to target_repo() is purely for development convenience.
issue_gh <- function(endpoint, ..., number, tr = NULL) {
  tr <- tr %||% target_repo(github_get = NA)
  gh <- gh_tr(tr)
  out <- gh(endpoint, ..., issue_number = number)
  if (substr(endpoint, 1, 4) == "GET ") {
    out
  } else {
    invisible(out)
  }
}

issue_details <- function(info) {
  repo_dat <- parse_github_remotes(info$html_url)
  list(
    shorthand = glue(
      "{repo_dat$repo_owner}/{repo_dat$repo_name}/#{info$number}"
    ),
    author = glue("@{info$user$login}"),
    title = info$title
  )
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/jenkins.R ---
#' Create Jenkinsfile for Jenkins CI Pipelines
#'
#' `use_jenkins()` adds a basic Jenkinsfile for R packages to the project root
#' directory. The Jenkinsfile stages take advantage of calls to `make`, and so
#' calling this function will also run `use_make()` if a Makefile does not
#' already exist at the project root.
#'
#' @seealso The [documentation on Jenkins
#'   Pipelines](https://www.jenkins.io/doc/book/pipeline/jenkinsfile/).
#' @seealso [use_make()]
#' @export
use_jenkins <- function() {
  use_make()
  use_template(
    "Jenkinsfile",
    data = list(name = project_name())
  )
  use_build_ignore("Jenkinsfile")
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/latest-dependencies.R ---
#' Use "latest" versions of all dependencies
#'
#' Pins minimum versions of all `Imports` and `Depends` dependencies to latest
#' ones (as determined by `source`). Useful for the tidyverse package, but
#' should otherwise be used with extreme care.
#'
#' @keywords internal
#' @export
#' @param overwrite By default (`TRUE`), all dependencies will be modified.
#'   Set to `FALSE` to only modify dependencies without version
#'   specifications.
#' @param source Use "CRAN" or "local" package versions.
use_latest_dependencies <- function(
  overwrite = TRUE,
  source = c("CRAN", "local")
) {
  source <- arg_match(source)

  desc <- proj_desc()
  updated <- update_versions(
    desc$get_deps(),
    overwrite = overwrite,
    source = source
  )

  desc$set_deps(updated)
  desc$write()

  invisible(TRUE)
}

update_versions <- function(
  deps,
  overwrite = TRUE,
  source = c("CRAN", "local")
) {
  baserec <- base_and_recommended()
  to_change <- !deps$package %in% c("R", baserec) & deps$type != "Suggests"
  if (!overwrite) {
    to_change <- to_change & deps$version == "*"
  }

  packages <- deps$package[to_change]
  versions <- switch(
    match.arg(source),
    local = map_chr(packages, \(x) as.character(utils::packageVersion(x))),
    CRAN = utils::available.packages()[packages, "Version"]
  )
  deps$version[to_change] <- paste0(">= ", versions)

  deps
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/license.R ---
#' License a package
#'
#' @description
#' Adds the necessary infrastructure to declare your package as licensed
#' with one of these popular open source licenses:
#'
#' Permissive:
#' * [MIT](https://choosealicense.com/licenses/mit/): simple and permissive.
#' * [Apache 2.0](https://choosealicense.com/licenses/apache-2.0/): MIT +
#'   provides patent protection.
#'
#' Copyleft:
#' * [GPL v2](https://choosealicense.com/licenses/gpl-2.0/): requires sharing
#'   of improvements.
#' * [GPL v3](https://choosealicense.com/licenses/gpl-3.0/): requires sharing
#'   of improvements.
#' * [AGPL v3](https://choosealicense.com/licenses/agpl-3.0/): requires sharing
#'   of improvements.
#' * [LGPL v2.1](https://choosealicense.com/licenses/lgpl-2.1/): requires sharing
#'   of improvements.
#' * [LGPL v3](https://choosealicense.com/licenses/lgpl-3.0/): requires sharing
#'   of improvements.
#'
#' Creative commons licenses appropriate for data packages:
#' * [CC0](https://creativecommons.org/publicdomain/zero/1.0/): dedicated
#'   to public domain.
#' * [CC-BY](https://creativecommons.org/licenses/by/4.0/): Free to share and
#'    adapt, must give appropriate credit.
#'
#' See <https://choosealicense.com> for more details and other options.
#'
#' Alternatively, for code that you don't want to share with others,
#' `use_proprietary_license()` makes it clear that all rights are reserved,
#' and the code is not open source.
#'
#' @details
#' CRAN does not permit you to include copies of standard licenses in your
#' package, so these functions save the license as `LICENSE.md` and add it
#' to `.Rbuildignore`.
#'
#' @name licenses
#' @param copyright_holder Name of the copyright holder or holders. This
#'   defaults to `"{package name} authors"`; you should only change this if you
#'   use a CLA to assign copyright to a single entity.
#' @param version License version. This defaults to latest version all licenses.
#' @param include_future If `TRUE`, will license your package under the current
#'   and any potential future versions of the license. This is generally
#'   considered to be good practice because it means your package will
#'   automatically include "bug" fixes in licenses.
#' @seealso For more details, refer to the the
#'   [license chapter](https://r-pkgs.org/license.html) in _R Packages_.
#' @aliases NULL
NULL

#' @rdname licenses
#' @export
use_mit_license <- function(copyright_holder = NULL) {
  data <- list(
    year = format(Sys.Date(), "%Y"),
    copyright_holder = copyright_holder %||% glue("{project_name()} authors")
  )

  if (is_package()) {
    proj_desc_field_update("License", "MIT + file LICENSE", overwrite = TRUE)
    use_template("year-copyright.txt", save_as = "LICENSE", data = data)
  }

  use_license_template("mit", data)
}

#' @rdname licenses
#' @export
use_gpl_license <- function(version = 3, include_future = TRUE) {
  version <- check_license_version(version, 2:3)

  if (is_package()) {
    abbr <- license_abbr("GPL", version, include_future)
    proj_desc_field_update("License", abbr, overwrite = TRUE)
  }
  use_license_template(glue("GPL-{version}"))
}

#' @rdname licenses
#' @export
use_agpl_license <- function(version = 3, include_future = TRUE) {
  version <- check_license_version(version, 3)

  if (is_package()) {
    abbr <- license_abbr("AGPL", version, include_future)
    proj_desc_field_update("License", abbr, overwrite = TRUE)
  }
  use_license_template(glue("AGPL-{version}"))
}

#' @rdname licenses
#' @export
use_lgpl_license <- function(version = 3, include_future = TRUE) {
  version <- check_license_version(version, c(2.1, 3))
  if (is_package()) {
    abbr <- license_abbr("LGPL", version, include_future)
    proj_desc_field_update("License", abbr, overwrite = TRUE)
  }
  use_license_template(glue("LGPL-{version}"))
}

#' @rdname licenses
#' @export
use_apache_license <- function(version = 2, include_future = TRUE) {
  version <- check_license_version(version, 2)

  if (is_package()) {
    abbr <- license_abbr("Apache License", version, include_future)
    proj_desc_field_update("License", abbr, overwrite = TRUE)
  }
  use_license_template(glue("apache-{version}"))
}

#' @rdname licenses
#' @export
use_cc0_license <- function() {
  if (is_package()) {
    proj_desc_field_update("License", "CC0", overwrite = TRUE)
  }
  use_license_template("cc0")
}

#' @rdname licenses
#' @export
use_ccby_license <- function() {
  if (is_package()) {
    proj_desc_field_update("License", "CC BY 4.0", overwrite = TRUE)
  }
  use_license_template("ccby-4")
}

#' @rdname licenses
#' @export
use_proprietary_license <- function(copyright_holder) {
  data <- list(
    year = year(),
    copyright_holder = copyright_holder
  )

  if (is_package()) {
    proj_desc_field_update("License", "file LICENSE", overwrite = TRUE)
  }
  use_template("license-proprietary.txt", save_as = "LICENSE", data = data)
}

# Fallbacks ---------------------------------------------------------------

#' @rdname licenses
#' @export
#' @usage NULL
use_gpl3_license <- function() {
  use_gpl_license(3)
}

#' @rdname licenses
#' @export
#' @usage NULL
use_agpl3_license <- function() {
  use_agpl_license(3)
}

#' @rdname licenses
#' @export
#' @usage NULL
use_apl2_license <- function() {
  use_apache_license(2)
}

# Helpers -----------------------------------------------------------------

use_license_template <- function(license, data = list()) {
  license_template <- glue("license-{license}.md")

  use_template(
    license_template,
    save_as = "LICENSE.md",
    data = data,
    ignore = TRUE
  )
}

check_license_version <- function(version, possible) {
  version <- as.double(version)

  if (!version %in% possible) {
    ui_abort("{.arg version} must be {.or {possible}}.")
  }

  version
}

license_abbr <- function(name, version, include_future) {
  if (include_future) {
    glue_chr("{name} (>= {version})")
  } else {
    if (name %in% c("GPL", "LGPL", "AGPL")) {
      # Standard abbreviations listed at
      # https://cran.rstudio.com/doc/manuals/r-devel/R-exts.html#Licensing
      glue_chr("{name}-{version}")
    } else {
      glue_chr("{name} (== {version})")
    }
  }
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/lifecycle.R ---
#' Use lifecycle badges
#'
#' @description
#' This helper:
#'
#' * Adds lifecycle as a dependency.
#' * Imports [lifecycle::deprecated()] for use in function arguments.
#' * Copies the lifecycle badges into `man/figures`.
#' * Reminds you how to use the badge syntax.
#'
#' Learn more at <https://lifecycle.r-lib.org/articles/communicate.html>
#'
#' @seealso [use_lifecycle_badge()] to signal the
#'  [lifecycle stage](https://lifecycle.r-lib.org/articles/stages.html) of
#'  your package as whole
#' @export
use_lifecycle <- function() {
  check_is_package("use_lifecycle()")
  check_uses_roxygen("use_lifecycle()")
  if (!uses_roxygen_md()) {
    ui_abort(
      "
      Turn on roxygen2 markdown support with {.run usethis::use_roxygen_md()},
      then try again."
    )
  }

  use_package("lifecycle")
  use_import_from("lifecycle", "deprecated")

  dest_dir <- proj_path("man", "figures")
  create_directory(dest_dir)

  templ_dir <- path_package("usethis", "templates")
  templ_files <- dir_ls(templ_dir, glob = "*/lifecycle-*.svg")

  purrr::walk(templ_files, file_copy, dest_dir, overwrite = TRUE)
  ui_bullets(c(
    "v" = "Copied SVG badges to {.path {pth(dest_dir)}}.",
    "_" = "Add badges in documentation topics by inserting a line like this:",
    " " = "#' `r lifecycle::badge('experimental')`",
    " " = "#' `r lifecycle::badge('superseded')`",
    " " = "#' `r lifecycle::badge('deprecated')`"
  ))

  invisible(TRUE)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/line-ending.R ---
proj_line_ending <- function() {
  # First look in .Rproj file
  proj_path <- proj_path(paste0(project_name(), ".Rproj"))
  if (file_exists(proj_path)) {
    config <- read_utf8(proj_path)

    if (any(grepl("^LineEndingConversion: Posix", config))) {
      return("\n")
    } else if (any(grepl("^LineEndingConversion: Windows", config))) {
      return("\r\n")
    }
  }

  # Then try DESCRIPTION
  desc_path <- proj_path("DESCRIPTION")
  if (file_exists(desc_path)) {
    return(detect_line_ending(desc_path))
  }

  # Then try any .R file
  r_path <- proj_path("R")
  if (dir_exists(r_path)) {
    r_files <- dir_ls(r_path, regexp = "[.][rR]$")
    if (length(r_files) > 0) {
      return(detect_line_ending(r_files[[1]]))
    }
  }

  # Then give up - this is used (for example), when writing the
  # first file into the package
  platform_line_ending()
}

platform_line_ending <- function() {
  if (.Platform$OS.type == "windows") "\r\n" else "\n"
}

detect_line_ending <- function(path) {
  samp <- suppressWarnings(readChar(path, nchars = 500))
  if (isTRUE(grepl("\r\n", samp))) "\r\n" else "\n"
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/logo.R ---
#' Use a package logo
#'
#' This function helps you use a logo in your package:
#'   * Enforces a specific size
#'   * Stores logo image file at `man/figures/logo.png`
#'   * Produces the markdown text you need in README to include the logo
#'
#' @param img The path to an existing image file
#' @param geometry a [magick::geometry] string specifying size. The default
#'   assumes that you have a hex logo using spec from
#'   <http://hexb.in/sticker.html>.
#' @param retina `TRUE`, the default, scales the image on the README,
#'   assuming that geometry is double the desired size.
#'
#' @examples
#' \dontrun{
#' use_logo("usethis.png")
#' }
#' @export
use_logo <- function(img, geometry = "240x278", retina = TRUE) {
  check_is_package("use_logo()")

  ext <- tolower(path_ext(img))
  logo_path <- proj_path("man", "figures", "logo", ext = ext)
  create_directory(path_dir(logo_path))
  if (!can_overwrite(logo_path)) {
    return(invisible(FALSE))
  }

  if (ext == "svg") {
    logo_path <- path("man", "figures", "logo.svg")
    file_copy(img, proj_path(logo_path), overwrite = TRUE)
    ui_bullets(c("v" = "Copied {.path {pth(img)}} to {.path {logo_path}}."))

    height <- as.integer(sub(".*x", "", geometry))
  } else {
    check_installed("magick")

    img_data <- magick::image_read(img)
    img_data <- magick::image_resize(img_data, geometry)
    magick::image_write(img_data, logo_path)
    ui_bullets(c("v" = "Resized {.path {pth(img)}} to {geometry}."))

    height <- magick::image_info(magick::image_read(logo_path))$height
  }

  pkg <- project_name()
  if (retina) {
    height <- round(height / 2)
  }

  # Have a clickable hyperlink to jump to README if exists.
  readme_path <- find_readme()
  if (is.null(readme_path)) {
    readme_show <- "your README"
  } else {
    readme_show <- cli::format_inline("{.path {pth(readme_path)}}")
  }

  ui_bullets(c("_" = "Add logo to {readme_show} with the following html:"))
  pd_link <- pkgdown_url(pedantic = TRUE)
  if (is.null(pd_link)) {
    ui_code_snippet(
      "# {pkg} <img src=\"{proj_rel_path(logo_path)}\" align=\"right\" height=\"{height}\" alt=\"\" />",
      language = ""
    )
  } else {
    ui_code_snippet(
      "# {pkg} <a href=\"{pd_link}\"><img src=\"{proj_rel_path(logo_path)}\" align=\"right\" height=\"{height}\" alt=\"{pkg} website\" /></a>",
      language = ""
    )
  }
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/make.R ---
#' Create Makefile
#'
#' `use_make()` adds a basic Makefile to the project root directory.
#'
#' @seealso The [documentation for GNU
#'   Make](https://www.gnu.org/software/make/manual/html_node/).
#' @export
use_make <- function() {
  use_template(
    "Makefile",
    data = list(name = project_name())
  )
  use_build_ignore("Makefile")
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/namespace.R ---
#' Use a basic `NAMESPACE`
#'
#' If `roxygen` is `TRUE` generates an empty `NAMESPACE` that exports nothing;
#' you'll need to explicitly export functions with `@export`. If `roxygen`
#' is `FALSE`, generates a default `NAMESPACE` that exports all functions
#' except those that start with `.`.
#'
#' @param roxygen Do you plan to manage `NAMESPACE` with roxygen2?
#' @seealso The [namespace
#'   chapter](https://r-pkgs.org/dependencies-mindset-background.html#sec-dependencies-namespace)
#'   of [R Packages](https://r-pkgs.org).
#' @export
use_namespace <- function(roxygen = TRUE) {
  check_is_package("use_namespace()")

  path <- proj_path("NAMESPACE")
  if (roxygen) {
    write_over(path, c("# Generated by roxygen2: do not edit by hand", ""))
  } else {
    write_over(path, 'exportPattern("^[^\\\\.]")')
  }
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/news.R ---
#' Create a simple `NEWS.md`
#'
#' This creates a basic `NEWS.md` in the root directory.
#'
#' @inheritParams use_template
#' @seealso The [other markdown files
#'   section](https://r-pkgs.org/other-markdown.html) of [R
#'   Packages](https://r-pkgs.org).
#' @export
use_news_md <- function(open = rlang::is_interactive()) {
  check_is_package("use_news_md()")

  version <- if (is_dev_version()) "(development version)" else proj_version()

  on_cran <- !is.null(cran_version())

  if (on_cran) {
    init_bullet <- "Added a `NEWS.md` file to track changes to the package."
  } else {
    init_bullet <- "Initial CRAN submission."
  }

  use_template(
    "NEWS.md",
    data = list(
      Package = project_name(),
      Version = version,
      InitialBullet = init_bullet
    ),
    open = open
  )

  git_ask_commit("Add NEWS.md", untracked = TRUE, paths = "NEWS.md")
}

use_news_heading <- function(version) {
  news_path <- proj_path("NEWS.md")
  if (!file_exists(news_path)) {
    return(invisible())
  }

  news <- read_utf8(news_path)
  idx <- match(TRUE, grepl("[^[:space:]]", news))

  if (is.na(idx)) {
    return(news)
  }

  title <- glue("# {project_name()} {version}")
  if (title == news[[idx]]) {
    return(invisible())
  }

  development_title <- glue("# {project_name()} (development version)")
  if (development_title == news[[idx]]) {
    news[[idx]] <- title

    ui_bullets(c("v" = "Replacing development heading in {.path NEWS.md}."))
    return(write_utf8(news_path, news))
  }

  ui_bullets(c("v" = "Adding new heading to {.path NEWS.md}."))
  write_utf8(news_path, c(title, "", news))
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/package.R ---
#' Depend on another package
#'
#' @description

#' `use_package()` adds a CRAN package dependency to `DESCRIPTION` and offers a
#' little advice about how to best use it. `use_dev_package()` adds a dependency
#' on an in-development package, adding the dev repo to `Remotes` so it will be
#' automatically installed from the correct location. There is no helper to
#' remove a dependency: to do that, simply remove that package from your
#' `DESCRIPTION` file.
#'
#' `use_package()` exists to support a couple of common maneuvers:
#' * Add a dependency to `Imports` or `Suggests` or `LinkingTo`.
#' * Add a minimum version to a dependency.
#' * Specify the minimum supported version for R.
#'
#' `use_package()` probably works for slightly more exotic modifications, but at
#' some point, you should edit `DESCRIPTION` yourself by hand. There is no
#' intention to account for all possible edge cases.
#'
#' @param package Name of package to depend on.
#' @param type Type of dependency: must be one of "Imports", "Depends",
#'   "Suggests", "Enhances", or "LinkingTo" (or unique abbreviation). Matching
#'   is case insensitive.
#' @param min_version Optionally, supply a minimum version for the package. Set
#'   to `TRUE` to use the currently installed version or use a version string
#'   suitable for [numeric_version()], such as "2.5.0".
#' @param remote By default, an `OWNER/REPO` GitHub remote is inserted.
#'   Optionally, you can supply a character string to specify the remote, e.g.
#'   `"gitlab::jimhester/covr"`, using any syntax supported by the [remotes
#'   package](
#'   https://remotes.r-lib.org/articles/dependencies.html#other-sources).
#'
#' @seealso The [dependencies section](https://r-pkgs.org/dependencies-mindset-background.html) of
#'   [R Packages](https://r-pkgs.org).
#'
#' @export
#' @examples
#' \dontrun{
#' use_package("ggplot2")
#' use_package("dplyr", "suggests")
#' use_dev_package("glue")
#'
#' # Depend on R version 4.1
#' use_package("R", type = "Depends", min_version = "4.1")
#' }
use_package <- function(package, type = "Imports", min_version = NULL) {
  if (type == "Imports") {
    refuse_package(package, verboten = c("tidyverse", "tidymodels"))
  }

  changed <- use_dependency(package, type, min_version = min_version)
  if (changed) {
    how_to_use(package, type)
  }

  invisible()
}

#' @export
#' @rdname use_package
use_dev_package <- function(package, type = "Imports", remote = NULL) {
  refuse_package(package, verboten = c("tidyverse", "tidymodels"))

  changed <- use_dependency(package, type = type, min_version = TRUE)
  use_remote(package, remote)
  if (changed) {
    how_to_use(package, type)
  }

  invisible()
}

use_remote <- function(package, package_remote = NULL) {
  desc <- proj_desc()

  remotes <- desc$get_remotes()
  if (any(grepl(package, remotes))) {
    return(invisible())
  }

  if (is.null(package_remote)) {
    package_desc <- desc::desc(package = package)
    package_remote <- package_remote(package_desc)
  }

  ui_bullets(c(
    "v" = "Adding {.val {package_remote}} to {.field Remotes} field in
           DESCRIPTION."
  ))
  remotes <- c(remotes, package_remote)

  desc$set_remotes(remotes)
  desc$write()

  invisible()
}

# Helpers -----------------------------------------------------------------

package_remote <- function(desc) {
  remote <- as.list(desc$get(c("RemoteType", "RemoteUsername", "RemoteRepo")))

  is_recognized_remote <- all(map_lgl(remote, \(x) is_string(x) && !is.na(x)))

  if (is_recognized_remote) {
    # non-GitHub remotes get a 'RemoteType::' prefix
    if (!identical(remote$RemoteType, "github")) {
      remote$RemoteUsername <- paste0(
        remote$RemoteType,
        "::",
        remote$RemoteUsername
      )
    }
    return(paste0(remote$RemoteUsername, "/", remote$RemoteRepo))
  }

  package <- desc$get_field("Package")
  urls <- desc_urls(package, desc = desc)
  urls <- urls[urls$is_github, ]
  if (nrow(urls) < 1) {
    ui_abort("Cannot determine remote for {.pkg {package}}.")
  }
  parsed <- parse_github_remotes(urls$url[[1]])
  remote <- paste0(parsed$repo_owner, "/", parsed$repo_name)
  if (
    ui_yep(c(
      "!" = "{.pkg {package}} was either installed from CRAN or local source.",
      "i" = "Based on DESCRIPTION, we propose the remote: {.val {remote}}.",
      " " = "Is this OK?"
    ))
  ) {
    remote
  } else {
    ui_abort("Cannot determine remote for {.pkg {package}}.")
  }
}

refuse_package <- function(package, verboten) {
  if (package %in% verboten) {
    code <- glue('use_package("{package}", type = "depends")')
    ui_abort(c(
      "x" = "{.pkg {package}} is a meta-package and it is rarely a good idea to
             depend on it.",
      "_" = "Please determine the specific underlying package(s) that provide
             the function(s) you need and depend on that instead.",
      "i" = "For data analysis projects that use a package structure but do not
             implement a formal R package, adding {.pkg {package}} to
             {.field Depends} is a reasonable compromise.",
      "_" = "Call {.code {code}} to achieve this."
    ))
  }
  invisible(package)
}

how_to_use <- function(package, type) {
  types <- tolower(c("Imports", "Depends", "Suggests", "Enhances", "LinkingTo"))
  type <- match.arg(tolower(type), types)
  if (package == "R" && type == "depends") {
    return("")
  }

  switch(
    type,
    imports = ui_bullets(c(
      "_" = "Refer to functions with {.code {paste0(package, '::fun()')}}."
    )),
    depends = ui_bullets(c(
      "!" = "Are you sure you want {.field Depends}?
             {.field Imports} is almost always the better choice."
    )),
    suggests = suggests_usage_hint(package),
    enhances = "",
    linkingto = show_includes(package)
  )
}

suggests_usage_hint <- function(package) {
  imports_rlang <- proj_desc()$has_dep("rlang", type = "Imports")
  if (imports_rlang) {
    code1 <- glue('rlang::is_installed("{package}")')
    code2 <- glue('rlang::check_installed("{package}")')
    ui_bullets(c(
      "_" = "In your package code, use {.code {code1}} or {.code {code2}} to
             test if {.pkg {package}} is installed."
    ))
    code <- glue("{package}::fun()")
    ui_bullets(c("_" = "Then directly refer to functions with {.code {code}}."))
  } else {
    code <- glue('requireNamespace("{package}", quietly = TRUE)')
    ui_bullets(c(
      "_" = "Use {.code {code}} to test if {.pkg {package}} is installed."
    ))
    code <- glue("{package}::fun()")
    ui_bullets(c("_" = "Then directly refer to functions with {.code {code}}."))
  }
}

show_includes <- function(package) {
  incl <- path_package("include", package = package)
  h <- dir_ls(incl, regexp = "[.](h|hpp)$")
  if (length(h) == 0) {
    return()
  }

  ui_bullets(c("Possible includes are:"))
  ui_code_snippet("#include <{path_file(h)}>", copy = FALSE, language = "")
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/pipe.R ---
#' Use magrittr's pipe in your package
#'
#' Does setup necessary to use magrittr's pipe operator, `%>%` in your package.
#' This function requires the use of \pkg{roxygen2}.
#' * Adds magrittr to "Imports" in `DESCRIPTION`.
#' * Imports the pipe operator specifically, which is necessary for internal
#'   use.
#' * Exports the pipe operator, if `export = TRUE`, which is necessary to make
#'   `%>%` available to the users of your package.
#'
#' @param export If `TRUE`, the file `R/utils-pipe.R` is added, which provides
#' the roxygen template to import and re-export `%>%`. If `FALSE`, the necessary
#' roxygen directive is added, if possible, or otherwise instructions are given.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' use_pipe()
#' }
use_pipe <- function(export = TRUE) {
  check_is_package("use_pipe()")
  check_uses_roxygen("use_pipe()")

  if (export) {
    use_dependency("magrittr", "Imports")
    use_template("pipe.R", "R/utils-pipe.R") && roxygen_remind()
    return(invisible(TRUE))
  }

  use_import_from("magrittr", "%>%")
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/pkgdown.R ---
#' Use pkgdown
#'
#' @description
#' [pkgdown](https://pkgdown.r-lib.org) makes it easy to turn your package into
#' a beautiful website. usethis provides two functions to help you use pkgdown:
#'
#' * `use_pkgdown()`: creates a pkgdown config file and adds relevant files or
#'   directories to `.Rbuildignore` and `.gitignore`.
#'
#' * `use_pkgdown_github_pages()`: implements the GitHub setup needed to
#'   automatically publish your pkgdown site to GitHub pages:
#'
#'   - (first, it calls `use_pkgdown()`)
#'   - [use_github_pages()] prepares to publish the pkgdown site from the
#'     `gh-pages` branch
#'   - [`use_github_action("pkgdown")`][use_github_action()] configures a
#'     GitHub Action to automatically build the pkgdown site and deploy it via
#'     GitHub Pages
#'   - The pkgdown site's URL is added to the pkgdown configuration file,
#'     to the URL field of DESCRIPTION, and to the GitHub repo.
#'   - Packages owned by certain GitHub organizations (tidyverse, r-lib, and
#'     tidymodels) get some special treatment, in terms of anticipating the
#'     (eventual) site URL and the use of a pkgdown template.
#'
#' @seealso <https://pkgdown.r-lib.org/articles/pkgdown.html#configuration>
#' @param config_file Path to the pkgdown yaml config file, relative to the
#'  project.
#' @param destdir Target directory for pkgdown docs.
#' @export
use_pkgdown <- function(config_file = "_pkgdown.yml", destdir = "docs") {
  check_is_package("use_pkgdown()")
  check_installed("pkgdown")

  use_build_ignore(c(config_file, destdir, "pkgdown"))
  use_git_ignore(destdir)

  config <- pkgdown_config(destdir)
  config_path <- proj_path(config_file)
  write_over(config_path, yaml::as.yaml(config))
  edit_file(config_path)

  invisible(TRUE)
}

pkgdown_config <- function(destdir) {
  config <- list(
    url = NULL
  )

  if (pkgdown_version() >= "1.9000") {
    config$template <- list(bootstrap = 5L)
  }

  if (!identical(destdir, "docs")) {
    config$destination <- destdir
  }

  config
}

# wrapping because I need to be able to mock this in tests
pkgdown_version <- function() {
  utils::packageVersion("pkgdown")
}

#' @rdname use_pkgdown
#' @export
use_pkgdown_github_pages <- function() {
  tr <- target_repo(github_get = TRUE, ok_configs = c("ours", "fork"))
  check_can_push(tr = tr, "to turn on GitHub Pages")

  use_pkgdown()
  site <- use_github_pages()
  use_github_action("pkgdown")

  site_url <- tidyverse_url(url = site$html_url, tr = tr)
  use_pkgdown_url(url = site_url, tr = tr)

  if (is_posit_pkg()) {
    proj_desc_field_update(
      "Config/Needs/website",
      "tidyverse/tidytemplate",
      append = TRUE
    )
  }
}

# helpers ----------------------------------------------------------------------
use_pkgdown_url <- function(url, tr = NULL) {
  tr <- tr %||% target_repo(github_get = TRUE)

  config_path <- pkgdown_config_path()
  ui_bullets(c(
    "v" = "Recording {.url {url}} as site's {.field url} in
           {.path {pth(config_path)}}."
  ))
  config <- pkgdown_config_meta()
  if (has_name(config, "url")) {
    config$url <- url
  } else {
    config <- c(url = url, config)
  }
  write_utf8(config_path, yaml::as.yaml(config))

  proj_desc_field_update("URL", url, append = TRUE)
  if (has_package_doc()) {
    ui_bullets(c(
      "_" = "Run {.run devtools::document()} to update package-level documentation."
    ))
  }

  gh <- gh_tr(tr)
  homepage <- gh("GET /repos/{owner}/{repo}")[["homepage"]]
  if (is.null(homepage) || homepage != url) {
    ui_bullets(c(
      "v" = "Setting {.url {url}} as homepage of GitHub repo {.val {tr$repo_spec}}."
    ))
    gh("PATCH /repos/{owner}/{repo}", homepage = url)
  }

  invisible()
}

tidyverse_url <- function(url, tr = NULL) {
  tr <- tr %||% target_repo(github_get = TRUE)
  if (
    !is_interactive() ||
      !tr$repo_owner %in% c("tidyverse", "r-lib", "tidymodels")
  ) {
    return(url)
  }

  custom_url <- glue("https://{tr$repo_name}.{tr$repo_owner}.org")
  if (grepl(glue("{custom_url}/?"), url)) {
    return(url)
  }
  if (
    ui_yep(c(
      "i" = "{.val {tr$repo_name}} is owned by the {.val {tr$repo_owner}} GitHub
           organization.",
      " " = "Shall we configure {.val {custom_url}} as the (eventual) pkgdown URL?"
    ))
  ) {
    custom_url
  } else {
    url
  }
}

pkgdown_config_path <- function() {
  path_first_existing(
    proj_path(
      c(
        "_pkgdown.yml",
        "_pkgdown.yaml",
        "pkgdown/_pkgdown.yml",
        "pkgdown/_pkgdown.yaml",
        "inst/_pkgdown.yml",
        "inst/_pkgdown.yaml"
      )
    )
  )
}

uses_pkgdown <- function() {
  !is.null(pkgdown_config_path())
}

uses_pkgdown_bootstrap_version <- function(version = 5) {
  config <- pkgdown_config_meta()
  identical(
    purrr::pluck(config, "template", "bootstrap"),
    as.integer(version)
  )
}

pkgdown_config_meta <- function() {
  if (!uses_pkgdown()) {
    return(list())
  }
  path <- pkgdown_config_path()
  yaml::read_yaml(path) %||% list()
}

pkgdown_url <- function(pedantic = FALSE) {
  if (!uses_pkgdown()) {
    return(NULL)
  }

  meta <- pkgdown_config_meta()
  url <- meta$url
  if (!is.null(url)) {
    return(url)
  }

  if (pedantic) {
    ui_bullets(c(
      "!" = "{.pkg pkgdown} config does not specify the site's {.field url},
             which is optional but recommended."
    ))
  }
  NULL
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/positron.R ---
is_positron <- function() {
  Sys.getenv("POSITRON") == "1"
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/pr.R ---
#' Helpers for GitHub pull requests
#'
#' @description
#' The `pr_*` family of functions is designed to make working with GitHub pull
#' requests (PRs) as painless as possible for both contributors and package
#' maintainers.
#'
#' To use the `pr_*` functions, your project must be a Git repo and have one of
#' these GitHub remote configurations:
#' * "ours": You can push to the GitHub remote configured as `origin` and it's
#'   not a fork.
#' * "fork": You can push to the GitHub remote configured as `origin`, it's a
#'   fork, and its parent is configured as `upstream`. `origin` points to your
#'   **personal** copy and `upstream` points to the **source repo**.
#'
#' "Ours" and "fork" are two of several GitHub remote configurations examined in
#' [Common remote setups](https://happygitwithr.com/common-remote-setups.html)
#' in Happy Git and GitHub for the useR.
#'
#' The [Pull Request
#' Helpers](https://usethis.r-lib.org/articles/articles/pr-functions.html)
#' article walks through the process of making a pull request with the `pr_*`
#' functions.
#'

#' The `pr_*` functions also use your Git/GitHub credentials to carry out
#' various remote operations; see below for more about auth. The `pr_*`
#' functions also proactively check for agreement re: the default branch in your
#' local repo and the source repo. See [git_default_branch()] for more.
#'

#' @template double-auth
#'

#' @section For contributors:

#' To contribute to a package, first use `create_from_github("OWNER/REPO")`.
#' This forks the source repository and checks out a local copy.

#'
#' Next use `pr_init()` to create a branch for your PR. It is best practice to
#' never make commits to the default branch branch of a fork (usually named
#' `main` or `master`), because you do not own it. A pull request should always
#' come from a feature branch. It will be much easier to pull upstream changes
#' from the fork parent if you only allow yourself to work in feature branches.
#' It is also much easier for a maintainer to explore and extend your PR if you
#' create a feature branch.
#'
#' Work locally, in your branch, making changes to files, and committing your
#' work. Once you're ready to create the PR, run `pr_push()` to push your local
#' branch to GitHub, and open a webpage that lets you initiate the PR (or draft
#' PR).
#'
#' To learn more about the process of making a pull request, read the [Pull
#' Request
#' Helpers](https://usethis.r-lib.org/articles/articles/pr-functions.html)
#' vignette.
#'
#' If you are lucky, your PR will be perfect, and the maintainer will accept it.
#' You can then run `pr_finish()` to delete your PR branch. In most cases,
#' however, the maintainer will ask you to make some changes. Make the changes,
#' then run `pr_push()` to update your PR.
#'
#' It's also possible that the maintainer will contribute some code to your PR:
#' to get those changes back onto your computer, run `pr_pull()`. It can also
#' happen that other changes have occurred in the package since you first
#' created your PR. You might need to merge the default branch (usually named
#' `main` or `master`) into your PR branch. Do that by running
#' `pr_merge_main()`: this makes sure that your PR is compatible with the
#' primary repo's main line of development. Both `pr_pull()` and
#' `pr_merge_main()` can result in merge conflicts, so be prepared to resolve
#' before continuing.
#'
#' @section For maintainers:
#' To download a PR locally so that you can experiment with it, run
#' `pr_fetch()` and select the PR or, if you already know its number, call
#' `pr_fetch(<pr_number>)`. If you make changes, run `pr_push()` to push them
#' back to GitHub. After you have merged the PR, run `pr_finish()` to delete the
#' local branch and remove the remote associated with the contributor's fork.
#'
#' @section Overview of all the functions:

#' * `pr_init()`: As a contributor, start work on a new PR by ensuring that
#' your local repo is up-to-date, then creating and checking out a new branch.
#' Nothing is pushed to or created on GitHub until you call `pr_push()`.

#' * `pr_fetch()`: As a maintainer, review or contribute changes to an existing
#' PR by creating a local branch that tracks the remote PR. `pr_fetch()` does as
#' little work as possible, so you can also use it to resume work on an PR that
#' already has a local branch (where it will also ensure your local branch is
#' up-to-date). If called with no arguments, up to 9 open PRs are offered for
#' interactive selection.

#' * `pr_resume()`: Resume work on a PR by switching to an existing local branch
#' and pulling any changes from its upstream tracking branch, if it has one. If
#' called with no arguments, up to 9 local branches are offered for interactive
#' selection, with a preference for branches connected to PRs and for branches
#' with recent activity.

#' * `pr_push()`: The first time it's called, a PR branch is pushed to GitHub
#' and you're taken to a webpage where a new PR (or draft PR) can be created.
#' This also sets up the local branch to track its remote counterpart.
#' Subsequent calls to `pr_push()` make sure the local branch has all the remote
#' changes and, if so, pushes local changes, thereby updating the PR.

#' * `pr_pull()`: Pulls changes from the local branch's remote tracking branch.
#' If a maintainer has extended your PR, this is how you bring those changes
#' back into your local work.

#' * `pr_merge_main()`: Pulls changes from the default branch of the source repo
#' into the current local branch. This can be used when the local branch is the
#' default branch or when it's a PR branch.

#' * `pr_pause()`: Makes sure you're up-to-date with any remote changes in the
#' PR. Then switches back to the default branch and pulls from the source repo.
#' Use `pr_resume()` with name of branch or use `pr_fetch()` to resume using PR
#' number.

#' * `pr_view()`: Visits the PR associated with the current branch in the
#' browser (default) or the specific PR identified by `number`.
#' (FYI [browse_github_pulls()] is a handy way to visit the list of all PRs for
#' the current project.)

#' * `pr_forget()`: Does local clean up when the current branch is an actual or
#' notional PR that you want to abandon. Maybe you initiated it yourself, via
#' `pr_init()`, or you used `pr_fetch()` to explore a PR from GitHub. Only does
#' *local* operations: does not update or delete any remote branches, nor does
#' it close any PRs. Alerts the user to any uncommitted or unpushed work that is
#' at risk of being lost. If user chooses to proceed, switches back to the
#' default branch, pulls changes from source repo, and deletes local PR branch.
#' Any associated Git remote is deleted, if the "forgotten" PR was the only
#' branch using it.

#' * `pr_finish()`: Does post-PR clean up, but does NOT actually merge or close
#' a PR (maintainer should do this in the browser). If `number` is not given,
#' infers the PR from the upstream tracking branch of the current branch. If
#' `number` is given, it does not matter whether the PR exists locally. If PR
#' exists locally, alerts the user to uncommitted or unpushed changes, then
#' switches back to the default branch, pulls changes from source repo, and
#' deletes local PR branch. If the PR came from an external fork, any associated
#' Git remote is deleted, provided it's not in use by any other local branches.
#' If the PR has been merged and user has permission, deletes the remote branch
#' (this is the only remote operation that `pr_finish()` potentially does).
#'
#' @name pull-requests
NULL

#' @export
#' @rdname pull-requests
#' @param branch Name of a new or existing local branch. If creating a new
#'   branch, note this should usually consist of lower case letters, numbers,
#'   and `-`.
pr_init <- function(branch) {
  check_string(branch)
  repo <- git_repo()

  if (gert::git_branch_exists(branch, local = TRUE, repo = repo)) {
    code <- glue('pr_resume("{branch}")')
    ui_bullets(c(
      "i" = "Branch {.val {branch}} already exists, calling {.code {code}}."
    ))
    return(pr_resume(branch))
  }

  # don't absolutely require PAT success, because we could be offline
  # or in another salvageable situation, e.g. need to configure PAT
  cfg <- github_remote_config(github_get = NA)
  check_for_bad_config(cfg)
  tr <- target_repo(cfg, ask = FALSE)
  online <- is_online(tr$host)

  if (!online) {
    ui_bullets(c(
      "x" = "You are not currently online.",
      "i" = "You can still create a local branch, but we can't check that your
             current branch is up-to-date or setup the remote branch."
    ))
    if (ui_nah("Do you want to continue?")) {
      ui_bullets(c("x" = "Cancelling."))
      return(invisible())
    }
  } else {
    maybe_good_configs <- c("maybe_ours_or_theirs", "maybe_fork")
    if (cfg$type %in% maybe_good_configs) {
      ui_bullets(c(
        "x" = 'Unable to confirm the GitHub remote configuration is
               "pull request ready".',
        "i" = "You probably need to configure a personal access token for
               {.val {tr$host}}.",
        "i" = "See {.run usethis::gh_token_help()} for help with that."
      ))
      if (ui_github_remote_config_wat(cfg)) {
        ui_bullets(c("x" = "Cancelling."))
        return(invisible())
      }
    }
  }

  default_branch <- if (online) {
    git_default_branch_(cfg)
  } else {
    guess_local_default_branch()
  }
  challenge_non_default_branch(
    "Are you sure you want to create a PR branch based on a non-default branch?",
    default_branch = default_branch
  )

  if (online) {
    # this is not pr_pull_source_override() because:
    # a) we may NOT be on default branch (although we probably are)
    # b) we didn't just switch to the branch we're on, therefore we have to
    #    consider that the pull may be affected by uncommitted changes or a
    #    merge
    current_branch <- git_branch()
    if (current_branch == default_branch) {
      # override for mis-configured forks, that have default branch tracking
      # the fork (origin) instead of the source (upstream)
      remref <- glue("{tr$remote}/{default_branch}")
    } else {
      remref <- git_branch_tracking(current_branch)
    }
    if (!is.na(remref)) {
      comparison <- git_branch_compare(current_branch, remref)
      if (comparison$remote_only > 0) {
        challenge_uncommitted_changes()
      }
      ui_bullets(c("v" = "Pulling changes from {.val {remref}}."))
      git_pull(remref = remref, verbose = FALSE)
    }
  }

  ui_bullets(c("v" = "Creating and switching to local branch {.val {branch}}."))
  gert::git_branch_create(branch, repo = repo)
  config_key <- glue("branch.{branch}.created-by")
  gert::git_config_set(config_key, value = "usethis::pr_init", repo = repo)

  ui_bullets(c("_" = "Use {.run usethis::pr_push()} to create a PR."))
  invisible()
}

#' @export
#' @rdname pull-requests
pr_resume <- function(branch = NULL) {
  repo <- git_repo()

  if (is.null(branch)) {
    ui_bullets(c(
      "i" = "No branch specified ... looking up local branches and associated PRs."
    ))
    default_branch <- guess_local_default_branch()
    branch <- choose_branch(exclude = default_branch)
    if (is.null(branch)) {
      ui_bullets(c("x" = "Repo doesn't seem to have any non-default branches."))
      return(invisible())
    }
    if (length(branch) == 0) {
      ui_bullets(c("x" = "No branch selected, exiting."))
      return(invisible())
    }
  }
  check_string(branch)

  if (!gert::git_branch_exists(branch, local = TRUE, repo = repo)) {
    code <- glue('usethis::pr_init("{branch}")')
    ui_abort(c(
      "x" = "No branch named {.val {branch}} exists.",
      "_" = "Call {.run {code}} to create a new PR branch."
    ))
  }

  challenge_uncommitted_changes()

  ui_bullets(c("v" = "Switching to branch {.val {branch}}."))
  gert::git_branch_checkout(branch, repo = repo)
  git_pull()

  ui_bullets(c("_" = "Use {.run usethis::pr_push()} to create or update PR."))
  invisible()
}

#' @export
#' @rdname pull-requests
#' @param number Number of PR.
#' @param target Which repo to target? This is only a question in the case of a
#'   fork. In a fork, there is some slim chance that you want to consider pull
#'   requests against your fork (the primary repo, i.e. `origin`) instead of
#'   those against the source repo (i.e. `upstream`, which is the default).
#'
#' @examples
#' \dontrun{
#' pr_fetch(123)
#' }
pr_fetch <- function(number = NULL, target = c("source", "primary")) {
  repo <- git_repo()
  tr <- target_repo(github_get = NA, role = target, ask = FALSE)
  challenge_uncommitted_changes()

  if (is.null(number)) {
    ui_bullets(c("i" = "No PR specified ... looking up open PRs."))
    pr <- choose_pr(tr = tr)
    if (is.null(pr)) {
      ui_bullets(c("x" = "No open PRs found for {.val {tr$repo_spec}}."))
      return(invisible())
    }
    if (min(lengths(pr)) == 0) {
      ui_bullets(c("x" = "No PR selected, exiting."))
      return(invisible())
    }
  } else {
    pr <- pr_get(number = number, tr = tr)
  }

  if (is.na(pr$pr_repo_owner)) {
    ui_abort(
      "
      The repo or branch where {.href [PR #{pr$pr_number}]({pr$pr_html_url})} originates seems to have been
      deleted."
    )
  }

  pr_user <- glue("@{pr$pr_user}")
  ui_bullets(c(
    "v" = "Checking out PR {.href [{pr$pr_string}]({pr$pr_html_url})} ({.field {pr_user}}):
           {.val {pr$pr_title}}."
  ))

  if (pr$pr_from_fork && isFALSE(pr$maintainer_can_modify)) {
    ui_bullets(c(
      "!" = "Note that user does NOT allow maintainer to modify this PR at this
             time, although this can be changed."
    ))
  }

  remote <- github_remote_list(pr$pr_remote)
  if (nrow(remote) == 0) {
    url <- switch(tr$protocol, https = pr$pr_https_url, ssh = pr$pr_ssh_url)
    ui_bullets(c("v" = "Adding remote {.val {pr$pr_remote}} as {.val {url}}."))
    gert::git_remote_add(url = url, name = pr$pr_remote, repo = repo)
    config_key <- glue("remote.{pr$pr_remote}.created-by")
    gert::git_config_set(config_key, "usethis::pr_fetch", repo = repo)
  }
  pr_remref <- glue_data(pr, "{pr_remote}/{pr_ref}")
  gert::git_fetch(
    remote = pr$pr_remote,
    refspec = pr$pr_ref,
    repo = repo,
    verbose = FALSE
  )

  if (is.na(pr$pr_local_branch)) {
    pr$pr_local_branch <-
      if (pr$pr_from_fork) sub(":", "-", pr$pr_label) else pr$pr_ref
  }

  # Create local branch, if necessary, and switch to it ----
  if (!gert::git_branch_exists(pr$pr_local_branch, local = TRUE, repo = repo)) {
    ui_bullets(c(
      "v" = "Creating and switching to local branch {.val {pr$pr_local_branch}}.",
      "v" = "Setting {.val {pr_remref}} as remote tracking branch."
    ))
    gert::git_branch_create(pr$pr_local_branch, ref = pr_remref, repo = repo)
    config_key <- glue("branch.{pr$pr_local_branch}.created-by")
    gert::git_config_set(config_key, "usethis::pr_fetch", repo = repo)
    config_url <- glue("branch.{pr$pr_local_branch}.pr-url")
    gert::git_config_set(config_url, pr$pr_html_url, repo = repo)
    return(invisible())
  }

  # Local branch pre-existed; make sure tracking branch is set, switch, & pull
  ui_bullets(c("v" = "Switching to branch {.val {pr$pr_local_branch}}."))
  gert::git_branch_checkout(pr$pr_local_branch, repo = repo)
  config_url <- glue("branch.{pr$pr_local_branch}.pr-url")
  gert::git_config_set(config_url, pr$pr_html_url, repo = repo)

  pr_branch_ours_tracking <- git_branch_tracking(pr$pr_local_branch)
  if (
    is.na(pr_branch_ours_tracking) ||
      pr_branch_ours_tracking != pr_remref
  ) {
    ui_bullets(c("v" = "Setting {.val {pr_remref}} as remote tracking branch."))
    gert::git_branch_set_upstream(pr_remref, repo = repo)
  }
  git_pull(verbose = FALSE)
}

#' @export
#' @rdname pull-requests
pr_push <- function() {
  repo <- git_repo()
  cfg <- github_remote_config(github_get = TRUE)
  check_for_config(cfg, ok_configs = c("ours", "fork"))
  default_branch <- git_default_branch_(cfg)
  check_pr_branch(default_branch)
  challenge_uncommitted_changes()

  branch <- git_branch()
  remref <- git_branch_tracking(branch)
  if (is.na(remref)) {
    # this is the first push
    if (cfg$type == "fork" && cfg$upstream$can_push && is_interactive()) {
      choices <- c(
        origin = ui_pre_glue(
          "
          <<cfg$origin$repo_spec>> = {.val origin} (external PR)"
        ),
        upstream = ui_pre_glue(
          "
          <<cfg$upstream$repo_spec>> = {.val upstream} (internal PR)"
        )
      )
      choices_formatted <- map_chr(choices, cli::format_inline)
      title <- glue("Which repo do you want to push to?")
      choice <- utils::menu(choices_formatted, graphics = FALSE, title = title)
      remote <- names(choices)[[choice]]
    } else {
      remote <- "origin"
    }

    git_push_first(branch, remote)
  } else {
    check_branch_pulled(use = "pr_pull()")
    git_push(branch, remref)
  }

  # Prompt to create PR if does not exist yet
  tr <- target_repo(cfg, ask = FALSE)
  pr <- pr_find(branch, tr = tr)
  if (is.null(pr)) {
    pr_create()
  } else {
    ui_bullets(c(
      "_" = "View PR at {.url {pr$pr_html_url}} or call {.run usethis::pr_view()}."
    ))
  }

  invisible()
}

#' @export
#' @rdname pull-requests
pr_pull <- function() {
  cfg <- github_remote_config(github_get = TRUE)
  check_for_config(cfg)
  default_branch <- git_default_branch_(cfg)
  check_pr_branch(default_branch)
  challenge_uncommitted_changes()

  git_pull()

  # note associated PR in git config, if applicable
  tr <- target_repo(cfg, ask = FALSE)
  pr_find(tr = tr)

  invisible(TRUE)
}

#' @export
#' @rdname pull-requests
pr_merge_main <- function() {
  tr <- target_repo(github_get = TRUE, ask = FALSE)
  challenge_uncommitted_changes()
  remref <- glue("{tr$remote}/{tr$default_branch}")
  ui_bullets(c("v" = "Pulling changes from {.val {remref}}."))
  git_pull(remref, verbose = FALSE)
}

#' @export
#' @rdname pull-requests
pr_view <- function(number = NULL, target = c("source", "primary")) {
  cfg <- github_remote_config(github_get = NA)
  tr <- target_repo(cfg, github_get = NA, role = target, ask = FALSE)
  url <- NULL
  if (is.null(number)) {
    branch <- git_branch()
    default_branch <- git_default_branch_(cfg)
    if (branch != default_branch) {
      url <- pr_url(branch = branch, tr = tr)
      if (is.null(url)) {
        ui_bullets(c(
          "i" = "Current branch ({.val {branch}}) does not appear to be
                 connected to a PR."
        ))
      } else {
        number <- sub("^.+pull/", "", url)
        ui_bullets(c(
          "i" = "Current branch ({.val {branch}}) is connected to PR #{number}."
        ))
      }
    }
  } else {
    pr <- pr_get(number = number, tr = tr)
    url <- pr$pr_html_url
  }
  if (is.null(url)) {
    ui_bullets(c("i" = "No PR specified ... looking up open PRs."))
    pr <- choose_pr(tr = tr)
    if (is.null(pr)) {
      ui_bullets(c("x" = "No open PRs found for {.val {tr$repo_spec}}."))
      return(invisible())
    }
    if (min(lengths(pr)) == 0) {
      ui_bullets(c("x" = "No PR selected, exiting."))
      return(invisible())
    }
    url <- pr$pr_html_url
  }
  view_url(url)
}

#' @export
#' @rdname pull-requests
pr_pause <- function() {
  cfg <- github_remote_config(github_get = NA)
  tr <- target_repo(cfg, github_get = NA, ask = FALSE)

  ui_bullets(c("v" = "Switching back to the default branch."))
  default_branch <- git_default_branch_(cfg)
  if (git_branch() == default_branch) {
    ui_bullets(c(
      "!" = "Already on this repo's default branch ({.val {default_branch}}),
             nothing to do."
    ))
    return(invisible())
  }
  challenge_uncommitted_changes()
  # TODO: what happens here if offline?
  check_branch_pulled(use = "pr_pull()")

  ui_bullets(c(
    "v" = "Switching back to default branch ({.val {default_branch}})."
  ))
  gert::git_branch_checkout(default_branch, repo = git_repo())
  pr_pull_source_override(tr = tr, default_branch = default_branch)
}

#' @export
#' @rdname pull-requests
pr_finish <- function(number = NULL, target = c("source", "primary")) {
  pr_clean(number = number, target = target, mode = "finish")
}

#' @export
#' @rdname pull-requests
pr_forget <- function() pr_clean(mode = "forget")

# unexported helpers ----

# Removes local evidence of PRs that you're done with or wish you'd never
# started or fetched
# Only possible remote action is to delete the remote branch for a merged PR
pr_clean <- function(
  number = NULL,
  target = c("source", "primary"),
  mode = c("finish", "forget")
) {
  withr::defer(rstudio_git_tickle())
  mode <- match.arg(mode)
  repo <- git_repo()

  cfg <- github_remote_config(github_get = NA)
  tr <- target_repo(cfg, github_get = NA, role = target, ask = FALSE)
  default_branch <- git_default_branch_(cfg)

  if (is.null(number)) {
    check_pr_branch(default_branch)
    pr <- pr_find(git_branch(), tr = tr, state = "all")
    # if the remote branch has already been deleted (probably post-merge), we
    # can't always reverse engineer what the corresponding local branch was, but
    # we already know it -- it's how we found the PR in the first place!
    if (!is.null(pr)) {
      pr$pr_local_branch <- pr$pr_local_branch %|% git_branch()
    }
  } else {
    pr <- pr_get(number = number, tr = tr)
  }

  if (!is.null(pr)) {
    ing <- switch(mode, finish = "Finishing", forget = "Forgetting")
    ui_bullets(c(
      "i" = "{ing} PR {.href [{pr$pr_string}]({pr$pr_html_url})}"
    ))
  }

  pr_local_branch <- if (is.null(pr)) git_branch() else pr$pr_local_branch

  if (!is.na(pr_local_branch)) {
    if (pr_local_branch == git_branch()) {
      challenge_uncommitted_changes()
    }
    tracking_branch <- git_branch_tracking(pr_local_branch)
    if (is.na(tracking_branch)) {
      if (
        ui_nah(c(
          "!" = "Local branch {.val {pr_local_branch}} has no associated remote
               branch.",
          "i" = "If we delete {.val {pr_local_branch}}, any work that exists only
               on this branch may be hard for you to recover.",
          " " = "Proceed anyway?"
        ))
      ) {
        ui_bullets(c("x" = "Cancelling."))
        return(invisible())
      }
    } else {
      cmp <- git_branch_compare(
        branch = pr_local_branch,
        remref = tracking_branch
      )
      if (
        cmp$local_only > 0 &&
          ui_nah(c(
            "!" = "Local branch {.val {pr_local_branch}} has 1 or more commits that
               have not been pushed to {.val {tracking_branch}}.",
            "i" = "If we delete {.val {pr_local_branch}}, this work may be hard for
               you to recover.",
            " " = "Proceed anyway?"
          ))
      ) {
        ui_bullets(c("x" = "Cancelling."))
        return(invisible())
      }
    }
  }

  if (git_branch() != default_branch) {
    ui_bullets(c(
      "v" = "Switching back to default branch ({.val {default_branch}})."
    ))
    gert::git_branch_checkout(default_branch, force = TRUE, repo = repo)
    pr_pull_source_override(tr = tr, default_branch = default_branch)
  }

  if (!is.na(pr_local_branch)) {
    ui_bullets(c(
      "v" = "Deleting local {.val {pr_local_branch}} branch."
    ))
    tryCatch(
      gert::git_branch_delete(pr_local_branch, repo = repo),
      libgit2_error = function(e) {
        # The expected error doesn't have a distinctive class, so we have to
        # detect it based on the message.
        # If we get an unexpected libgit2 error, rethrow.
        if (
          !grepl(
            "could not find key 'branch[.].+[.](vscode-merge-base|github-pr-owner-number|github-pr-base-branch)' to delete",
            e$message
          )
        ) {
          stop(e)
        }
      }
    )
  }

  if (is.null(pr)) {
    return(invisible())
  }

  if (mode == "finish") {
    pr_branch_delete(pr)
  }

  # delete remote, if we (usethis) added it AND no remaining tracking branches
  created_by <- git_cfg_get(glue("remote.{pr$pr_remote}.created-by"))
  if (is.null(created_by) || !grepl("^usethis::", created_by)) {
    return(invisible())
  }

  branches <- gert::git_branch_list(local = TRUE, repo = repo)
  branches <- branches[!is.na(branches$upstream), ]
  if (
    sum(grepl(glue("^refs/remotes/{pr$pr_remote}"), branches$upstream)) == 0
  ) {
    ui_bullets(c("v" = "Removing remote {.val {pr$pr_remote}}."))
    gert::git_remote_remove(remote = pr$pr_remote, repo = repo)
  }
  invisible()
}

# Make sure to pull from upstream/DEFAULT (as opposed to origin/DEFAULT) if
# we're in DEFAULT branch of a fork. I wish everyone set up DEFAULT to track the
# DEFAULT branch in the source repo, but this protects us against sub-optimal
# setup.
pr_pull_source_override <- function(tr, default_branch) {
  # TODO: why does this not use a check_*() function, i.e. shared helper?
  # I guess to issue a specific error message?
  current_branch <- git_branch()
  if (current_branch != default_branch) {
    ui_abort(
      "
      Internal error: {.fun pr_pull_source_override} should only be used when on
      default branch."
    )
  }

  # guard against mis-configured forks, that have default branch tracking
  # the fork (origin) instead of the source (upstream)
  # TODO: should I just change the upstream tracking branch, i.e. fix it?
  remref <- glue("{tr$remote}/{default_branch}")
  if (is_online(tr$host)) {
    ui_bullets(c("v" = "Pulling changes from {.val {remref}}."))
    git_pull(remref = remref, verbose = FALSE)
  } else {
    ui_bullets(c(
      "!" = "Can't reach {.val {tr$host}}, therefore unable to pull changes from
             {.val {remref}}."
    ))
  }
}

pr_create <- function() {
  branch <- git_branch()
  tracking_branch <- git_branch_tracking(branch)
  remote <- remref_remote(tracking_branch)
  remote_dat <- github_remotes(remote, github_get = FALSE)
  ui_bullets(c("_" = "Create PR at link given below."))
  view_url(glue_data(remote_dat, "{host_url}/{repo_spec}/compare/{branch}"))
}

# retrieves 1 PR, if:
# * we can establish a tracking relationship between `branch` and a PR branch
# * we can get the user to choose 1
pr_find <- function(
  branch = git_branch(),
  tr = NULL,
  state = c("open", "closed", "all")
) {
  # Have we done this before? Check if we've cached pr-url in git config.
  config_url <- glue("branch.{branch}.pr-url")
  url <- git_cfg_get(config_url, where = "local")
  if (!is.null(url)) {
    return(pr_get(number = sub("^.+pull/", "", url), tr = tr))
  }

  tracking_branch <- git_branch_tracking(branch)
  if (is.na(tracking_branch)) {
    return(NULL)
  }

  state <- match.arg(state)
  remote <- remref_remote(tracking_branch)
  remote_dat <- github_remotes(remote)

  pr_head <- glue("{remote_dat$repo_owner}:{remref_branch(tracking_branch)}")
  pr_dat <- pr_list(tr = tr, state = state, head = pr_head)
  if (nrow(pr_dat) == 0) {
    return(NULL)
  }
  if (nrow(pr_dat) > 1) {
    spec <- sub(":", "/", pr_head)
    ui_bullets(c("!" = "Multiple PRs are associated with {.val {spec}}."))
    pr_dat <- choose_pr(pr_dat = pr_dat)
    if (min(lengths(pr_dat)) == 0) {
      ui_abort(
        "
        One of these PRs must be specified explicitly or interactively: \\
        {.or {paste0('#', pr_dat$pr_number)}}."
      )
    }
  }

  gert::git_config_set(config_url, pr_dat$pr_html_url, repo = git_repo())
  as.list(pr_dat)
}

pr_url <- function(
  branch = git_branch(),
  tr = NULL,
  state = c("open", "closed", "all")
) {
  state <- match.arg(state)
  pr <- pr_find(branch, tr = tr, state = state)
  if (is.null(pr)) {
    NULL
  } else {
    pr$pr_html_url
  }
}

pr_data_tidy <- function(pr) {
  out <- list(
    pr_number = pluck_int(pr, "number"),
    pr_title = pluck_chr(pr, "title"),
    pr_state = pluck_chr(pr, "state"),
    pr_user = pluck_chr(pr, "user", "login"),
    pr_created_at = pluck_chr(pr, "created_at"),
    pr_updated_at = pluck_chr(pr, "updated_at"),
    pr_merged_at = pluck_chr(pr, "merged_at"),
    pr_label = pluck_chr(pr, "head", "label"),
    # the 'repo' element of 'head' is NULL when fork has been deleted
    pr_repo_owner = pluck_chr(pr, "head", "repo", "owner", "login"),
    pr_ref = pluck_chr(pr, "head", "ref"),
    pr_repo_spec = pluck_chr(pr, "head", "repo", "full_name"),
    pr_from_fork = pluck_lgl(pr, "head", "repo", "fork"),
    # 'maintainer_can_modify' is only present when we GET one specific PR
    pr_maintainer_can_modify = pluck_lgl(pr, "maintainer_can_modify"),
    pr_https_url = pluck_chr(pr, "head", "repo", "clone_url"),
    pr_ssh_url = pluck_chr(pr, "head", "repo", "ssh_url"),
    pr_html_url = pluck_chr(pr, "html_url"),
    pr_string = glue(
      "
      {pluck_chr(pr, 'base', 'repo', 'full_name')}/#{pluck_int(pr, 'number')}"
    )
  )

  grl <- github_remote_list(these = NULL)
  m <- match(out$pr_repo_spec, grl$repo_spec)
  out$pr_remote <- if (is.na(m)) out$pr_repo_owner else grl$remote[m]

  pr_remref <- glue("{out$pr_remote}/{out$pr_ref}")
  gbl <- gert::git_branch_list(local = TRUE, repo = git_repo())
  gbl <- gbl[!is.na(gbl$upstream), c("name", "upstream")]
  gbl$upstream <- sub("^refs/remotes/", "", gbl$upstream)
  m <- match(pr_remref, gbl$upstream)
  out$pr_local_branch <- if (is.na(m)) NA_character_ else gbl$name[m]

  # If the fork has been deleted, these are all NA
  # - Because pr$head$repo is NULL:
  #   pr_repo_owner, pr_repo_spec, pr_from_fork, pr_https_url, pr_ssh_url
  # - Because derived from those above:
  #   pr_remote, pr_remref pr_local_branch
  # I suppose one could already have a local branch, if you fetched the PR
  # before the fork got deleted.
  # But an initial pr_fetch() won't work if the fork has been deleted.
  # I'm willing to accept that the pr_*() functions don't necessarily address
  # the "deleted fork" scenario. It's relatively rare.
  # example: https://github.com/r-lib/httr/pull/634

  out
}

pr_list <- function(
  tr = NULL,
  github_get = NA,
  state = c("open", "closed", "all"),
  head = NULL
) {
  tr <- tr %||% target_repo(github_get = github_get, ask = FALSE)
  state <- match.arg(state)
  gh <- gh_tr(tr)
  safely_gh <- purrr::safely(gh, otherwise = NULL)
  out <- safely_gh(
    "GET /repos/{owner}/{repo}/pulls",
    state = state,
    head = head,
    .limit = Inf
  )
  if (is.null(out$error)) {
    prs <- out$result
  } else {
    ui_bullets(c("x" = "Unable to retrieve PRs for {.value {tr$repo_spec}}."))
    prs <- NULL
  }
  no_prs <- length(prs) == 0
  if (no_prs) {
    prs <- list(list())
  }
  out <- map(prs, pr_data_tidy)
  out <- map(out, \(x) as.data.frame(x, stringsAsFactors = FALSE))
  out <- do.call(rbind, out)
  if (no_prs) {
    out[0, ]
  } else {
    pr_is_open <- out$pr_state == "open"
    rbind(out[pr_is_open, ], out[!pr_is_open, ])
  }
}

# retrieves specific PR by number
pr_get <- function(number, tr = NULL, github_get = NA) {
  tr <- tr %||% target_repo(github_get = github_get, ask = FALSE)
  gh <- gh_tr(tr)
  raw <- gh("GET /repos/{owner}/{repo}/pulls/{number}", number = number)
  pr_data_tidy(raw)
}

branches_with_no_upstream_or_github_upstream <- function(tr = NULL) {
  repo <- git_repo()
  gb_dat <- gert::git_branch_list(local = TRUE, repo = repo)
  gb_dat <- gb_dat[, c("name", "upstream", "updated")]
  gb_dat$remref <- sub("^refs/remotes/", "", gb_dat$upstream)
  gb_dat$upstream <- NULL
  gb_dat$remote <- remref_remote(gb_dat$remref)
  gb_dat$ref <- remref_branch(gb_dat$remref)
  gb_dat$cfg_pr_url <- map_chr(
    glue("branch.{gb_dat$name}.pr-url"),
    \(x) git_cfg_get(x, where = "local") %||% NA_character_
  )

  ghr <- github_remote_list(these = NULL)[["remote"]]
  gb_dat <- gb_dat[is.na(gb_dat$remref) | (gb_dat$remote %in% ghr), ]

  pr_dat <- pr_list(tr = tr)
  dat <- merge(
    x = gb_dat,
    y = pr_dat,
    by.x = "name",
    by.y = "pr_local_branch",
    all.x = TRUE
  )
  dat <- dat[
    order(dat$pr_number, dat$pr_updated_at, dat$updated, decreasing = TRUE),
  ]

  missing_cfg <- is.na(dat$cfg_pr_url) & !is.na(dat$pr_html_url)
  purrr::walk2(
    glue("branch.{dat$name[missing_cfg]}.pr-url"),
    dat$pr_html_url[missing_cfg],
    \(x, y) gert::git_config_set(x, y, repo = repo)
  )

  dat
}

choose_branch <- function(exclude = character()) {
  if (!is_interactive()) {
    return(character())
  }
  dat <- branches_with_no_upstream_or_github_upstream()
  dat <- dat[!dat$name %in% exclude, ]
  if (nrow(dat) == 0) {
    return()
  }
  prompt <- "Which branch do you want to checkout? (0 to exit)"
  n_show_max <- 9
  n <- nrow(dat)
  n_shown <- compute_n_show(n, n_show_nominal = n_show_max)
  n_not_shown <- n - n_shown
  if (n_not_shown > 0) {
    branches_not_shown <- utils::tail(dat$name, -n_shown)
    dat <- dat[seq_len(n_shown), ]
    fine_print <- cli::format_inline(
      "{n_not_shown} branch{?/es} not listed: {.val {branches_not_shown}}"
    )
    prompt <- glue("{prompt}\n{fine_print}")
  }
  dat$pretty_name <- format(dat$name, justify = "right")
  dat_pretty <- purrr::pmap_chr(
    dat[c("pretty_name", "pr_number", "pr_html_url", "pr_user", "pr_title")],
    function(pretty_name, pr_number, pr_html_url, pr_user, pr_title) {
      if (is.na(pr_number)) {
        pretty_name
      } else {
        href_number <- ui_pre_glue(
          "{.href [PR #<<pr_number>>](<<pr_html_url>>)}"
        )
        at_user <- glue("@{pr_user}")
        template <- ui_pre_glue(
          "{pretty_name} {cli::symbol$arrow_right} <<href_number>> ({.field <<at_user>>}): {.val <<ui_escape_glue(pr_title)>>}"
        )
        cli::format_inline(template)
      }
    }
  )
  choice <- utils::menu(title = prompt, choices = cli::ansi_strtrim(dat_pretty))
  dat$name[choice]
}

choose_pr <- function(tr = NULL, pr_dat = NULL) {
  if (!is_interactive()) {
    return(list(pr_number = list()))
  }
  if (is.null(pr_dat)) {
    tr <- tr %||% target_repo()
    pr_dat <- pr_list(tr)
  }
  if (nrow(pr_dat) == 0) {
    return()
  }

  # wording needs to make sense for several PR-choosing tasks, e.g. fetch, view,
  # finish, forget
  prompt <- "Which PR are you interested in? (0 to exit)"
  n_show_max <- 9
  n <- nrow(pr_dat)
  n_shown <- compute_n_show(n, n_show_nominal = n_show_max)
  n_not_shown <- n - n_shown
  if (n_not_shown > 0) {
    pr_dat <- pr_dat[seq_len(n_shown), ]
    info1 <- cli::format_inline("Not shown: {n_not_shown} more PR{?s}.")
    info2 <- cli::format_inline(
      "Call {.run usethis::browse_github_pulls()} to browse all PRs."
    )
    prompt <- glue("{prompt}\n{info1}\n{info2}")
  }

  some_closed <- any(pr_dat$pr_state == "closed")
  pr_pretty <- purrr::pmap_chr(
    pr_dat[c("pr_number", "pr_html_url", "pr_user", "pr_state", "pr_title")],
    function(pr_number, pr_html_url, pr_user, pr_state, pr_title) {
      href_number <- ui_pre_glue("{.href [PR #<<pr_number>>](<<pr_html_url>>)}")
      at_user <- glue("@{pr_user}")
      pr_title_escaped <- ui_escape_glue(pr_title)
      if (some_closed) {
        template <- ui_pre_glue(
          "<<href_number>> ({.field <<at_user>>}, {pr_state}): {.val <<pr_title_escaped>>}"
        )
        cli::format_inline(template)
      } else {
        template <- ui_pre_glue(
          "<<href_number>> ({.field <<at_user>>}): {.val <<pr_title_escaped>>}"
        )
        cli::format_inline(template)
      }
    }
  )
  choice <- utils::menu(
    title = prompt,
    choices = cli::ansi_strtrim(pr_pretty)
  )
  as.list(pr_dat[choice, ])
}

# deletes the remote branch associated with a PR
# returns invisible TRUE/FALSE re: whether a deletion actually occurred
# reasons this returns FALSE
# * don't have push permission on remote where PR branch lives
# * PR has not been merged
# * remote branch has already been deleted
pr_branch_delete <- function(pr) {
  remote <- pr$pr_remote
  remote_dat <- github_remotes(remote)
  if (!isTRUE(remote_dat$can_push)) {
    return(invisible(FALSE))
  }

  gh <- gh_tr(remote_dat)
  pr_ref <- tryCatch(
    gh(
      "GET /repos/{owner}/{repo}/git/ref/{ref}",
      ref = glue("heads/{pr$pr_ref}")
    ),
    http_error_404 = function(cnd) NULL
  )

  pr_remref <- glue_data(pr, "{pr_remote}/{pr_ref}")

  if (is.null(pr_ref)) {
    ui_bullets(c(
      "i" = "PR {.href [{pr$pr_string}]({pr$pr_html_url})} originated from branch {.val {pr_remref}},
             which no longer exists."
    ))
    return(invisible(FALSE))
  }

  if (is.na(pr$pr_merged_at)) {
    ui_bullets(c(
      "i" = "PR {.href [{pr$pr_string}]({pr$pr_html_url})} is unmerged, we will not delete the
             remote branch {.val {pr_remref}}."
    ))
    return(invisible(FALSE))
  }

  ui_bullets(c(
    "v" = "PR {.href [{pr$pr_string}]({pr$pr_html_url})} has been merged, deleting remote branch
           {.val {pr_remref}}."
  ))
  # TODO: tryCatch here?
  gh(
    "DELETE /repos/{owner}/{repo}/git/refs/{ref}",
    ref = glue("heads/{pr$pr_ref}")
  )
  invisible(TRUE)
}

check_pr_branch <- function(default_branch) {
  # the glue-ing happens inside check_current_branch(), where `gb` gives the
  # current git branch
  check_current_branch(
    is_not = default_branch,
    message = c(
      "i" = "The {.code pr_*()} functions facilitate pull requests.",
      "i" = "The current branch ({.val {gb}}) is this repo's default branch, but
             pull requests should NOT come from the default branch.",
      "i" = "Do you need to call {.fun usethis::pr_init} (new PR)?
             Or {.fun usethis::pr_resume} or
             {.fun usethis::pr_fetch} (existing PR)?"
    )
  )
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/proj-desc.R ---
proj_desc <- function(path = proj_get()) {
  desc::desc(file = path)
}

proj_version <- function() {
  proj_desc()$get_field("Version")
}

proj_deps <- function() {
  proj_desc()$get_deps()
}

proj_desc_create <- function(name, fields = list(), roxygen = TRUE) {
  fields <- use_description_defaults(name, roxygen = roxygen, fields = fields)

  # https://github.com/r-lib/desc/issues/132
  desc <- desc::desc(text = glue("{names(fields)}: {fields}"))
  tidy_desc(desc)

  tf <- withr::local_tempfile()
  desc$write(file = tf)
  write_over(proj_path("DESCRIPTION"), read_utf8(tf))

  # explicit check of "usethis.quiet" since I'm not doing the printing
  if (!is_quiet()) {
    desc$print()
  }
}

# Here overwrite means "update the field if there is already a value in it,
# including appending".
proj_desc_field_update <- function(
  key,
  value,
  overwrite = TRUE,
  append = FALSE
) {
  check_string(key)
  check_character(value)
  check_bool(overwrite)

  desc <- proj_desc()

  old <- desc$get_list(key, default = "")
  if (all(value %in% old)) {
    return(invisible())
  }

  if (!overwrite && length(old) > 0 && any(old != "")) {
    ui_abort(
      "
      {.field {key}} has a different value in DESCRIPTION.
      Use {.code overwrite = TRUE} to overwrite."
    )
  }

  ui_bullets(c("v" = "Adding {.val {value}} to {.field {key}}."))

  if (append) {
    value <- union(old, value)
  }

  # https://github.com/r-lib/desc/issues/117
  desc$set_list(key, value)
  desc$write()

  invisible()
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/proj.R ---
proj <- new.env(parent = emptyenv())

proj_get_ <- function() proj$cur

proj_set_ <- function(path) {
  old <- proj$cur
  proj$cur <- path
  invisible(old)
}

#' Utility functions for the active project
#'
#' @description
#' Most `use_*()` functions act on the **active project**. If it is
#' unset, usethis uses [rprojroot](https://rprojroot.r-lib.org) to
#' find the project root of the current working directory. It establishes the
#' project root by looking for signs such as:
#' * a `.here` file
#' * an RStudio Project, i.e. a `.Rproj` file
#' * an R package, i.e. a `DESCRIPTION` file
#' * a Git repository
#' * a Positron or VS Code workspace, i.e. a `.vscode/settings.json` file
#' * a Quarto project, i.e. a `_quarto.yml` file
#' * an renv project, i.e. a `renv.lock` file
#'
#' usethis then stores the active project for use for the remainder of the
#' session.
#'
#' In general, end user scripts should not contain direct calls to
#' `usethis::proj_*()` utility functions. They are internal functions that are
#' exported for occasional interactive use or use in packages that extend
#' usethis. End user code should call `here::here()` or other functions from
#' the [here](https://here.r-lib.org) or
#' [rprojroot](https://rprojroot.r-lib.org) packages to programmatically
#' detect a project and build paths within it.
#'
#' If you are puzzled why a path (usually the current working directory) does
#' *not* appear to be inside project, it can be helpful to call
#' `here::dr_here()` to get much more verbose feedback.
#'
#' @name proj_utils
#' @family project functions
#' @examples
#' \dontrun{
#' ## see the active project
#' proj_get()
#'
#' ## manually set the active project
#' proj_set("path/to/target/project")
#'
#' ## build a path within the active project (both produce same result)
#' proj_path("R/foo.R")
#' proj_path("R", "foo", ext = "R")
#'
#' ## build a path within SOME OTHER project
#' with_project("path/to/some/other/project", proj_path("blah.R"))
#'
#' ## convince yourself that with_project() temporarily changes the project
#' with_project("path/to/some/other/project", print(proj_sitrep()))
#' }
NULL

#' @describeIn proj_utils Retrieves the active project and, if necessary,
#'   attempts to set it in the first place.
#' @export
proj_get <- function() {
  # Called for first time so try working directory
  if (!proj_active()) {
    proj_set(".")
  }

  proj_get_()
}

#' @describeIn proj_utils Sets the active project.
#' @param path Path to set. This `path` should exist or be `NULL`.
#' @param force If `TRUE`, use this path without checking the usual criteria for
#'   a project. Use sparingly! The main application is to solve a temporary
#'   chicken-egg problem: you need to set the active project in order to add
#'   project-signalling infrastructure, such as initialising a Git repo or
#'   adding a `DESCRIPTION` file.
#' @export
proj_set <- function(path = ".", force = FALSE) {
  if (!force && dir_exists(path %||% "") && is_in_proj(path)) {
    return(invisible(proj_get_()))
  }

  path <- proj_path_prep(path)
  if (is.null(path) || force) {
    proj_string <- if (is.null(path)) "<no active project>" else path
    ui_bullets(c("v" = "Setting active project to {.val {proj_string}}."))
    return(proj_set_(path))
  }

  check_path_is_directory(path)
  new_project <- proj_find(path)
  if (is.null(new_project)) {
    ui_abort(c(
      "Path {.path {pth(path)}} does not appear to be inside a project or package.",
      "Read more in the help for {.fun usethis::proj_get}."
    ))
  }
  proj_set(path = new_project, force = TRUE)
}

#' @describeIn proj_utils Builds paths within the active project returned by
#'   `proj_get()`. Thin wrapper around [fs::path()].
#' @inheritParams fs::path
#' @export
proj_path <- function(..., ext = "") {
  has_absolute_path <- function(x) any(is_absolute_path(x))
  dots <- list(...)
  if (any(map_lgl(dots, has_absolute_path))) {
    ui_abort("Paths must be relative to the active project, not absolute.")
  }

  path_norm(path(proj_get(), ..., ext = ext))
}

#' @describeIn proj_utils Runs code with a temporary active project and,
#'   optionally, working directory. It is an example of the `with_*()` functions
#'   in [withr](https://withr.r-lib.org).
#' @param code Code to run with temporary active project
#' @param setwd Whether to also temporarily set the working directory to the
#'   active project, if it is not `NULL`
#' @param quiet Whether to suppress user-facing messages, while operating in the
#'   temporary active project
#' @export
with_project <- function(
  path = ".",
  code,
  force = FALSE,
  setwd = TRUE,
  quiet = getOption("usethis.quiet", default = FALSE)
) {
  local_project(path = path, force = force, setwd = setwd, quiet = quiet)
  force(code)
}

#' @describeIn proj_utils Sets an active project and, optionally, working
#'   directory until the current execution environment goes out of scope, e.g.
#'   the end of the current function or test.  It is an example of the
#'   `local_*()` functions in [withr](https://withr.r-lib.org).
#' @param .local_envir The environment to use for scoping. Defaults to current
#'   execution environment.
#' @export
local_project <- function(
  path = ".",
  force = FALSE,
  setwd = TRUE,
  quiet = getOption("usethis.quiet", default = FALSE),
  .local_envir = parent.frame()
) {
  withr::local_options(usethis.quiet = quiet, .local_envir = .local_envir)

  old_project <- proj_get_() # this could be `NULL`, i.e. no active project
  withr::defer(proj_set(path = old_project, force = TRUE), envir = .local_envir)
  proj_set(path = path, force = force)
  temp_proj <- proj_get_() # this could be `NULL`

  if (isTRUE(setwd) && !is.null(temp_proj)) {
    withr::local_dir(temp_proj, .local_envir = .local_envir)
  }
}

## usethis policy re: preparation of the path to active project
proj_path_prep <- function(path) {
  if (is.null(path)) {
    return(path)
  }
  path <- path_abs(path)
  if (file_exists(path)) {
    path_real(path)
  } else {
    path
  }
}

## usethis policy re: preparation of user-provided path to a resource on user's
## file system
user_path_prep <- function(path) {
  ## usethis uses fs's notion of home directory
  ## this ensures we are consistent about that
  path_expand(path)
}

proj_rel_path <- function(path) {
  if (is_in_proj(path)) {
    as.character(path_rel(path, start = proj_get()))
  } else {
    path
  }
}

proj_crit <- function() {
  rprojroot::has_file(".here") |
    rprojroot::is_rstudio_project |
    rprojroot::is_r_package |
    rprojroot::is_git_root |
    rprojroot::is_vscode_project |
    rprojroot::is_quarto_project |
    rprojroot::is_renv_project |
    rprojroot::is_remake_project |
    rprojroot::is_projectile_project
}

proj_find <- function(path = ".") {
  tryCatch(
    rprojroot::find_root(proj_crit(), path = path),
    error = function(e) NULL
  )
}

possibly_in_proj <- function(path = ".") !is.null(proj_find(path))

is_package <- function(base_path = proj_get()) {
  res <- tryCatch(
    rprojroot::find_package_root_file(path = base_path),
    error = function(e) NULL
  )
  !is.null(res)
}

check_is_package <- function(whos_asking = NULL) {
  if (is_package()) {
    return(invisible())
  }

  message <- "Project {.val {project_name()}} is not an R package."
  if (!is.null(whos_asking)) {
    whos_asking_fn <- sub("()", "", whos_asking, fixed = TRUE)
    message <- c(
      "i" = "{.topic [{whos_asking}](usethis::{whos_asking_fn})} is designed to work with packages.",
      "x" = message
    )
  }
  ui_abort(message)
}

check_is_project <- function() {
  if (!possibly_in_proj()) {
    ui_abort(c(
      "We do not appear to be inside a valid project or package.",
      "Read more in the help for {.fun usethis::proj_get}."
    ))
  }
}

proj_active <- function() !is.null(proj_get_())

is_in_proj <- function(path) {
  if (!proj_active()) {
    return(FALSE)
  }
  identical(
    proj_get(),
    ## use path_abs() in case path does not exist yet
    path_common(c(proj_get(), path_expand(path_abs(path))))
  )
}

project_name <- function(base_path = proj_get()) {
  ## escape hatch necessary to solve this chicken-egg problem:
  ## create_package() calls use_description(), which calls project_name()
  ## to learn package name from the path, in order to make DESCRIPTION
  ## and DESCRIPTION is how we recognize a package as a usethis project
  if (!possibly_in_proj(base_path)) {
    return(path_file(base_path))
  }

  if (is_package(base_path)) {
    proj_desc(base_path)$get_field("Package")
  } else {
    path_file(base_path)
  }
}

#' Activate a project
#'
#' Activates a project in the usethis, R session, and (if relevant) RStudio
#' senses. If you are in RStudio, this will open a new RStudio session. If not, it will
#' change the working directory and [active project][proj_set()].
#'
#' * If using RStudio desktop, the project is opened in a new session.
#'   * If using Positron, the project is opened in a new window.
#'   * If using RStudio or Positron on a server, the project is opened in a new
#'     browser tab.
#'   * Otherwise, the working directory and active project is changed in the
#'     current R session.
#'
#' @param path Project directory
#' @return Single logical value indicating if current session is modified.
#' @export
proj_activate <- function(path) {
  check_path_is_directory(path)
  path <- user_path_prep(path)

  if (rstudio_available() && rstudioapi::hasFun("openProject")) {
    # TODO: Perhaps in future this message can be specialized for RStudio vs.
    # Positron. For now, I've just made it more generic to work better for both.
    ui_bullets(c(
      "v" = "Opening {.path {pth(path, base = NA)}} in a new session."
    ))
    rstudioapi::openProject(path, newSession = TRUE)
    invisible(FALSE)
  } else {
    proj_set(path)
    rel_path <- path_rel(proj_get(), path_wd())
    if (rel_path != ".") {
      ui_bullets(c(
        "v" = "Changing working directory to {.path {pth(path, base = NA)}}"
      ))
      setwd(proj_get())
    }
    invisible(TRUE)
  }
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/r.R ---
#' Create or edit R or test files
#'
#' This pair of functions makes it easy to create paired R and test files,
#' using the convention that the tests for `R/foofy.R` should live
#' in `tests/testthat/test-foofy.R`. You can use them to create new files
#' from scratch by supplying `name`, or if you use RStudio, you can call
#' to create (or navigate to) the companion file based on the currently open
#' file. This also works when a test snapshot file is active, i.e. if you're
#' looking at `tests/testthat/_snaps/foofy.md`, `use_r()` or `use_test()` take
#' you to `R/foofy.R` or `tests/testthat/test-foofy.R`, respectively.
#'
#' @section Renaming files in an existing package:
#'
#' Here are some tips on aligning file names across `R/` and `tests/testthat/`
#' in an existing package that did not necessarily follow this convention
#' before.
#'
#' This script generates a data frame of `R/` and test files that can help you
#' identify missed opportunities for pairing:
#'
#' ```
#' library(fs)
#' library(tidyverse)
#'
#' bind_rows(
#'   tibble(
#'     type = "R",
#'     path = dir_ls("R/", regexp = "\\.[Rr]$"),
#'     name = as.character(path_ext_remove(path_file(path))),
#'   ),
#'   tibble(
#'     type = "test",
#'     path = dir_ls("tests/testthat/", regexp = "/test[^/]+\\.[Rr]$"),
#'     name = as.character(path_ext_remove(str_remove(path_file(path), "^test[-_]"))),
#'   )
#' ) |>
#'   pivot_wider(names_from = type, values_from = path) |>
#'   print(n = Inf)
#' ```
#'
#' The [rename_files()] function can also be helpful.
#'
#' @param name Either a string giving a file name (without directory) or
#'   `NULL` to take the name from the currently open file in RStudio.
#' @inheritParams edit_file
#' @seealso
#' * The [testing](https://r-pkgs.org/testing-basics.html) and
#'   [R code](https://r-pkgs.org/code.html) chapters of
#'   [R Packages](https://r-pkgs.org).
#' * [use_test_helper()] to create a testthat helper file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # create a new .R file below R/
#' use_r("coolstuff")
#'
#' # if `R/coolstuff.R` is active in a supported IDE, you can now do:
#' use_test()
#'
#' # if `tests/testthat/test-coolstuff.R` is active in a supported IDE, you can
#' # return to `R/coolstuff.R` with:
#' use_r()
#' }
use_r <- function(name = NULL, open = rlang::is_interactive()) {
  use_directory("R")

  path <- path("R", compute_name(name))
  edit_file(proj_path(path), open = open)

  invisible(TRUE)
}

#' @rdname use_r
#' @export
use_test <- function(name = NULL, open = rlang::is_interactive()) {
  if (!uses_testthat()) {
    use_testthat_impl()
  }

  path <- path("tests", "testthat", paste0("test-", compute_name(name)))
  if (!file_exists(path)) {
    use_template("test-example-2.1.R", save_as = path)
  }
  edit_file(proj_path(path), open = open)

  invisible(TRUE)
}

#' Create or edit a test helper file
#'
#' This function creates (or opens) a test helper file, typically
#' `tests/testthat/helper.R`. Test helper files are executed at the
#' beginning of every automated test run and are also executed by
#' [`load_all()`][pkgload::load_all]. A helper file is a great place to
#' define test helper functions for use throughout your test suite, such as
#' a custom expectation.
#'
#' @param name Can be used to specify the optional "SLUG" in
#'   `tests/testthat/helper-SLUG.R`.
#' @inheritParams edit_file
#' @seealso
#' * [use_test()] to create a test file.
#' * The testthat vignette on special files
#'   `vignette("special-files", package = "testthat")`.
#' @export
#'
#' @examples
#' \dontrun{
#' use_test_helper()
#' use_test_helper("mocks")
#' }
use_test_helper <- function(name = NULL, open = rlang::is_interactive()) {
  maybe_name(name)

  if (!uses_testthat()) {
    ui_abort(c(
      "x" = "Your package must use {.pkg testthat} to use a helper file.",
      "_" = "Call {.run usethis::use_testthat()} to set up {.pkg testthat}."
    ))
  }

  target_path <- proj_path(
    path("tests", "testthat", as_test_helper_file(name))
  )

  if (!file_exists(target_path)) {
    ui_bullets(c(
      "i" = "Test helper files are executed at the start of all automated
             test runs.",
      "i" = "{.run devtools::load_all()} also sources test helper files."
    ))
  }
  edit_file(target_path, open = open)

  invisible(TRUE)
}

# helpers -----------------------------------------------------------------

compute_name <- function(name = NULL, ext = "R", error_call = caller_env()) {
  if (!is.null(name)) {
    check_file_name(name, call = error_call)

    if (path_ext(name) == "") {
      name <- path_ext_set(name, ext)
    } else if (path_ext(name) != ext) {
      cli::cli_abort(
        "{.arg name} must have extension {.str {ext}}, not {.str {path_ext(name)}}.",
        call = error_call
      )
    }
    return(as.character(name))
  }

  if (!rstudio_available()) {
    cli::cli_abort(
      "{.arg name} is absent but must be specified.",
      call = error_call
    )
  }
  compute_active_name(
    path = rstudioapi::getSourceEditorContext()$path,
    ext = ext,
    error_call = error_call
  )
}

compute_active_name <- function(path, ext, error_call = caller_env()) {
  if (is.null(path)) {
    cli::cli_abort(
      c(
        "No file is open in RStudio.",
        i = "Please specify {.arg name}."
      ),
      call = error_call
    )
  }

  ## rstudioapi can return a path like '~/path/to/file' where '~' means
  ## R's notion of user's home directory
  path <- proj_path_prep(path_expand_r(path))

  dir <- path_dir(proj_rel_path(path))
  if (!dir %in% c("R", "src", "tests/testthat", "tests/testthat/_snaps")) {
    cli::cli_abort(
      "Open file must be code, test, or snapshot.",
      call = error_call
    )
  }

  file <- path_file(path)
  if (dir == "tests/testthat") {
    file <- gsub("^test[-_]", "", file)
  }
  as.character(path_ext_set(file, ext))
}

check_file_name <- function(name, call = caller_env()) {
  if (!is_string(name)) {
    cli::cli_abort("{.arg name} must be a single string", call = call)
  }

  if (name == "") {
    cli::cli_abort("{.arg name} must not be an empty string", call = call)
  }

  if (path_dir(name) != ".") {
    cli::cli_abort(
      "{.arg name} must be a file name without directory.",
      call = call
    )
  }

  if (!valid_file_name(path_ext_remove(name))) {
    cli::cli_abort(
      c(
        "{.arg name} ({.str {name}}) must be a valid file name.",
        i = "A valid file name consists of only ASCII letters, numbers, '-', and '_'."
      ),
      call = call
    )
  }
}

valid_file_name <- function(x) {
  grepl("^[a-zA-Z0-9._-]+$", x)
}

as_test_helper_file <- function(name = NULL) {
  file <- name %||% "helper.R"
  if (!grepl("^helper", file)) {
    file <- glue("helper-{file}")
  }
  if (path_ext(file) == "") {
    file <- path_ext_set(file, "R")
  }
  unclass(file)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/rcpp.R ---
#' Use C, C++, RcppArmadillo, or RcppEigen
#'
#' Adds infrastructure commonly needed when using compiled code:
#'   * Creates `src/`
#'   * Adds required packages to `DESCRIPTION`
#'   * May create an initial placeholder `.c` or `.cpp` file
#'   * Creates `Makevars` and `Makevars.win` files (`use_rcpp_armadillo()` only)
#'
#' @inheritParams use_r
#' @export
use_rcpp <- function(name = NULL) {
  check_is_package("use_rcpp()")
  check_uses_roxygen("use_rcpp()")

  use_dependency("Rcpp", "LinkingTo")
  use_dependency("Rcpp", "Imports")
  roxygen_ns_append("@importFrom Rcpp sourceCpp") && roxygen_remind()

  use_src()
  path <- path("src", compute_name(name, "cpp"))
  use_template("code.cpp", path)
  edit_file(proj_path(path))

  invisible()
}

#' @rdname use_rcpp
#' @export
use_rcpp_armadillo <- function(name = NULL) {
  use_rcpp(name)

  use_dependency("RcppArmadillo", "LinkingTo")

  makevars_settings <- list(
    "CXX_STD" = "CXX11",
    "PKG_CXXFLAGS" = "$(SHLIB_OPENMP_CXXFLAGS)",
    "PKG_LIBS" = "$(SHLIB_OPENMP_CXXFLAGS) $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)"
  )
  use_makevars(makevars_settings)

  invisible()
}

#' @rdname use_rcpp
#' @export
use_rcpp_eigen <- function(name = NULL) {
  use_rcpp(name)

  use_dependency("RcppEigen", "LinkingTo")

  roxygen_ns_append("@import RcppEigen") && roxygen_remind()

  invisible()
}

#' @rdname use_rcpp
#' @export
use_c <- function(name = NULL) {
  check_is_package("use_c()")
  check_uses_roxygen("use_c()")

  use_src()

  path <- path("src", compute_name(name, ext = "c"))
  use_template("code.c", path)
  edit_file(proj_path(path))

  invisible(TRUE)
}

use_src <- function() {
  use_directory("src")
  use_git_ignore(c("*.o", "*.so", "*.dll"), "src")
  roxygen_ns_append(glue(
    "@useDynLib {project_name()}, .registration = TRUE"
  )) &&
    roxygen_remind()

  invisible()
}

use_makevars <- function(settings = NULL) {
  use_directory("src")

  settings_list <- settings %||% list()
  check_is_named_list(settings_list)

  makevars_entries <- vapply(settings_list, glue_collapse, character(1))
  makevars_content <- glue("{names(makevars_entries)} = {makevars_entries}")

  makevars_path <- proj_path("src", "Makevars")
  makevars_win_path <- proj_path("src", "Makevars.win")

  if (!file_exists(makevars_path) && !file_exists(makevars_win_path)) {
    write_utf8(makevars_path, makevars_content)
    file_copy(makevars_path, makevars_win_path)
    ui_bullets(c(
      "v" = "Created {.path {pth(makevars_path)}} and
             {.path {pth(makevars_win_path)}} with requested compilation settings."
    ))
  } else {
    ui_bullets(c(
      "_" = "Ensure the following Makevars compilation settings are set for both
             {.path {pth(makevars_path)}} and {.path {pth(makevars_win_path)}}:"
    ))
    ui_code_snippet(
      makevars_content,
      language = ""
    )
    edit_file(makevars_path)
    edit_file(makevars_win_path)
  }
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/readme.R ---
#' Create README files
#'
#' @description
#' Creates skeleton README files with possible stubs for
#' * a high-level description of the project/package and its goals
#' * R code to install from GitHub, if GitHub usage detected
#' * a basic example
#'
#' Use `Rmd` if you want a rich intermingling of code and output. Use `md` for a
#' basic README. `README.Rmd` will be automatically added to `.Rbuildignore`.
#' The resulting README is populated with default YAML frontmatter and R fenced
#' code blocks (`md`) or chunks (`Rmd`).
#'
#' If you use `Rmd`, you'll still need to render it regularly, to keep
#' `README.md` up-to-date. `devtools::build_readme()` is handy for this. You
#' could also use GitHub Actions to re-render `README.Rmd` every time you push.
#' An example workflow can be found in the `examples/` directory here:
#' <https://github.com/r-lib/actions/>.
#'
#' If the current project is a Git repo, then `use_readme_rmd()` automatically
#' configures a pre-commit hook that helps keep `README.Rmd` and `README.md`,
#' synchronized. The hook creates friction if you try to commit when
#' `README.Rmd` has been edited more recently than `README.md`. If this hook
#' causes more problems than it solves for you, it is implemented in
#' `.git/hooks/pre-commit`, which you can modify or even delete.
#'
#' @inheritParams use_template
#' @seealso The [other markdown files
#'   section](https://r-pkgs.org/other-markdown.html) of [R
#'   Packages](https://r-pkgs.org).
#' @export
#' @examples
#' \dontrun{
#' use_readme_rmd()
#' use_readme_md()
#' }
use_readme_rmd <- function(open = rlang::is_interactive()) {
  check_is_project()
  check_installed("rmarkdown")

  is_pkg <- is_package()
  repo_spec <- tryCatch(target_repo_spec(ask = FALSE), error = function(e) NULL)
  nm <- if (is_pkg) "Package" else "Project"
  data <- list2(
    !!nm := project_name(),
    Rmd = TRUE,
    on_github = !is.null(repo_spec),
    github_spec = repo_spec
  )

  new <- use_template(
    if (is_pkg) "package-README" else "project-README",
    "README.Rmd",
    data = data,
    ignore = is_pkg,
    open = open
  )
  if (!new) {
    return(invisible(FALSE))
  }

  if (is_pkg && !data$on_github) {
    ui_bullets(c(
      "_" = "Update {.path {pth('README.Rmd')}} to include installation instructions."
    ))
  }

  if (uses_git()) {
    use_git_hook(
      "pre-commit",
      render_template("readme-rmd-pre-commit.sh")
    )
  }

  invisible(TRUE)
}

#' @export
#' @rdname use_readme_rmd
use_readme_md <- function(open = rlang::is_interactive()) {
  check_is_project()
  is_pkg <- is_package()
  repo_spec <- tryCatch(target_repo_spec(ask = FALSE), error = function(e) NULL)
  nm <- if (is_pkg) "Package" else "Project"
  data <- list2(
    !!nm := project_name(),
    Rmd = FALSE,
    on_github = !is.null(repo_spec),
    github_spec = repo_spec
  )

  new <- use_template(
    if (is_pkg) "package-README" else "project-README",
    "README.md",
    data = data,
    open = open
  )

  if (is_pkg && !data$on_github) {
    ui_bullets(c(
      "_" = "Update {.path {pth('README.md')}} to include installation instructions."
    ))
  }

  invisible(new)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/release.R ---
#' Create a release checklist in a GitHub issue
#'
#' @description
#' When preparing to release a package to CRAN there are quite a few steps that
#' need to be performed, and some of the steps can take multiple hours. This
#' function creates a checklist in a GitHub issue to:
#'
#' * Help you keep track of where you are in the process
#' * Feel a sense of satisfaction as you progress towards final submission
#' * Help watchers of your package stay informed.
#'
#' The checklist contains a generic set of steps that we've found to be helpful,
#' based on the type of release ("patch", "minor", or "major"). You're
#' encouraged to edit the issue to customize this list to meet your needs.
#'
#' ## Customization
#'
#' * If you want to consistently add extra bullets for every release, you can
#'   include your own custom bullets by providing an (unexported)
#'   `release_bullets()` function that returns a character vector.
#'   (For historical reasons, `release_questions()` is also supported).
#'
#' * If you want to check additional packages in the revdep check process,
#'   provide an (unexported) `release_extra_revdeps()` function that
#'   returns a character vector. This is currently only supported for
#'   Posit internal check tooling.
#'
#' @param version Optional version number for release. If unspecified, you can
#'   make an interactive choice.
#' @export
#' @examples
#' \dontrun{
#' use_release_issue("2.0.0")
#' }
use_release_issue <- function(version = NULL) {
  check_is_package("use_release_issue()")
  tr <- target_repo(github_get = TRUE)
  if (!isTRUE(tr$can_push)) {
    ui_bullets(c(
      "!" = "It is very unusual to open a release issue on a repo you can't push
             to ({.val {tr$repo_spec}})."
    ))
    if (ui_nah("Do you really want to do this?")) {
      ui_bullets(c("x" = "Cancelling."))
      return(invisible())
    }
  }

  version <- version %||%
    choose_version(
      "What should the release version be?",
      which = c("major", "minor", "patch")
    )
  if (is.null(version)) {
    return(invisible(FALSE))
  }

  on_cran <- !is.null(cran_version())
  checklist <- release_checklist(version, on_cran, tr)

  gh <- gh_tr(tr)
  issue <- gh(
    "POST /repos/{owner}/{repo}/issues",
    title = glue("Release {project_name()} {version}"),
    body = paste0(checklist, "\n", collapse = "")
  )

  Sys.sleep(1)
  view_url(issue$html_url)
}

release_checklist <- function(version, on_cran, target_repo = NULL) {
  type <- release_type(version)
  cran_results <- cran_results_url()
  has_news <- file_exists(proj_path("NEWS.md"))
  has_pkgdown <- uses_pkgdown()
  has_lifecycle <- proj_desc()$has_dep("lifecycle")
  has_readme <- file_exists(proj_path("README.Rmd"))
  has_github_links <- has_github_links(target_repo)
  is_posit_pkg <- is_posit_pkg()
  milestone_num <- gh_milestone_number(target_repo, version)

  c(
    if (!on_cran) {
      c(
        "First release:",
        "",
        todo("`usethis::use_news_md()`", !has_news),
        todo("`usethis::use_cran_comments()`"),
        todo("Update (aspirational) install instructions in README"),
        todo("Proofread `Title:` and `Description:`"),
        todo(
          "Check that all exported functions have `@return` and `@examples`"
        ),
        todo(
          "Check that `Authors@R:` includes a copyright holder (role 'cph')"
        ),
        todo(
          "Check [licensing of included files](https://r-pkgs.org/license.html#sec-code-you-bundle)"
        ),
        todo("Review <https://github.com/DavisVaughan/extrachecks>"),
        ""
      )
    },
    "Prepare for release:",
    "",
    todo("`git pull`"),
    todo(
      "[Close v{version} milestone](../milestone/{milestone_num})",
      !is.na(milestone_num)
    ),
    todo("Check [current CRAN check results]({cran_results})", on_cran),
    todo(
      "[Advance deprecations](https://lifecycle.r-lib.org/articles/communicate.html#gradual-deprecation), if needed",
      type != "patch" && has_lifecycle
    ),
    todo("`usethis::use_news_md()`", on_cran && !has_news),
    todo(
      "[Polish NEWS](https://style.tidyverse.org/news.html#news-release)",
      on_cran
    ),
    todo("`usethis::use_github_links()`", !has_github_links),
    todo("`urlchecker::url_check()`"),
    todo("`devtools::build_readme()`", has_readme),
    todo("`devtools::check(remote = TRUE, manual = TRUE)`"),
    todo("`devtools::check_win_devel()`"),
    if (type != "patch") release_revdepcheck(on_cran, is_posit_pkg),
    todo("Update `cran-comments.md`", on_cran),
    todo("`git push`"),
    todo("Draft blog post", type != "patch"),
    release_extra_bullets(),
    "",
    "Submit to CRAN:",
    "",
    todo("`usethis::use_version('{type}')`"),
    todo("`devtools::submit_cran()`"),
    todo("Approve email"),
    "",
    "Wait for CRAN...",
    "",
    todo("Accepted :tada:"),
    todo("Finish & publish blog post", type != "patch"),
    todo("Add link to blog post in pkgdown news menu", type != "patch"),
    todo("`usethis::use_github_release()`"),
    todo("`usethis::use_dev_version(push = TRUE)`"),
    todo("`usethis::use_news_md()`", !has_news),
    todo("Share on social media", type != "patch"),
    todo(
      "Slack link to blog post, bluesky, and linkedin in #open-source-comms",
      type != "patch" && is_posit_pkg
    )
  )
}

gh_milestone_number <- function(target_repo, version, state = "open") {
  gh <- gh_tr(target_repo)
  milestones <- tryCatch(
    gh("/repos/{owner}/{repo}/milestones", state = state),
    error = function(e) list()
  )
  titles <- map_chr(milestones, "title")
  numbers <- map_int(milestones, "number")

  numbers[match(paste0("v", version), titles)]
}

# Get revdeps for current package
get_revdeps <- function() {
  pkg <- proj_desc()$get_field("Package")
  tools::package_dependencies(pkg, which = "all", reverse = TRUE)[[pkg]]
}

release_revdepcheck <- function(
  on_cran = TRUE,
  is_posit_pkg = TRUE,
  env = NULL
) {
  if (!on_cran || length(get_revdeps()) == 0) {
    return()
  }

  env <- env %||% safe_pkg_env()
  if (env_has(env, "release_extra_revdeps")) {
    extra <- env$release_extra_revdeps()
    stopifnot(is.character(extra))
  } else {
    extra <- character()
  }

  if (is_posit_pkg) {
    if (length(extra) > 0) {
      extra_code <- paste0(deparse(extra), collapse = "")
      todo("`revdepcheck::cloud_check(extra_revdeps = {extra_code})`")
    } else {
      todo("`revdepcheck::cloud_check()`")
    }
  } else {
    todo("`revdepcheck::revdep_check(num_workers = 4)`")
  }
}

release_extra_bullets <- function(env = NULL) {
  env <- env %||% safe_pkg_env()

  if (env_has(env, "release_bullets")) {
    paste0("* [ ] ", env$release_bullets())
  } else if (env_has(env, "release_questions")) {
    # For backwards compatibility with devtools
    paste0("* [ ] ", env$release_questions())
  } else {
    character()
  }
}

safe_pkg_env <- function() {
  tryCatch(
    ns_env(project_name()),
    error = function(e) emptyenv()
  )
}

release_type <- function(version) {
  x <- unclass(numeric_version(version))[[1]]
  n <- length(x)
  if (n >= 3 && x[[3]] != 0L) {
    "patch"
  } else if (n >= 2 && x[[2]] != 0L) {
    "minor"
  } else {
    "major"
  }
}

#' Publish a GitHub release
#'
#' @description
#' Pushes the current branch (if safe) then publishes a GitHub release for the
#' latest CRAN submission.
#'
#' If you use [devtools::submit_cran()] to submit to CRAN, information about the
#' submitted state is captured in a `CRAN-SUBMISSION` file.
#' `use_github_release()` uses this info to populate the GitHub release notes
#' and, after success, deletes the file. In the absence of such a file, we
#' assume that current state (SHA of `HEAD`, package version, NEWS) is the
#' submitted state.
#'
#' @param publish If `TRUE`, publishes a release. If `FALSE`, creates a draft
#'   release.
#' @export
use_github_release <- function(publish = TRUE) {
  check_is_package("use_github_release()")

  tr <- target_repo(github_get = TRUE, ok_configs = c("ours", "fork"))
  check_can_push(tr = tr, "to create a release")

  dat <- get_release_data(tr)
  release_name <- glue("{dat$Package} {dat$Version}")
  tag_name <- glue("v{dat$Version}")
  kv_line("Release name", release_name)
  kv_line("Tag name", tag_name)
  kv_line("SHA", dat$SHA)

  if (git_can_push()) {
    git_push()
  }
  check_github_has_SHA(SHA = dat$SHA, tr = tr)

  on_cran <- !is.null(cran_version())
  news <- get_release_news(SHA = dat$SHA, tr = tr, on_cran = on_cran)

  gh <- gh_tr(tr)

  ui_bullets("Publishing {tag_name} release to GitHub")
  release <- gh(
    "POST /repos/{owner}/{repo}/releases",
    name = release_name,
    tag_name = tag_name,
    target_commitish = dat$SHA,
    body = news,
    draft = !publish
  )
  ui_bullets("Release at {.url {release$html_url}}")

  if (!is.null(dat$file)) {
    ui_bullets("Deleting {.path {dat$file}}")
    file_delete(dat$file)
  }

  invisible()
}

get_release_data <- function(tr = target_repo(github_get = TRUE)) {
  cran_submission <-
    path_first_existing(proj_path(c("CRAN-SUBMISSION", "CRAN-RELEASE")))

  if (is.null(cran_submission)) {
    ui_bullets(c("v" = "Using current HEAD commit for the release."))
    challenge_non_default_branch()
    check_branch_pushed()
    return(list(
      Package = project_name(),
      Version = proj_version(),
      SHA = gert::git_info(repo = git_repo())$commit
    ))
  }

  if (path_file(cran_submission) == "CRAN-SUBMISSION") {
    # new style ----
    # Version: 2.4.2
    # Date: 2021-10-13 20:40:36 UTC
    # SHA: fbe18b5a22be8ebbb61fa7436e826ba8d7f485a9
    out <- as.list(read.dcf(cran_submission)[1, ])
  }

  if (path_file(cran_submission) == "CRAN-RELEASE") {
    gh <- gh_tr(tr)
    # old style ----
    # This package was submitted to CRAN on 2021-10-13.
    # Once it is accepted, delete this file and tag the release (commit e10658f5).
    lines <- read_utf8(cran_submission)
    str_extract <- function(marker, pattern) {
      re_match(grep(marker, lines, value = TRUE), pattern)$.match
    }
    date <- str_extract("submitted.*on", "[0-9]{4}-[0-9]{2}-[0-9]{2}")
    sha <- str_extract("commit", "[[:xdigit:]]{7,40}")
    if (nchar(sha) != 40) {
      # the release endpoint requires the full sha
      sha <-
        gh("/repos/{owner}/{repo}/commits/{commit_sha}", commit_sha = sha)$sha
    }

    HEAD <- gert::git_info(repo = git_repo())$commit
    if (HEAD == sha) {
      version <- proj_version()
    } else {
      tf <- withr::local_tempfile()
      gh(
        "/repos/{owner}/{repo}/contents/{path}",
        path = "DESCRIPTION",
        ref = sha,
        .destfile = tf,
        .accept = "application/vnd.github.v3.raw"
      )
      version <- desc::desc_get_version(tf)
    }

    out <- list(
      Version = version,
      Date = Sys.Date(),
      SHA = sha
    )
  }

  out$Package <- project_name()
  out$file <- cran_submission
  ui_bullets(c(
    "{.path {pth(out$file)}} file found, from a submission on {as.Date(out$Date)}."
  ))

  out
}

check_github_has_SHA <- function(
  SHA = gert::git_info(repo = git_repo())$commit,
  tr = target_repo(github_get = TRUE)
) {
  safe_gh <- purrr::safely(gh_tr(tr))
  SHA_GET <- safe_gh(
    "/repos/{owner}/{repo}/git/commits/{commit_sha}",
    commit_sha = SHA
  )
  if (is.null(SHA_GET$error)) {
    return()
  }
  if (inherits(SHA_GET$error, "http_error_404")) {
    ui_abort(c(
      "Can't find SHA {.val {substr(SHA, 1, 7)}} in {.val {tr$repo_spec}}.",
      "Do you need to push?"
    ))
  }
  ui_abort("Internal error: Unexpected error when checking for SHA on GitHub.")
}

get_release_news <- function(
  SHA = gert::git_info(repo = git_repo())$commit,
  tr = target_repo(github_get = TRUE),
  on_cran = !is.null(cran_version())
) {
  HEAD <- gert::git_info(repo = git_repo())$commit

  if (HEAD == SHA) {
    news_path <- proj_path("NEWS.md")
    news <- if (file_exists(news_path)) read_utf8(news_path) else NULL
  } else {
    news <- tryCatch(
      read_github_file(
        tr$repo_spec,
        path = "NEWS.md",
        ref = SHA,
        host = tr$api_url
      ),
      github_error = NULL
    )
  }

  if (is.null(news)) {
    ui_bullets(c(
      "x" = "Can't find {.path {pth('NEWS.md')}} in the released package source.",
      "i" = "{.pkg usethis} consults this file for release notes.",
      "i" = "Call {.run usethis::use_news_md()} to set this up for the future."
    ))
    if (on_cran) "-- no release notes --" else "Initial release"
  } else {
    news_latest(news)
  }
}

cran_version <- function(package = project_name(), available = NULL) {
  if (!curl::has_internet()) {
    return(NULL)
  }

  if (is.null(available)) {
    # Guard against CRAN mirror being unset
    available <- tryCatch(
      available.packages(repos = default_cran_mirror()),
      error = function(e) NULL
    )
    if (is.null(available)) {
      return(NULL)
    }
  }

  idx <- available[, "Package"] == package
  if (any(idx)) {
    as.package_version(available[package, "Version"])
  } else {
    NULL
  }
}

cran_results_url <- function(package = project_name()) {
  glue("https://cran.rstudio.org/web/checks/check_results_{package}.html")
}

news_latest <- function(lines) {
  headings <- which(grepl("^#\\s+", lines))

  if (length(headings) == 0) {
    ui_abort("No top-level headings found in {.path {pth('NEWS.md')}}.")
  } else if (length(headings) == 1) {
    news <- lines[seq2(headings + 1, length(lines))]
  } else {
    news <- lines[seq2(headings[[1]] + 1, headings[[2]] - 1)]
  }

  # Remove leading and trailing empty lines
  text <- which(news != "")
  if (length(text) == 0) {
    return("")
  }

  news <- news[text[[1]]:text[[length(text)]]]

  paste0(news, "\n", collapse = "")
}

is_posit_pkg <- function() {
  is_posit_cph_or_fnd() || is_in_posit_org()
}

is_posit_cph_or_fnd <- function() {
  if (!is_package()) {
    return(FALSE)
  }
  roles <- get_posit_roles()
  "cph" %in% roles || "fnd" %in% roles
}

is_posit_person_canonical <- function() {
  if (!is_package()) {
    return(FALSE)
  }
  roles <- get_posit_roles()
  length(roles) > 0 &&
    "fnd" %in% roles &&
    "cph" %in% roles &&
    attr(roles, "appears_in", exact = TRUE) == "given" &&
    attr(roles, "appears_as", exact = TRUE) == "Posit Software, PBC" &&
    attr(roles, "ror", exact = TRUE) %in% "03wc8by49"
}

get_posit_roles <- function() {
  if (!is_package()) {
    return()
  }

  desc <- proj_desc()
  fnd <- unclass(desc$get_author("fnd"))
  cph <- unclass(desc$get_author("cph"))

  detect_posit <- function(x) {
    any(grepl("rstudio|posit", tolower(x[c("given", "family")])))
  }
  fnd <- purrr::keep(fnd, detect_posit)
  cph <- purrr::keep(cph, detect_posit)

  if (length(fnd) < 1 && length(cph) < 1) {
    return(character())
  }

  person <- c(fnd, cph)[[1]]
  out <- person$role
  if (!is.null(person$given) && nzchar(person$given)) {
    attr(out, "appears_as") <- person$given
    attr(out, "appears_in") <- "given"
  } else {
    attr(out, "appears_as") <- person$family
    attr(out, "appears_in") <- "family"
  }

  comment <- person$comment %||% character()
  attr(out, "ror") <- comment["ROR"]

  out
}

is_in_posit_org <- function() {
  if (!is_package()) {
    return(FALSE)
  }
  desc <- proj_desc()
  urls <- desc$get_urls()
  dat <- parse_github_remotes(urls)
  dat <- dat[dat$host == "github.com", ]
  purrr::some(dat$repo_owner, \(x) x %in% posit_orgs())
}

posit_orgs <- function() {
  c(
    "tidyverse",
    "r-lib",
    "tidymodels",
    "rstudio",
    "posit-dev"
  )
}

todo <- function(x, cond = TRUE) {
  x <- glue(x, .envir = parent.frame())
  if (cond) {
    paste0("* [ ] ", x)
  }
}

author_has_rstudio_email <- function() {
  if (!is_package()) {
    return()
  }
  desc <- proj_desc()
  any(grepl("@rstudio[.]com", tolower(desc$get_authors())))
}

pkg_minimum_r_version <- function() {
  deps <- proj_desc()$get_deps()
  r_dep <- deps[deps$package == "R" & deps$type == "Depends", "version"]
  if (length(r_dep) > 0) {
    numeric_version(gsub("[^0-9.]", "", r_dep))
  } else {
    NA_character_
  }
}

# Borrowed from pak, but modified also retain user's non-cran repos:
# https://github.com/r-lib/pak/blob/168ab5d58fc244e5084c2800c87b8a574d66c3ba/R/default-cran-mirror.R
default_cran_mirror <- function() {
  repos <- getOption("repos")
  cran <- repos["CRAN"]
  if (is.null(cran) || is.na(cran) || cran == "@CRAN@") {
    repos["CRAN"] <- "https://cloud.r-project.org"
  }
  repos
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/rename-files.R ---
#' Automatically rename paired `R/` and `test/` files
#'
#' @description
#' * Moves `R/{old}.R` to `R/{new}.R`
#' * Moves `src/{old}.*` to `src/{new}.*`
#' * Moves `tests/testthat/test-{old}.R` to `tests/testthat/test-{new}.R`
#' * Moves `tests/testthat/test-{old}-*.*` to `tests/testthat/test-{new}-*.*`
#'   and updates paths in the test file.
#' * Removes `context()` calls from the test file, which are unnecessary
#'   (and discouraged) as of testthat v2.1.0.
#'
#' This is a potentially dangerous operation, so you must be using Git in
#' order to use this function.
#'
#' @param old,new Old and new file names (with or without `.R` extensions).
#' @export
rename_files <- function(old, new) {
  check_uses_git()
  challenge_uncommitted_changes(
    msg = "
    There are uncommitted changes and we're about to bulk-rename files. It is \\
    highly recommended to get into a clean Git state before bulk-editing files",
    untracked = TRUE
  )

  old <- sub("\\.R$", "", old)
  new <- sub("\\.R$", "", new)

  # R/ ------------------------------------------------------------------------
  r_old_path <- proj_path("R", old, ext = "R")
  r_new_path <- proj_path("R", new, ext = "R")
  if (file_exists(r_old_path)) {
    ui_bullets(c(
      "v" = "Moving {.path {pth(r_old_path)}} to {.path {pth(r_new_path)}}."
    ))
    file_move(r_old_path, r_new_path)
  }

  # src/ ------------------------------------------------------------------------
  if (dir_exists(proj_path("src"))) {
    src_old <- dir_ls(proj_path("src"), glob = glue("*/src/{old}.*"))

    src_new_file <- gsub(glue("^{old}"), glue("{new}"), path_file(src_old))
    src_new <- path(path_dir(src_old), src_new_file)

    if (length(src_old) > 1) {
      ui_bullets(c(
        "v" = "Moving {.path {pth(src_old)}} to {.path {pth(src_new)}}."
      ))
      file_move(src_old, src_new)
    }
  }

  # tests/testthat/ ------------------------------------------------------------
  if (!uses_testthat()) {
    return(invisible())
  }

  rename_test <- function(path) {
    file <- gsub(glue("^test-{old}"), glue("test-{new}"), path_file(path))
    file <- gsub(glue("^{old}.md"), glue("{new}.md"), file)
    path(path_dir(path), file)
  }
  old_test <- dir_ls(
    proj_path("tests", "testthat"),
    glob = glue("*/test-{old}*")
  )
  new_test <- rename_test(old_test)
  if (length(old_test) > 0) {
    ui_bullets(c(
      "v" = "Moving {.path {pth(old_test)}} to {.path {pth(new_test)}}."
    ))
    file_move(old_test, new_test)
  }
  snaps_dir <- proj_path("tests", "testthat", "_snaps")
  if (dir_exists(snaps_dir)) {
    old_snaps <- dir_ls(snaps_dir, glob = glue("*/{old}.md"))
    if (length(old_snaps) > 0) {
      new_snaps <- rename_test(old_snaps)
      ui_bullets(c(
        "v" = "Moving {.path {pth(old_snaps)}} to {.path {pth(new_snaps)}}."
      ))
      file_move(old_snaps, new_snaps)
    }
  }

  # tests/testthat/test-{new}.R ------------------------------------------------
  test_path <- proj_path("tests", "testthat", glue("test-{new}"), ext = "R")
  if (!file_exists(test_path)) {
    return(invisible())
  }

  lines <- read_utf8(test_path)

  # Remove old context lines
  context <- grepl("context\\(.*\\)", lines)
  if (any(context)) {
    ui_bullets(c("v" = "Removing call to {.fun context}."))
    lines <- lines[!context]
    if (lines[[1]] == "") {
      lines <- lines[-1]
    }
  }

  old_test <- old_test[new_test != test_path]
  new_test <- new_test[new_test != test_path]

  if (length(old_test) > 0) {
    ui_bullets(c("v" = "Updating paths in {.path {pth(test_path)}}."))

    for (i in seq_along(old_test)) {
      lines <- gsub(
        path_file(old_test[[i]]),
        path_file(new_test[[i]]),
        lines,
        fixed = TRUE
      )
    }
  }

  write_utf8(test_path, lines)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/revdep.R ---
#' Reverse dependency checks
#'
#' Performs set up for checking the reverse dependencies of an R package, as
#' implemented by the revdepcheck package:
#' * Creates `revdep/` directory and adds it to `.Rbuildignore`
#' * Populates `revdep/.gitignore` to prevent tracking of various revdep
#' artefacts
#' * Prompts user to run the checks with `revdepcheck::revdep_check()`
#'
#' @export
use_revdep <- function() {
  check_is_package("use_revdep()")
  use_directory("revdep", ignore = TRUE)
  use_git_ignore(
    directory = "revdep",
    c(
      "checks",
      "library",
      "checks.noindex",
      "library.noindex",
      "cloud.noindex",
      "data.sqlite",
      "*.html"
    )
  )

  ui_bullets(c(
    "_" = "Run checks with {.run revdepcheck::revdep_check(num_workers = 4)}."
  ))
  invisible()
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/rmarkdown.R ---
#' Add an RMarkdown Template
#'
#' Adds files and directories necessary to add a custom rmarkdown template to
#' RStudio. It creates:
#' * `inst/rmarkdown/templates/{{template_dir}}`. Main directory.
#' * `skeleton/skeleton.Rmd`. Your template Rmd file.
#' * `template.yml` with basic information filled in.
#'
#' @param template_name The name as printed in the template menu.
#' @param template_dir Name of the directory the template will live in within
#'   `inst/rmarkdown/templates`. If none is provided by the user, it will be
#'   created from `template_name`.
#' @param template_description Sets the value of `description` in
#'   `template.yml`.
#' @param template_create_dir Sets the value of `create_dir` in `template.yml`.
#'
#' @export
#' @examples
#' \dontrun{
#' use_rmarkdown_template()
#' }
use_rmarkdown_template <- function(
  template_name = "Template Name",
  template_dir = NULL,
  template_description = "A description of the template",
  template_create_dir = FALSE
) {
  # Process some of the inputs
  template_dir <- template_dir %||% tolower(asciify(template_name))
  template_create_dir <- as.character(template_create_dir)
  template_dir <- path("inst", "rmarkdown", "templates", template_dir)

  # Scaffold files
  use_directory(path(template_dir, "skeleton"))
  use_template(
    "rmarkdown-template.yml",
    data = list(
      template_dir = template_dir,
      template_name = template_name,
      template_description = template_description,
      template_create_dir = template_create_dir
    ),
    save_as = path(template_dir, "template.yaml")
  )

  use_template(
    "rmarkdown-template.Rmd",
    path(template_dir, "skeleton", "skeleton.Rmd")
  )

  invisible(TRUE)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/roxygen.R ---
#' Use roxygen2 with markdown
#'
#' If you are already using roxygen2, but not with markdown, you'll need to use
#' [roxygen2md](https://roxygen2md.r-lib.org) to convert existing Rd expressions
#' to markdown. The conversion is not perfect, so make sure to check the
#' results.
#'
#' @param overwrite Whether to overwrite an existing `Roxygen` field in
#'   `DESCRIPTION` with `"list(markdown = TRUE)"`.
#'
#'
#' @export
use_roxygen_md <- function(overwrite = FALSE) {
  check_installed("roxygen2")

  if (!uses_roxygen()) {
    roxy_ver <- as.character(utils::packageVersion("roxygen2"))

    proj_desc_field_update(
      "Roxygen",
      "list(markdown = TRUE)",
      overwrite = FALSE
    )
    proj_desc_field_update("RoxygenNote", roxy_ver, overwrite = FALSE)
    ui_bullets(c("_" = "Run {.run devtools::document()}."))
    return(invisible())
  }

  already_setup <- uses_roxygen_md()

  if (isTRUE(already_setup)) {
    return(invisible())
  }

  if (isFALSE(already_setup) || isTRUE(overwrite)) {
    proj_desc_field_update("Roxygen", "list(markdown = TRUE)", overwrite = TRUE)

    check_installed("roxygen2md")
    ui_bullets(c(
      "_" = "Run {.run roxygen2md::roxygen2md()} to convert existing Rd
             comments to markdown."
    ))
    if (!uses_git()) {
      ui_bullets(c(
        "!" = "Consider using Git for greater visibility into and control over
               the conversion process."
      ))
    }
    ui_bullets(c("_" = "Run {.run devtools::document()} when you're done."))

    return(invisible())
  }

  ui_abort(c(
    "DESCRIPTION already has a {.field Roxygen} field.",
    "Delete that field and try again or call {.code use_roxygen_md(overwrite = TRUE)}."
  ))

  invisible()
}

# FALSE: no Roxygen field
# TRUE: matches regex targetting 'markdown = TRUE', with some whitespace slop
# NA: everything else
uses_roxygen_md <- function() {
  desc <- proj_desc()

  if (!desc$has_fields("Roxygen")) {
    return(FALSE)
  }

  roxygen <- desc$get_field("Roxygen", "")
  if (grepl("markdown\\s*=\\s*TRUE", roxygen)) {
    TRUE
  } else {
    NA
  }
}

uses_roxygen <- function() {
  proj_desc()$has_fields("RoxygenNote")
}

roxygen_ns_append <- function(tag) {
  block_append(
    tag,
    glue("#' {tag}"),
    path = proj_path(package_doc_path()),
    block_start = "## usethis namespace: start",
    block_end = "## usethis namespace: end",
    block_suffix = "NULL",
    sort = TRUE
  )
}

roxygen_ns_show <- function() {
  block_show(
    path = proj_path(package_doc_path()),
    block_start = "## usethis namespace: start",
    block_end = "## usethis namespace: end"
  )
}

roxygen_remind <- function() {
  ui_bullets(c(
    "_" = "Run {.run devtools::document()} to update {.path {pth('NAMESPACE')}}."
  ))
  TRUE
}

roxygen_update_ns <- function(load = is_interactive()) {
  ui_bullets(c("v" = "Writing {.path {pth('NAMESPACE')}}."))
  utils::capture.output(
    suppressMessages(roxygen2::roxygenise(proj_get(), "namespace"))
  )

  if (load) {
    ui_bullets(c("v" = "Loading {.pkg {project_name()}}."))
    pkgload::load_all(path = proj_get(), quiet = TRUE)
  }

  TRUE
}

# Checkers ----------------------------------------------------------------

check_uses_roxygen <- function(whos_asking) {
  force(whos_asking)

  if (uses_roxygen()) {
    return(invisible())
  }

  whos_asking_fn <- sub("()", "", whos_asking, fixed = TRUE)
  ui_abort(c(
    "Package {.pkg {project_name()}} does not use roxygen2.",
    "{.fun {whos_asking_fn}} can not work without it.",
    "You might just need to run {.run devtools::document()} once, then try again."
  ))
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/rprofile.R ---
#' Helpers to make useful changes to `.Rprofile`
#'
#' @description
#' All functions open your `.Rprofile` and give you the code you need to
#' paste in.
#'
#' * `use_devtools()`: makes devtools available in interactive sessions.
#' * `use_usethis()`: makes usethis available in interactive sessions.
#' * `use_reprex()`: makes reprex available in interactive sessions.
#' * `use_conflicted()`:  makes conflicted available in interactive sessions.
#' * `use_partial_warnings()`: warns on partial matches.
#'
#' @name rprofile-helper
NULL

#' @rdname rprofile-helper
#' @export
use_conflicted <- function() {
  use_rprofile_package("conflicted")
}

#' @rdname rprofile-helper
#' @export
use_reprex <- function() {
  use_rprofile_package("reprex")
}

#' @rdname rprofile-helper
#' @export
use_usethis <- function() {
  use_rprofile_package("usethis")
}

#' @rdname rprofile-helper
#' @export
use_devtools <- function() {
  use_rprofile_package("devtools")
}

use_rprofile_package <- function(package) {
  check_installed(package)
  ui_bullets(c(
    "_" = "Include this code in {.path .Rprofile} to make {.pkg {package}}
           available in all interactive sessions:"
  ))
  ui_code_snippet(
    "
    if (interactive()) {{
      suppressMessages(require({package}))
    }}"
  )
  edit_r_profile("user")
}

#' @rdname rprofile-helper
#' @export
use_partial_warnings <- function() {
  ui_bullets(c(
    "_" = "Include this code in {.path .Rprofile} to warn on partial matches:"
  ))
  ui_code_snippet(
    "
    options(
      warnPartialMatchArgs = TRUE,
      warnPartialMatchDollar = TRUE,
      warnPartialMatchAttr = TRUE
    )"
  )
  edit_r_profile("user")
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/rstudio.R ---
#' Add RStudio Project infrastructure
#'
#' It is likely that you want to use [create_project()] or [create_package()]
#' instead of `use_rstudio()`! Both `create_*()` functions can add RStudio
#' Project infrastructure to a pre-existing project or package. `use_rstudio()`
#' is mostly for internal use or for those creating a usethis-like package for
#' their organization. It does the following in the current project, often after
#' executing `proj_set(..., force = TRUE)`:
#'   * Creates an `.Rproj` file
#'   * Adds RStudio files to `.gitignore`
#'   * Adds RStudio files to `.Rbuildignore`, if project is a package
#'
#' @param line_ending Line ending
#' @param reformat If `TRUE`, the `.Rproj` is setup with common options that
#'   reformat files on save: adding a trailing newline, trimming trailing
#'   whitespace, and setting the line-ending. This is best practice for
#'   new projects.
#'
#'   If `FALSE`, these options are left unset, which is more appropriate when
#'   you're contributing to someone else's project that does not have its own
#'   `.Rproj` file.
#' @export
use_rstudio <- function(line_ending = c("posix", "windows"), reformat = TRUE) {
  line_ending <- arg_match(line_ending)
  line_ending <- c("posix" = "Posix", "windows" = "Windows")[[line_ending]]

  rproj_file <- paste0(project_name(), ".Rproj")
  new <- use_template(
    "template.Rproj",
    save_as = rproj_file,
    data = list(
      line_ending = line_ending,
      is_pkg = is_package(),
      reformat = reformat
    ),
    ignore = is_package()
  )

  use_git_ignore(".Rproj.user")
  if (is_package()) {
    use_build_ignore(".Rproj.user")
  }

  invisible(new)
}

#' Don't save/load user workspace between sessions
#'
#' R can save and reload the user's workspace between sessions via an `.RData`
#' file in the current directory. However, long-term reproducibility is enhanced
#' when you turn this feature off and clear R's memory at every restart.
#' Starting with a blank slate provides timely feedback that encourages the
#' development of scripts that are complete and self-contained. More detail can
#' be found in the blog post [Project-oriented
#' workflow](https://www.tidyverse.org/blog/2017/12/workflow-vs-script/).
#'
#' @inheritParams edit
#'
#' @export
use_blank_slate <- function(scope = c("user", "project")) {
  scope <- match.arg(scope)

  if (scope == "user") {
    use_rstudio_preferences(
      save_workspace = "never",
      load_workspace = FALSE
    )
  } else {
    rproj_fields <- modify_rproj(
      rproj_path(),
      list(RestoreWorkspace = "No", SaveWorkspace = "No")
    )
    write_utf8(rproj_path(), serialize_rproj(rproj_fields))
    restart_rstudio("Restart RStudio with a blank slate?")
  }

  invisible()
}

# Is base_path an RStudio Project or inside an RStudio Project?
is_rstudio_project <- function(base_path = proj_get()) {
  length(rproj_paths(base_path)) == 1
}

rproj_paths <- function(base_path, recurse = FALSE) {
  dir_ls(base_path, regexp = "[.]Rproj$", recurse = recurse)
}

# Return path to single .Rproj or die trying
rproj_path <- function(base_path = proj_get(), call = caller_env()) {
  rproj <- rproj_paths(base_path)
  if (length(rproj) == 1) {
    rproj
  } else if (length(rproj) == 0) {
    name <- project_name(base_path)
    cli::cli_abort("{.val {name}} is not an RStudio Project.", call = call)
  } else {
    name <- project_name(base_path)
    cli::cli_abort(
      c(
        "{.val {name}} must contain a single .Rproj file.",
        i = "Found {.file {path_rel(rproj, base_path)}}."
      ),
      call = call
    )
  }
}

# Is base_path open in RStudio?
in_rstudio <- function(base_path = proj_get()) {
  if (!rstudio_available()) {
    return(FALSE)
  }

  if (!rstudioapi::hasFun("getActiveProject")) {
    return(FALSE)
  }

  proj <- rstudioapi::getActiveProject()

  if (is.null(proj)) {
    return(FALSE)
  }

  path_real(proj) == path_real(base_path)
}

# So we can override the default with a mock
rstudio_available <- function() {
  rstudioapi::isAvailable()
}

in_rstudio_server <- function() {
  if (!rstudio_available()) {
    return(FALSE)
  }
  identical(rstudioapi::versionInfo()$mode, "server")
}

parse_rproj <- function(file) {
  lines <- as.list(read_utf8(file))
  has_colon <- grepl(":", lines)
  fields <- lapply(lines[has_colon], function(x) strsplit(x, split = ": ")[[1]])
  lines[has_colon] <- vapply(fields, `[[`, "character", 2)
  names(lines)[has_colon] <- vapply(fields, `[[`, "character", 1)
  names(lines)[!has_colon] <- ""
  lines
}

modify_rproj <- function(file, update) {
  utils::modifyList(parse_rproj(file), update)
}

serialize_rproj <- function(fields) {
  named <- nzchar(names(fields))
  as.character(ifelse(named, paste0(names(fields), ": ", fields), fields))
}

# Must be last command run
restart_rstudio <- function(message = NULL) {
  if (!in_rstudio(proj_get())) {
    return(FALSE)
  }

  if (!is_interactive()) {
    return(FALSE)
  }

  if (!is.null(message)) {
    ui_bullets(message)
  }

  if (!rstudioapi::hasFun("openProject")) {
    return(FALSE)
  }

  if (ui_nah("Restart now?")) {
    return(FALSE)
  }

  rstudioapi::openProject(proj_get())
}

rstudio_git_tickle <- function() {
  if (uses_git() && rstudioapi::hasFun("executeCommand")) {
    rstudioapi::executeCommand("vcsRefresh")
  }
  invisible()
}

rstudio_config_path <- function(...) {
  if (is_windows()) {
    # https://github.com/r-lib/usethis/issues/1293
    base <- rappdirs::user_config_dir("RStudio", appauthor = NULL)
  } else {
    # RStudio only uses windows/unix conventions, not mac
    base <- rappdirs::user_config_dir("rstudio", os = "unix")
  }
  path(base, ...)
}

#' Set global RStudio preferences
#'
#' This function allows you to set global RStudio preferences, achieving the
#' same effect programmatically as clicking buttons in RStudio's Global Options.
#' You can find a list of configurable properties at
#' <https://docs.posit.co/ide/server-pro/reference/session_user_settings.html>.
#'
#' @export
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Property-value pairs.
#' @return A named list of the previous values, invisibly.
use_rstudio_preferences <- function(...) {
  new <- dots_list(..., .homonyms = "last")
  if (length(new) > 0 && !is_named(new)) {
    cli::cli_abort("All arguments in {.arg ...} must be named.")
  }

  json <- rstudio_prefs_read()
  old <- json[names(new)]

  for (name in names(new)) {
    val <- new[[name]]

    if (identical(json[[name]], val)) {
      next
    }

    ui_bullets(c(
      "v" = "Setting RStudio preference {.field {name}} to {.val {val}}."
    ))
    json[[name]] <- val
  }

  rstudio_prefs_write(json)
  invisible(old)
}

rstudio_prefs_read <- function() {
  path <- rstudio_config_path("rstudio-prefs.json")
  if (file_exists(path)) {
    jsonlite::read_json(path)
  } else {
    list()
  }
}

rstudio_prefs_write <- function(json) {
  path <- rstudio_config_path("rstudio-prefs.json")
  create_directory(path_dir(path))
  jsonlite::write_json(json, path, auto_unbox = TRUE, pretty = TRUE)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/sitrep.R ---
#' Report working directory and usethis/RStudio project
#'
#' @description `proj_sitrep()` reports
#'   * current working directory
#'   * the active usethis project
#'   * the active RStudio Project
#'
#' @description Call this function if things seem weird and you're not sure
#'   what's wrong or how to fix it. Usually, all three of these should coincide
#'   (or be unset) and `proj_sitrep()` provides suggested commands for getting
#'   back to this happy state.
#'
#' @return A named list, with S3 class `sitrep` (for printing purposes),
#'   reporting current working directory, active usethis project, and active
#'   RStudio Project
#' @export
#' @family project functions
#' @examples
#' proj_sitrep()
proj_sitrep <- function() {
  out <- list(
    working_directory = getwd(),
    active_usethis_proj = if (proj_active()) proj_get(),
    active_rstudio_proj = if (rstudioapi::hasFun("getActiveProject")) {
      rstudioapi::getActiveProject()
    }
    ## TODO(?): address home directory to help clarify fs issues on Windows?
    ## home_usethis = fs::path_home(),
    ## home_r = normalizePath("~")
  )
  out <- ifelse(map_lgl(out, is.null), out, as.character(path_tidy(out)))
  structure(out, class = "sitrep")
}

#' @export
print.sitrep <- function(x, ...) {
  keys <- format(names(x), justify = "right")
  purrr::walk2(keys, x, kv_line)

  rstudio_proj_is_active <- !is.null(x[["active_rstudio_proj"]])
  usethis_proj_is_active <- !is.null(x[["active_usethis_proj"]])

  rstudio_proj_is_not_wd <- rstudio_proj_is_active &&
    x[["working_directory"]] != x[["active_rstudio_proj"]]
  usethis_proj_is_not_wd <- usethis_proj_is_active &&
    x[["working_directory"]] != x[["active_usethis_proj"]]
  usethis_proj_is_not_rstudio_proj <- usethis_proj_is_active &&
    rstudio_proj_is_active &&
    x[["active_rstudio_proj"]] != x[["active_usethis_proj"]]

  if (rstudio_available() && !rstudio_proj_is_active) {
    ui_bullets(c(
      "i" = "You are working in RStudio, but are not in an RStudio Project.",
      "i" = "A Project-based workflow offers many advantages. Read more at:",
      " " = "{.url https://docs.posit.co/ide/user/ide/guide/code/projects.html}",
      " " = "{.url https://rstats.wtf/projects}"
    ))
  }

  if (!usethis_proj_is_active) {
    ui_bullets(c(
      "i" = "There is currently no active {.pkg usethis} project.",
      "i" = "{.pkg usethis} attempts to activate a project upon first need.",
      "_" = "Call {.run usethis::proj_get()} to initiate project discovery.",
      "_" = 'Call {.code proj_set("path/to/project")} or
             {.code proj_activate("path/to/project")} to provide an explicit
             path.'
    ))
  }

  if (usethis_proj_is_not_wd) {
    ui_bullets(c(
      "i" = "Your working directory is not the same as the active usethis project.",
      "_" = "Set working directory to the project: {.code setwd(proj_get())}.",
      "_" = "Set project to working directory: {.code usethis::proj_set(getwd())}."
    ))
  }

  if (rstudio_proj_is_not_wd) {
    ui_bullets(c(
      "i" = "Your working directory is not the same as the active RStudio Project.",
      "_" = "Set working directory to the Project:
             {.code setwd(rstudioapi::getActiveProject())}."
    ))
  }

  if (usethis_proj_is_not_rstudio_proj) {
    ui_bullets(c(
      "i" = "Your active RStudio Project is not the same as the active
             {.pkg usethis} project.",
      "_" = "Set active {.pkg usethis} project to RStudio Project:
             {.code usethis::proj_set(rstudioapi::getActiveProject())}.",
      "_" = "Restart RStudio in the active {.pkg usethis} project:
             {.code rstudioapi::openProject(usethis::proj_get())}.",
      "_" = "Open the active {.pkg usethis} project in a new instance of RStudio:
             {.code usethis::proj_activate(usethis::proj_get())}."
    ))
  }

  invisible(x)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/spelling.R ---
#' Use spell check
#'
#' Adds a unit test to automatically run a spell check on documentation and,
#' optionally, vignettes during `R CMD check`, using the
#' [spelling][spelling::spell_check_package] package. Also adds a `WORDLIST`
#' file to the package, which is a dictionary of whitelisted words. See
#' [spelling::wordlist] for details.
#'
#' @param vignettes Logical, `TRUE` to spell check all `rmd` and `rnw` files in
#'   the `vignettes/` folder.
#' @param lang Preferred spelling language. Usually either `"en-US"` or
#'   `"en-GB"`.
#' @param error Logical, indicating whether the unit test should fail if
#'   spelling errors are found. Defaults to `FALSE`, which does not error, but
#'   prints potential spelling errors
#' @export
use_spell_check <- function(vignettes = TRUE, lang = "en-US", error = FALSE) {
  check_is_package("use_spell_check()")
  check_installed("spelling")
  use_dependency("spelling", "Suggests")
  proj_desc_field_update("Language", lang, overwrite = TRUE)
  spelling::spell_check_setup(
    pkg = proj_get(),
    vignettes = vignettes,
    lang = lang,
    error = error
  )
  ui_bullets(c("_" = "Run {.run devtools::check()} to trigger spell check."))
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/template.R ---
#' Use a usethis-style template
#'
#' Creates a file from data and a template found in a package. Provides control
#' over file name, the addition to `.Rbuildignore`, and opening the file for
#' inspection.
#'
#' This function can be used as the engine for a templating function in other
#' packages. The `template` argument is used along with the `package` argument
#' to derive the path to your template file; it will be expected at
#' `fs::path_package(package = package, "templates", template)`. We use
#' `fs::path_package()` instead of `base::system.file()` so that path
#' construction works even in a development workflow, e.g., works with
#' `devtools::load_all()` or `pkgload::load_all()`. *Note this describes the
#' behaviour of `fs::path_package()` in fs v1.2.7.9001 and higher.*
#'
#' To interpolate your data into the template, supply a list using
#' the `data` argument. Internally, this function uses
#' [whisker::whisker.render()] to combine your template file with your data.
#'
#' @param template Path to template file relative to `templates/` directory
#'   within `package`; see details.
#' @param save_as Path of file to create, relative to root of active project.
#'   Defaults to `template`
#' @param data A list of data passed to the template.
#' @param ignore Should the newly created file be added to `.Rbuildignore`?
#' @param open Open the newly created file for editing? Happens in RStudio, if
#'   applicable, or via [utils::file.edit()] otherwise.
#' @param package Name of the package where the template is found.
#' @return A logical vector indicating if file was modified.
#' @export
#' @examples
#' \dontrun{
#'   # Note: running this will write `NEWS.md` to your working directory
#'   use_template(
#'     template = "NEWS.md",
#'     data = list(Package = "acme", Version = "1.2.3"),
#'     package = "usethis"
#'   )
#' }
use_template <- function(
  template,
  save_as = template,
  data = list(),
  ignore = FALSE,
  open = FALSE,
  package = "usethis"
) {
  template_contents <- render_template(template, data, package = package)
  new <- write_over(proj_path(save_as), template_contents)

  if (ignore) {
    use_build_ignore(save_as)
  }

  if (open && new) {
    edit_file(proj_path(save_as))
  }

  invisible(new)
}

render_template <- function(template, data = list(), package = "usethis") {
  template_path <- find_template(template, package = package)
  strsplit(whisker::whisker.render(read_utf8(template_path), data), "\n")[[1]]
}

find_template <- function(template_name, package = "usethis") {
  check_installed(package)
  path <- tryCatch(
    path_package(package = package, "templates", template_name),
    error = function(e) ""
  )
  if (identical(path, "")) {
    ui_abort(
      "
      Could not find template {.val {template_name}} in package {.pkg package}
      package."
    )
  }
  path
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/test.R ---
#' Sets up overall testing infrastructure
#'
#' Creates `tests/testthat/`, `tests/testthat.R`, and adds the testthat package
#' to the Suggests field. Learn more in <https://r-pkgs.org/testing-basics.html>
#'
#' @param edition testthat edition to use. Defaults to the latest edition, i.e.
#'   the major version number of the currently installed testthat.
#' @param parallel Should tests be run in parallel? This feature appeared in
#'   testthat 3.0.0; see <https://testthat.r-lib.org/articles/parallel.html> for
#'   details and caveats.
#' @seealso [use_test()] to create individual test files
#' @export
#' @examples
#' \dontrun{
#' use_testthat()
#'
#' use_test()
#'
#' use_test("something-management")
#' }
use_testthat <- function(edition = NULL, parallel = FALSE) {
  use_testthat_impl(edition, parallel = parallel)

  ui_bullets(c(
    "_" = "Call {.run usethis::use_test()} to initialize a basic test file and
           open it for editing."
  ))
}

use_testthat_impl <- function(edition = NULL, parallel = FALSE) {
  check_installed("testthat", version = "2.1.0")

  if (is_package()) {
    edition <- check_edition(edition)

    use_dependency("testthat", "Suggests", paste0(edition, ".0.0"))
    proj_desc_field_update(
      "Config/testthat/edition",
      as.character(edition),
      overwrite = TRUE
    )

    if (parallel) {
      proj_desc_field_update(
        "Config/testthat/parallel",
        "true",
        overwrite = TRUE
      )
    } else {
      proj_desc()$del("Config/testthat/parallel")
    }
  } else {
    if (!is.null(edition)) {
      ui_abort("Can't declare {.pkg testthat} edition outside of a package.")
    }
  }

  use_directory(path("tests", "testthat"))
  use_template(
    "testthat.R",
    save_as = path("tests", "testthat.R"),
    data = list(name = project_name())
  )
}

check_edition <- function(edition = NULL) {
  version <- testthat_version()[[1, c(1, 2)]]
  if (version[[2]] == "99") {
    version <- version[[1]] + 1L
  } else {
    version <- version[[1]]
  }

  if (is.null(edition)) {
    version
  } else {
    if (!is.numeric(edition) || length(edition) != 1) {
      ui_abort("{.arg edition} must be a single number.")
    }
    if (edition > version) {
      vers <- testthat_version()
      ui_abort(
        "
        {.var edition} ({edition}) not available in installed verion of
        {.pkg testthat} ({vers})."
      )
    }
    as.integer(edition)
  }
}

# wrapping so we can mock this in tests
testthat_version <- function() {
  utils::packageVersion("testthat")
}

uses_testthat <- function() {
  paths <- proj_path(c(path("inst", "tests"), path("tests", "testthat")))
  any(dir_exists(paths))
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/tibble.R ---
#' Prepare to return a tibble
#'
#' @description
#'
#' `r lifecycle::badge("questioning")`
#'
#' Does minimum setup such that a tibble returned by your package
#' is handled using the tibble method for generics like `print()` or \code{[}.
#' Presumably you care about this if you've chosen to store and expose an
#' object with class `tbl_df`. Specifically:
#'   * Check that the active package uses roxygen2
#'   * Add the tibble package to "Imports" in `DESCRIPTION`
#'   * Prepare the roxygen directive necessary to import at least one function
#'     from tibble:
#'     - If possible, the directive is inserted into existing package-level
#'       documentation, i.e. the roxygen snippet created by [use_package_doc()]
#'     - Otherwise, we issue advice on where the user should add the directive
#'
#' This is necessary when your package returns a stored data object that has
#' class `tbl_df`, but the package code does not make direct use of functions
#' from the tibble package. If you do nothing, the tibble namespace is not
#' necessarily loaded and your tibble may therefore be printed and subsetted
#' like a base `data.frame`.
#'
#' @export
#' @examples
#' \dontrun{
#' use_tibble()
#' }
use_tibble <- function() {
  check_is_package("use_tibble()")
  check_uses_roxygen("use_tibble()")

  created <- use_import_from("tibble", "tibble")

  ui_bullets(c("_" = "Document a returned tibble like so:"))
  ui_code_snippet(
    "#' @return a [tibble][tibble::tibble-package]",
    language = "",
    copy = FALSE
  )

  invisible(created)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/tidyverse.R ---
#' Helpers for tidyverse development
#'
#' These helpers follow tidyverse conventions which are generally a little
#' stricter than the defaults, reflecting the need for greater rigor in
#' commonly used packages.
#'
#' @details
#'
#' * `create_tidy_package()`: creates a new package, immediately applies as many
#' of the tidyverse conventions as possible, issues a few reminders, and
#' activates the new package.
#'
#' * `use_tidy_dependencies()`: sets up standard dependencies used by all
#'   tidyverse packages (except packages that are designed to be dependency free).
#'
#' * `use_tidy_description()`: puts fields in standard order and alphabetises
#'   dependencies.
#'
#' * `use_tidy_eval()`: imports a standard set of helpers to facilitate
#'   programming with the tidy eval toolkit.
#'
#' * `use_tidy_style()`: styles source code according to the [tidyverse style
#' guide](https://style.tidyverse.org). This function will overwrite files! See
#' below for usage advice.
#'
#' * `use_tidy_contributing()`: adds standard tidyverse contributing guidelines.
#'
#' * `use_tidy_issue_template()`: adds a standard tidyverse issue template.
#'
#' * `use_tidy_release_test_env()`: updates the test environment section in
#'   `cran-comments.md`.
#'
#' * `use_tidy_support()`: adds a standard description of support resources for
#'    the tidyverse.
#'
#' * `use_tidy_coc()`: equivalent to `use_code_of_conduct()`, but puts the
#'    document in a `.github/` subdirectory.
#'
#' * `use_tidy_github()`: convenience wrapper that calls
#' `use_tidy_contributing()`, `use_tidy_issue_template()`, `use_tidy_support()`,
#' `use_tidy_coc()`.
#'
#' * [use_tidy_github_labels()] calls `use_github_labels()` to implement
#'   tidyverse conventions around GitHub issue label names and colours.
#'
#' * `use_tidy_upkeep_issue()` creates an issue containing a checklist of
#'   actions to bring your package up to current tidyverse standards. Also
#'   records the current date in the `Config/usethis/last-upkeep` field in
#'   `DESCRIPTION`.
#'
#' * `use_tidy_logo()` calls `use_logo()` on the appropriate hex sticker PNG
#'   file at <https://github.com/rstudio/hex-stickers>.
#'
#' @name tidyverse
NULL

#' @export
#' @rdname tidyverse
#' @inheritParams create_package
#' @inheritParams licenses
create_tidy_package <- function(path, copyright_holder = NULL) {
  path <- create_package(path, rstudio = TRUE, open = FALSE)
  local_project(path)

  use_testthat()
  use_mit_license(copyright_holder)
  use_tidy_description()

  use_readme_rmd(open = FALSE)
  use_lifecycle_badge("experimental")
  use_cran_badge()

  use_cran_comments(open = FALSE)

  ui_bullets(c("i" = "In the new package, remember to do:"))
  ui_code_snippet(
    "
    usethis::use_git()
    usethis::use_github()
    usethis::use_tidy_github()
    usethis::use_tidy_github_actions()
    usethis::use_tidy_github_labels()
    usethis::use_pkgdown_github_pages()
  "
  )

  proj_activate(path)
}


#' @export
#' @rdname tidyverse
use_tidy_description <- function() {
  desc <- proj_desc()
  tidy_desc(desc)
  desc$write()

  invisible(TRUE)
}

#' @export
#' @rdname tidyverse
use_tidy_dependencies <- function() {
  check_has_package_doc("use_tidy_dependencies()")

  use_dependency("rlang", "Imports")
  use_dependency("lifecycle", "Imports")
  use_dependency("cli", "Imports")
  use_dependency("glue", "Imports")
  use_dependency("withr", "Imports")

  # standard imports
  imports <- any(
    roxygen_ns_append("@import rlang"),
    roxygen_ns_append("@importFrom glue glue"),
    roxygen_ns_append("@importFrom lifecycle deprecated")
  )
  if (imports) {
    roxygen_update_ns()
  }

  # add badges; we don't need the details
  ui_silence(use_lifecycle())

  # If needed, copy in lightweight purrr compatibility layer
  if (!proj_desc()$has_dep("purrr")) {
    use_directory("R")
    use_standalone("r-lib/rlang", "purrr")
  }

  invisible()
}

#' @export
#' @rdname tidyverse
use_tidy_contributing <- function() {
  use_dot_github()
  data <- list(
    Package = project_name(),
    github_spec = target_repo_spec(ask = FALSE)
  )
  use_template(
    "tidy-contributing.md",
    path(".github", "CONTRIBUTING.md"),
    data = data
  )
}

#' @export
#' @rdname tidyverse
use_tidy_support <- function() {
  use_dot_github()
  data <- list(
    Package = project_name(),
    github_spec = target_repo_spec(ask = FALSE)
  )
  use_template(
    "tidy-support.md",
    path(".github", "SUPPORT.md"),
    data = data
  )
}


#' @export
#' @rdname tidyverse
use_tidy_issue_template <- function() {
  use_dot_github()
  use_directory(path(".github", "ISSUE_TEMPLATE"))
  use_template(
    "tidy-issue.md",
    path(".github", "ISSUE_TEMPLATE", "issue_template.md")
  )
}

#' @export
#' @rdname tidyverse
use_tidy_coc <- function() {
  old_top_level_coc <- proj_path(c("CODE_OF_CONDUCT.md", "CONDUCT.md"))
  old <- file_exists(old_top_level_coc)
  if (any(old)) {
    file_delete(old_top_level_coc[old])
  }

  use_dot_github()
  use_coc(contact = "codeofconduct@posit.co", path = ".github")
}

#' @export
#' @rdname tidyverse
use_tidy_github <- function() {
  use_dot_github()
  use_tidy_contributing()
  use_tidy_issue_template()
  use_tidy_support()
  use_tidy_coc()
}

use_dot_github <- function(ignore = TRUE) {
  use_directory(".github", ignore = ignore)
  use_git_ignore("*.html", directory = ".github")
}

#' Identify contributors via GitHub activity
#'
#' Derives a list of GitHub usernames, based on who has opened issues or pull
#' requests. Used to populate the acknowledgment section of package release blog
#' posts at <https://www.tidyverse.org/blog/>. If no arguments are given, we
#' retrieve all contributors to the active project since its last (GitHub)
#' release. Unexported helper functions, `releases()` and `ref_df()` can be
#' useful interactively to get a quick look at release tag names and a data
#' frame about refs (defaulting to releases), respectively.
#'
#' @param repo_spec Optional GitHub repo specification in any form accepted for
#'   the `repo_spec` argument of [create_from_github()] (plain spec or a browser
#'   or Git URL). A URL specification is the only way to target a GitHub host
#'   other than `"github.com"`, which is the default.
#' @param from,to GitHub ref (i.e., a SHA, tag, or release) or a timestamp in
#'   ISO 8601 format, specifying the start or end of the interval of interest,
#'   in the sense of `[from, to]`. Examples: "08a560d", "v1.3.0",
#'   "2018-02-24T00:13:45Z", "2018-05-01". When `from = NULL, to = NULL`, we set
#'   `from` to the timestamp of the most recent (GitHub) release. Otherwise,
#'   `NULL` means "no bound".
#'
#' @return A character vector of GitHub usernames, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' # active project, interval = since the last release
#' use_tidy_thanks()
#'
#' # active project, interval = since a specific datetime
#' use_tidy_thanks(from = "2020-07-24T00:13:45Z")
#'
#' # r-lib/usethis, interval = since a certain date
#' use_tidy_thanks("r-lib/usethis", from = "2020-08-01")
#'
#' # r-lib/usethis, up to a specific release
#' use_tidy_thanks("r-lib/usethis", from = NULL, to = "v1.1.0")
#'
#' # r-lib/usethis, since a specific commit, up to a specific date
#' use_tidy_thanks("r-lib/usethis", from = "08a560d", to = "2018-05-14")
#'
#' # r-lib/usethis, but with copy/paste of a browser URL
#' use_tidy_thanks("https://github.com/r-lib/usethis")
#' }
use_tidy_thanks <- function(repo_spec = NULL, from = NULL, to = NULL) {
  repo_spec <- repo_spec %||% target_repo_spec()
  parsed_repo_spec <- parse_repo_url(repo_spec)
  repo_spec <- parsed_repo_spec$repo_spec
  # this is the most practical way to propagate `host` to downstream helpers
  if (!is.null(parsed_repo_spec$host)) {
    withr::local_envvar(c(GITHUB_API_URL = parsed_repo_spec$host))
  }

  if (is.null(to)) {
    from <- from %||% releases(repo_spec)[[1]]
  }

  from_timestamp <- as_timestamp(repo_spec, x = from) %||% "2008-01-01"
  to_timestamp <- as_timestamp(repo_spec, x = to)
  ui_bullets(c(
    "i" = "Looking for contributors from {as.Date(from_timestamp)} to
           {to_timestamp %||% 'now'}."
  ))

  res <- gh::gh(
    "/repos/{owner}/{repo}/issues",
    owner = spec_owner(repo_spec),
    repo = spec_repo(repo_spec),
    since = from_timestamp,
    state = "all",
    filter = "all",
    .limit = Inf
  )
  if (length(res) < 1) {
    ui_bullets(c("x" = "No matching issues/PRs found."))
    return(invisible())
  }

  creation_time <- function(x) {
    as.POSIXct(map_chr(x, "created_at"))
  }

  res <- res[creation_time(res) >= as.POSIXct(from_timestamp)]

  if (!is.null(to_timestamp)) {
    res <- res[creation_time(res) <= as.POSIXct(to_timestamp)]
  }
  if (length(res) == 0) {
    ui_bullets(c("x" = "No matching issues/PRs found."))
    return(invisible())
  }

  contributors <- sort(unique(map_chr(res, c("user", "login"))))
  contrib_link <- glue(
    "[&#x0040;{contributors}](https://github.com/{contributors})"
  )

  ui_bullets(c("v" = "Found {length(contributors)} contributors:"))
  ui_code_snippet(
    glue_collapse(contrib_link, sep = ", ", last = ", and ") + glue("."),
    language = ""
  )

  invisible(contributors)
}

## if x appears to be a timestamp, pass it through
## otherwise, assume it's a ref and look up its timestamp
as_timestamp <- function(repo_spec, x = NULL) {
  if (is.null(x)) {
    return(NULL)
  }
  as_POSIXct <- try(as.POSIXct(x), silent = TRUE)
  if (inherits(as_POSIXct, "POSIXct")) {
    return(x)
  }
  ui_bullets(c("v" = "Resolving timestamp for ref {.val {x}}."))
  ref_df(repo_spec, refs = x)$timestamp
}

## returns a data frame on GitHub refs, defaulting to all releases
ref_df <- function(repo_spec, refs = NULL) {
  check_name(repo_spec)
  check_character(refs, allow_null = TRUE)
  refs <- refs %||% releases(repo_spec)
  if (is.null(refs)) {
    return(NULL)
  }
  get_thing <- function(thing) {
    gh::gh(
      "/repos/{owner}/{repo}/commits/{thing}",
      owner = spec_owner(repo_spec),
      repo = spec_repo(repo_spec),
      thing = thing
    )
  }
  res <- lapply(refs, get_thing)
  data.frame(
    ref = refs,
    sha = substr(map_chr(res, "sha"), 1, 7),
    timestamp = map_chr(res, c("commit", "committer", "date")),
    stringsAsFactors = FALSE
  )
}

## returns character vector of release tag names
releases <- function(repo_spec) {
  check_name(repo_spec)
  res <- gh::gh(
    "/repos/{owner}/{repo}/releases",
    owner = spec_owner(repo_spec),
    repo = spec_repo(repo_spec)
  )
  if (length(res) < 1) {
    return(NULL)
  }
  map_chr(res, "tag_name")
}

## approaches based on available.packages() and/or installed.packages() present
## several edge cases, requirements, and gotchas
## for this application, hard-wiring seems to be "good enough"
base_and_recommended <- function() {
  # base_pkgs <- as.vector(installed.packages(priority = "base")[, "Package"])
  # av <- available.packages()
  # keep <- av[ , "Priority", drop = TRUE] %in% "recommended"
  # rec_pkgs <- unname(av[keep, "Package", drop = TRUE])
  # dput(sort(unique(c(base_pkgs, rec_pkgs))))
  c(
    "base",
    "boot",
    "class",
    "cluster",
    "codetools",
    "compiler",
    "datasets",
    "foreign",
    "graphics",
    "grDevices",
    "grid",
    "KernSmooth",
    "lattice",
    "MASS",
    "Matrix",
    "methods",
    "mgcv",
    "nlme",
    "nnet",
    "parallel",
    "rpart",
    "spatial",
    "splines",
    "stats",
    "stats4",
    "survival",
    "tcltk",
    "tools",
    "utils"
  )
}

#' @rdname tidyverse
#' @inheritParams use_logo
#' @export
use_tidy_logo <- function(geometry = "240x278", retina = TRUE) {
  if (!is_posit_pkg()) {
    ui_abort("This function only works for Posit packages.")
  }

  tf <- withr::local_tempfile(fileext = ".png")

  gh::gh(
    "/repos/rstudio/hex-stickers/contents/PNG/{pkg}.png/",
    pkg = project_name(),
    .destfile = tf,
    .accept = "application/vnd.github.v3.raw"
  )

  use_logo(tf, geometry = geometry, retina = retina)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/tutorial.R ---
#' Create a learnr tutorial
#'
#' Creates a new tutorial below `inst/tutorials/`. Tutorials are interactive R
#' Markdown documents built with the [`learnr`
#' package](https://rstudio.github.io/learnr/index.html). `use_tutorial()` does
#' this setup:
#'   * Adds learnr to Suggests in `DESCRIPTION`.
#'   * Gitignores `inst/tutorials/*.html` so you don't accidentally track
#'     rendered tutorials.
#'   * Creates a new `.Rmd` tutorial from a template and, optionally, opens it
#'     for editing.
#'   * Adds new `.Rmd` to `.Rbuildignore`.
#'
#' @param name Base for file name to use for new `.Rmd` tutorial. Should consist
#'   only of numbers, letters, `_` and `-`. We recommend using lower case.
#' @param title The human-facing title of the tutorial.
#' @inheritParams use_template
#' @seealso The [learnr package
#'   documentation](https://rstudio.github.io/learnr/index.html).
#' @export
#' @examples
#' \dontrun{
#' use_tutorial("learn-to-do-stuff", "Learn to do stuff")
#' }
use_tutorial <- function(name, title, open = rlang::is_interactive()) {
  check_name(name)
  check_name(title)

  dir_path <- path("inst", "tutorials", name)
  dir_create(dir_path)

  use_directory(dir_path)
  use_git_ignore("*.html", directory = dir_path)
  use_dependency("learnr", "Suggests")

  path <- path(dir_path, asciify(name), ext = "Rmd")
  new <- use_template(
    "tutorial-template.Rmd",
    save_as = path,
    data = list(tutorial_title = title),
    ignore = FALSE,
    open = open
  )

  invisible(new)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/ui-legacy.R ---
#' Legacy functions related to user interface
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'

#'   These functions are now superseded. External users of the `usethis::ui_*()`
#'   functions are encouraged to use the [cli package](https://cli.r-lib.org/)
#'   instead. The cli package did not have the required functionality when the
#'   `usethis::ui_*()` functions were created, but it has had that for a while
#'   now and it's the superior option. There is even a cli vignette about how to
#'   make this transition: `vignette("usethis-ui", package = "cli")`.
#'
#'   usethis itself now uses cli internally for its UI, but these new functions
#'   are not exported and presumably never will be. There is a developer-focused
#'   article on the process of transitioning usethis's own UI to use cli:
#'   [Converting usethis's UI to use cli](https://usethis.r-lib.org/articles/ui-cli-conversion.html).

#' @details
#'
#' The `ui_` functions can be broken down into four main categories:
#'
#' * block styles: `ui_line()`, `ui_done()`, `ui_todo()`, `ui_oops()`,
#'   `ui_info()`.
#' * conditions: `ui_stop()`, `ui_warn()`.
#' * questions: [ui_yeah()], [ui_nope()].
#' * inline styles: `ui_field()`, `ui_value()`, `ui_path()`, `ui_code()`,
#'   `ui_unset()`.
#'
#' The question functions [ui_yeah()] and [ui_nope()] have their own [help
#' page][ui-questions].
#'
#' All UI output (apart from `ui_yeah()`/`ui_nope()` prompts) can be silenced
#' by setting `options(usethis.quiet = TRUE)`. Use [ui_silence()] to silence
#' selected actions.
#'
#' @param x A character vector.
#'
#'   For block styles, conditions, and questions, each element of the
#'   vector becomes a line, and the result is processed by [glue::glue()].
#'   For inline styles, each element of the vector becomes an entry in a
#'   comma separated list.
#' @param .envir Used to ensure that [glue::glue()] gets the correct
#'   environment. For expert use only.
#'
#' @return The block styles, conditions, and questions are called for their
#'   side-effect. The inline styles return a string.
#' @keywords internal
#' @name ui-legacy-functions
#' @examples
#' new_val <- "oxnard"
#' ui_done("{ui_field('name')} set to {ui_value(new_val)}")
#' ui_todo("Redocument with {ui_code('devtools::document()')}")
#'
#' ui_code_block(c(
#'   "Line 1",
#'   "Line 2",
#'   "Line 3"
#' ))
NULL

# Block styles ------------------------------------------------------------

#' @rdname ui-legacy-functions
#' @export
ui_line <- function(x = character(), .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  ui_inform(x)
}

#' @rdname ui-legacy-functions
#' @export
ui_todo <- function(x, .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  ui_legacy_bullet(x, crayon::red(cli::symbol$bullet))
}

#' @rdname ui-legacy-functions
#' @export
ui_done <- function(x, .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  ui_legacy_bullet(x, crayon::green(cli::symbol$tick))
}

#' @rdname ui-legacy-functions
#' @export
ui_oops <- function(x, .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  ui_legacy_bullet(x, crayon::red(cli::symbol$cross))
}

#' @rdname ui-legacy-functions
#' @export
ui_info <- function(x, .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  ui_legacy_bullet(x, crayon::yellow(cli::symbol$info))
}

#' @param copy If `TRUE`, the session is interactive, and the clipr package
#'   is installed, will copy the code block to the clipboard.
#' @rdname ui-legacy-functions
#' @export
ui_code_block <- function(
  x,
  copy = rlang::is_interactive(),
  .envir = parent.frame()
) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)

  block <- indent(x, "  ")
  block <- crayon::silver(block)
  ui_inform(block)

  if (copy && clipr::clipr_available()) {
    x <- crayon::strip_style(x)
    clipr::write_clip(x)
    ui_inform("  [Copied to clipboard]")
  }
}

# Conditions --------------------------------------------------------------

#' @rdname ui-legacy-functions
#' @export
ui_stop <- function(x, .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)

  cnd <- structure(
    class = c("usethis_error", "error", "condition"),
    list(message = x)
  )

  stop(cnd)
}

#' @rdname ui-legacy-functions
#' @export
ui_warn <- function(x, .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)

  warning(x, call. = FALSE, immediate. = TRUE)
}


# Questions ---------------------------------------------------------------
#' User interface - Questions
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'

#' `ui_yeah()` and `ui_nope()` are technically superseded, but, unlike the rest
#' of the legacy [`ui_*()`][ui-legacy-functions] functions, there's not yet a
#' drop-in replacement available in the [cli package](https://cli.r-lib.org/).
#' `ui_yeah()` and `ui_nope()` are no longer used internally in usethis.
#'
#' @inheritParams ui-legacy-functions
#' @param yes A character vector of "yes" strings, which are randomly sampled to
#'   populate the menu.
#' @param no A character vector of "no" strings, which are randomly sampled to
#'   populate the menu.
#' @param n_yes An integer. The number of "yes" strings to include.
#' @param n_no An integer. The number of "no" strings to include.
#' @param shuffle A logical. Should the order of the menu options be randomly
#'   shuffled?
#'
#' @return A logical. `ui_yeah()` returns `TRUE` when the user selects a "yes"
#'   option and `FALSE` otherwise, i.e. when user selects a "no" option or
#'   refuses to make a selection (cancels). `ui_nope()` is the logical opposite
#'   of `ui_yeah()`.
#' @name ui-questions
#' @keywords internal
#' @examples
#' \dontrun{
#' ui_yeah("Do you like R?")
#' ui_nope("Have you tried turning it off and on again?", n_yes = 1, n_no = 1)
#' ui_yeah("Are you sure its plugged in?", yes = "Yes", no = "No", shuffle = FALSE)
#' }
NULL

#' @rdname ui-questions
#' @export
ui_yeah <- function(
  x,
  yes = c(
    "Yes",
    "Definitely",
    "For sure",
    "Yup",
    "Yeah",
    "I agree",
    "Absolutely"
  ),
  no = c("No way", "Not now", "Negative", "No", "Nope", "Absolutely not"),
  n_yes = 1,
  n_no = 2,
  shuffle = TRUE,
  .envir = parent.frame()
) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)

  if (!is_interactive()) {
    ui_stop(c(
      "User input required, but session is not interactive.",
      "Query: {x}"
    ))
  }

  n_yes <- min(n_yes, length(yes))
  n_no <- min(n_no, length(no))

  qs <- c(sample(yes, n_yes), sample(no, n_no))

  if (shuffle) {
    qs <- sample(qs)
  }

  # TODO: should this be ui_inform()?
  # later observation: probably not? you would not want these prompts to be
  # suppressed when `usethis.quiet = TRUE`, i.e. if the menu() appears, then
  # the introduction should also always appear
  rlang::inform(x)
  out <- utils::menu(qs)
  out != 0L && qs[[out]] %in% yes
}

#' @rdname ui-questions
#' @export
ui_nope <- function(
  x,
  yes = c(
    "Yes",
    "Definitely",
    "For sure",
    "Yup",
    "Yeah",
    "I agree",
    "Absolutely"
  ),
  no = c("No way", "Not now", "Negative", "No", "Nope", "Absolutely not"),
  n_yes = 1,
  n_no = 2,
  shuffle = TRUE,
  .envir = parent.frame()
) {
  # TODO(jennybc): is this correct in the case of no selection / cancelling?
  !ui_yeah(
    x = x,
    yes = yes,
    no = no,
    n_yes = n_yes,
    n_no = n_no,
    shuffle = shuffle,
    .envir = .envir
  )
}

# Inline styles -----------------------------------------------------------

#' @rdname ui-legacy-functions
#' @export
ui_field <- function(x) {
  x <- crayon::green(x)
  x <- glue_collapse(x, sep = ", ")
  x
}

#' @rdname ui-legacy-functions
#' @export
ui_value <- function(x) {
  if (is.character(x)) {
    x <- encodeString(x, quote = "'")
  }
  x <- crayon::blue(x)
  x <- glue_collapse(x, sep = ", ")
  x
}

#' @rdname ui-legacy-functions
#' @export
#' @param base If specified, paths will be displayed relative to this path.
ui_path <- function(x, base = NULL) {
  ui_value(ui_path_impl(x, base = base))
}

#' @rdname ui-legacy-functions
#' @export
ui_code <- function(x) {
  x <- encodeString(x, quote = "`")
  x <- crayon::silver(x)
  x <- glue_collapse(x, sep = ", ")
  x
}

#' @rdname ui-legacy-functions
#' @export
ui_unset <- function(x = "unset") {
  check_string(x)
  x <- glue("<{x}>")
  x <- crayon::silver(x)
  x
}

# rlang::inform() wrappers -----------------------------------------------------

indent <- function(x, first = "  ", indent = first) {
  x <- gsub("\n", paste0("\n", indent), x)
  paste0(first, x)
}

ui_legacy_bullet <- function(x, bullet = cli::symbol$bullet) {
  bullet <- paste0(bullet, " ")
  x <- indent(x, bullet, "  ")
  ui_inform(x)
}

# All UI output must eventually go through ui_inform() so that it
# can be quieted with 'usethis.quiet' when needed.
ui_inform <- function(...) {
  if (!is_quiet()) {
    inform(paste0(...))
  }
  invisible()
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/upkeep.R ---
#' Create an upkeep checklist in a GitHub issue
#'
#' @description
#' This opens an issue in your package repository with a checklist of tasks for
#' regular maintenance of your package. This is a fairly opinionated list of
#' tasks but we believe taking care of them will generally make your package
#' better, easier to maintain, and more enjoyable for your users. Some of the
#' tasks are meant to be performed only once (and once completed shouldn't show
#' up in subsequent lists), and some should be reviewed periodically. The
#' tidyverse team uses a similar function [use_tidy_upkeep_issue()] for our
#' annual package Spring Cleaning.
#'
#' @param year Year you are performing the upkeep, used in the issue title.
#'   Defaults to current year
#'
#' @export
#' @examples
#' \dontrun{
#' use_upkeep_issue()
#' }
use_upkeep_issue <- function(year = NULL) {
  make_upkeep_issue(year = year, tidy = FALSE)
}

make_upkeep_issue <- function(year, last_upkeep, tidy) {
  who <- if (tidy) "use_tidy_upkeep_issue()" else "use_upkeep_issue()"
  check_is_package(who)

  tr <- target_repo(github_get = TRUE)

  if (!isTRUE(tr$can_push)) {
    ui_bullets(c(
      "!" = "It is very unusual to open an upkeep issue on a repo you can't push
             to ({.val {tr$repo_spec}})."
    ))
    if (ui_nah("Do you really want to do this?")) {
      ui_bullets(c("x" = "Cancelling."))
      return(invisible())
    }
  }

  gh <- gh_tr(tr)
  if (tidy) {
    checklist <- tidy_upkeep_checklist(last_upkeep, repo_spec = tr$repo_spec)
  } else {
    checklist <- upkeep_checklist(tr)
  }

  title_year <- year %||% format(Sys.Date(), "%Y")

  issue <- gh(
    "POST /repos/{owner}/{repo}/issues",
    title = glue("Upkeep for {project_name()} ({title_year})"),
    body = paste0(checklist, "\n", collapse = ""),
    labels = if (tidy) list("upkeep")
  )
  Sys.sleep(1)
  view_url(issue$html_url)
}

upkeep_checklist <- function(target_repo = NULL) {
  has_github_links <- has_github_links(target_repo)

  bullets <- c(
    todo("`usethis::use_readme_rmd()`", !file_exists(proj_path("README.Rmd"))),
    todo("`usethis::use_roxygen_md()`", !is_true(uses_roxygen_md())),
    todo("`usethis::use_github_links()`", !has_github_links),
    todo("`usethis::use_pkgdown_github_pages()`", !uses_pkgdown()),
    todo(
      "
      Consider using Bootstrap 5 in your pkgdown site. \\
      Read more in the [pkgdown customisation article](https://pkgdown.r-lib.org/articles/customise.html).",
      uses_pkgdown() && !uses_pkgdown_bootstrap_version(5)
    ),
    todo("`usethis::use_tidy_description()`"),
    todo(
      "
      `usethis::use_package_doc()`
      Consider letting usethis manage your `@importFrom` directives here. \\
      `usethis::use_import_from()` is handy for this.",
      !has_package_doc()
    ),
    todo(
      "
      `usethis::use_testthat()`. \\
      Learn more about testing at <https://r-pkgs.org/tests.html>",
      !uses_testthat()
    ),
    todo(
      "
      `usethis::use_testthat(3)` and upgrade to 3e, \\
      [testthat 3e vignette](https://testthat.r-lib.org/articles/third-edition.html)",
      uses_old_testthat_edition(current = 3)
    ),
    todo(
      "
      Align the names of `R/` files and `test/` files for workflow happiness. \\
      The docs for `usethis::use_r()` include a helpful script. \\
      `usethis::rename_files()` may be be useful."
    ),
    todo(
      "Consider changing default branch from `master` to `main`",
      git_default_branch() == "master"
    ),
    todo("`usethis::use_code_of_conduct()`", !has_coc()),
    todo(
      "Remove description of test environments from `cran-comments.md`.
      See `usethis::use_cran_comments()`.",
      has_old_cran_comments()
    ),
    todo(
      "
      Add alt-text to pictures, plots, etc; see \\
      <https://posit.co/blog/knitr-fig-alt/> for examples"
    ),
    "",
    "Set up or update GitHub Actions. \\
      Updating workflows to the latest version will often fix troublesome actions:",
    todo("`usethis::use_github_action('check-standard')`"),
    todo("`usethis::use_github_action('pkgdown')`", uses_pkgdown()),
    todo("`usethis::use_github_action('test-coverage')`", uses_testthat())
  )

  c(bullets, upkeep_extra_bullets(), checklist_footer(tidy = FALSE))
}

# tidyverse upkeep issue -------------------------------------------------------

#' @export
#' @rdname tidyverse
#' @param last_upkeep Year of last upkeep. By default, the
#' `Config/usethis/last-upkeep` field in `DESCRIPTION` is consulted for this, if
#' it's defined. If there's no information on the last upkeep, the issue will
#' contain the full checklist.
use_tidy_upkeep_issue <- function(last_upkeep = last_upkeep_year()) {
  make_upkeep_issue(year = NULL, last_upkeep = last_upkeep, tidy = TRUE)
  record_upkeep_date(Sys.Date())
}

# for mocking
Sys.Date <- NULL

tidy_upkeep_checklist <- function(
  last_upkeep = last_upkeep_year(),
  repo_spec = "OWNER/REPO"
) {
  desc <- proj_desc()

  posit_pkg <- is_posit_pkg()
  posit_person_ok <- is_posit_person_canonical()

  bullets <- c(
    "### To begin",
    "",
    todo('`usethis::pr_init("upkeep-{format(Sys.Date(), "%Y-%m")}")`'),
    ""
  )

  if (last_upkeep <= 2000) {
    bullets <- c(
      bullets,
      "### Pre-history",
      "",
      todo("`usethis::use_readme_rmd()`"),
      todo("`usethis::use_roxygen_md()`"),
      todo("`usethis::use_github_links()`"),
      todo("`usethis::use_pkgdown_github_pages()`"),
      todo("`usethis::use_tidy_github_labels()`"),
      todo("`urlchecker::url_check()`"),
      ""
    )
  }
  if (last_upkeep <= 2020) {
    bullets <- c(
      bullets,
      "### 2020",
      "",
      todo("`usethis::use_package_doc()`"),
      todo("`usethis::use_testthat(3)`"),
      todo("Align the names of `R/` files and `test/` files"),
      ""
    )
  }
  if (last_upkeep <= 2021) {
    bullets <- c(
      bullets,
      "### 2021",
      "",
      todo("Remove check environments section from `cran-comments.md`"),
      todo("Use lifecycle instead of artisanal deprecation messages"),
      ""
    )
  }
  if (last_upkeep <= 2022) {
    bullets <- c(
      bullets,
      "### 2022",
      "",
      todo("Handle and close any still-open `master` --> `main` issues"),
      todo('`usethis:::use_codecov_badge("{repo_spec}")`'),
      todo(
        "Update pkgdown site using instructions at <https://tidytemplate.tidyverse.org>"
      ),
      todo(
        "Update lifecycle badges with more accessible SVGs: `usethis::use_lifecycle()`"
      ),
      ""
    )
  }

  if (last_upkeep <= 2023) {
    bullets <- c(
      bullets,
      "### 2023",
      "",
      todo(
        "
        Update email addresses *@rstudio.com -> *@posit.co",
        author_has_rstudio_email()
      ),
      todo(
        '
        Update copyright holder in DESCRIPTION: \\
        `person("Posit Software, PBC", role = c("cph", "fnd"))`',
        posit_pkg && !posit_person_ok
      ),
      todo(
        "
        Run `devtools::document()` to re-generate package-level help topic \\
        with DESCRIPTION changes",
        author_has_rstudio_email() || (posit_pkg && !posit_person_ok)
      ),
      todo(
        "`usethis::use_tidy_logo(); pkgdown::build_favicons(overwrite = TRUE)`"
      ),
      todo("`usethis::use_tidy_coc()`"),
      todo(
        "Modernize citation files; see updated `use_citation()`",
        has_citation_file()
      ),
      todo('Use `pak::pak("{repo_spec}")` in README'),
      todo(
        "
        Consider running `usethis::use_tidy_dependencies()` and/or \\
        replace compat files with `use_standalone()`"
      ),
      todo(
        "Use cli errors or [file an issue](new) if you don\'t have time to do it now"
      ),
      todo(
        '
        `usethis::use_standalone("r-lib/rlang", "types-check")` \\
        instead of home grown argument checkers;
        or [file an issue](new) if you don\'t have time to do it now'
      ),
      todo(
        "
        Change files ending in `.r` to `.R` in `R/` and/or `tests/testthat/`",
        lowercase_r()
      ),
      todo(
        "
        Add alt-text to pictures, plots, etc; see \\
        https://posit.co/blog/knitr-fig-alt/ for examples"
      ),
      ""
    )
  }

  if (last_upkeep <= 2025) {
    bullets <- c(
      bullets,
      "### 2025",
      "",
      todo("`usethis::use_air()` <https://posit-dev.github.io/air/>"),
      todo('`usethis::use_package("R", "Depends", "4.1")`'),
      todo("Switch to the base pipe (`|>`)"),
      todo("Switch to the base anonymous function syntax (`\\(x)`) "),
      todo(
        '
        Add ROR for Posit in `DESCRIPTION`:
        `person("Posit Software, PBC", role = c("cph", "fnd"), comment = c(ROR = "03wc8by49"))`',
        posit_pkg && !posit_person_ok
      ),
      todo(
        '
        Convert in-header chunk options to the newer in-body style used by Quarto:
        `fs::dir_ls("vignettes", regexp = "[.][Rq]md$") |> purrr::walk(\\(x) knitr::convert_chunk_header(x, output = identity, type = "yaml"))`
        '
      ),
      todo(
        "Switch to `expect_snapshot(error = TRUE)` instead of calling `expect_error()` without specifying `class =`"
      ),
      ""
    )
  }

  minimum_r_version <- pkg_minimum_r_version()
  bullets <- c(
    bullets,
    "### To finish",
    "",
    # TODO: if the most recent year doesn't nudge about the minimum R version,
    # re-introduce that todo()
    #
    # todo(
    #   '`usethis::use_package("R", "Depends", "{tidy_minimum_r_version()}")`',
    #   is.na(minimum_r_version) || tidy_minimum_r_version() > minimum_r_version
    # ),
    todo(
      "`usethis::use_mit_license()`",
      grepl("MIT", desc$get_field("License"))
    ),
    todo("`usethis::use_tidy_description()`"),
    todo("`usethis::use_tidy_github_actions()`"),
    todo("`devtools::build_readme()`"),
    todo(
      "
      Add alt-text to pictures, plots, etc; see \\
      https://posit.co/blog/knitr-fig-alt/ for examples"
    ),
    todo(
      "[Re-publish released site](https://pkgdown.r-lib.org/dev/articles/how-to-update-released-site.html) if needed"
    ),
    ""
  )

  c(bullets, checklist_footer(tidy = TRUE))
}

# upkeep helpers ----------------------------------------------------------

# https://www.tidyverse.org/blog/2019/04/r-version-support/
tidy_minimum_r_version <- function() {
  con <- curl::curl("https://api.r-hub.io/rversions/r-oldrel/4")
  withr::defer(close(con))
  # I do not want a failure here to make use_tidy_upkeep_issue() fail
  json <- tryCatch(readLines(con, warn = FALSE), error = function(e) NULL)
  if (is.null(json)) {
    oldrel_4 <- "3.6"
  } else {
    version <- jsonlite::fromJSON(json)$version
    oldrel_4 <- re_match(version, "[0-9]+[.][0-9]+")$.match
  }
  numeric_version(oldrel_4)
}

lowercase_r <- function() {
  path <- proj_path(c("R", "tests"))
  path <- path[fs::dir_exists(path)]
  any(fs::path_ext(fs::dir_ls(path, recurse = TRUE)) == "r")
}

has_coc <- function() {
  path <- proj_path(c(".", ".github"), "CODE_OF_CONDUCT.md")
  any(file_exists(path))
}

has_citation_file <- function() {
  file_exists(proj_path("inst/CITATION"))
}

uses_old_testthat_edition <- function(current) {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    return(FALSE)
  }
  uses_testthat() && testthat::edition_get() < current
}

upkeep_extra_bullets <- function(env = NULL) {
  env <- env %||% safe_pkg_env()

  if (env_has(env, "upkeep_bullets")) {
    c(paste0("* [ ] ", env$upkeep_bullets()), "")
  } else {
    ""
  }
}

checklist_footer <- function(tidy) {
  tidy_fun <- if (tidy) "tidy_" else ""
  glue(
    '<sup>\\
    Created on {Sys.Date()} with `usethis::use_{tidy_fun}upkeep_issue()`, using \\
    [usethis v{usethis_version()}](https://usethis.r-lib.org)\\
    </sup>'
  )
}

usethis_version <- function() {
  utils::packageVersion("usethis")
}

has_old_cran_comments <- function() {
  cc <- proj_path("cran-comments.md")
  file_exists(cc) &&
    any(grepl("# test environment", readLines(cc), ignore.case = TRUE))
}

last_upkeep_date <- function() {
  as.Date(
    proj_desc()$get_field("Config/usethis/last-upkeep", "2000-01-01"),
    format = "%Y-%m-%d"
  )
}

last_upkeep_year <- function() {
  as.integer(format(last_upkeep_date(), "%Y"))
}

record_upkeep_date <- function(date) {
  proj_desc_field_update("Config/usethis/last-upkeep", format(date, "%Y-%m-%d"))
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/use_github_file.R ---
#' Copy a file from any GitHub repo into the current project
#'
#' Gets the content of a file from GitHub, from any repo the user can read, and
#' writes it into the active project. This function wraps an endpoint of the
#' GitHub API which supports specifying a target reference (i.e. branch, tag,
#' or commit) and which follows symlinks.
#'

#' @param repo_spec A string identifying the GitHub repo or, alternatively, a
#'   GitHub file URL. Acceptable forms:
#'   * Plain `OWNER/REPO` spec
#'   * A blob URL, such as `"https://github.com/OWNER/REPO/blob/REF/path/to/some/file"`
#'   * A raw URL, such as `"https://raw.githubusercontent.com/OWNER/REPO/REF/path/to/some/file"`
#'
#' In the case of a URL, the `path`, `ref`, and `host` are extracted from it, in
#' addition to the `repo_spec`.
#' @param path Path of file to copy, relative to the GitHub repo it lives in.
#'   This is extracted from `repo_spec` when user provides a URL.
#' @param save_as Path of file to create, relative to root of active project.
#'   Defaults to the last part of `path`, in the sense of `basename(path)` or
#'   `fs::path_file(path)`.
#' @param ref The name of a branch, tag, or commit. By default, the file at
#'   `path` will be copied from its current state in the repo's default branch.
#'   This is extracted from `repo_spec` when user provides a URL.
#' @inheritParams use_template
#' @inheritParams use_github
#' @inheritParams write_over
#'
#' @return A logical indicator of whether a file was written, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' use_github_file(
#'   "https://github.com/r-lib/actions/blob/v2/examples/check-standard.yaml"
#' )
#'
#' use_github_file(
#'   "r-lib/actions",
#'   path = "examples/check-standard.yaml",
#'   ref = "v2",
#'   save_as = ".github/workflows/R-CMD-check.yaml"
#' )
#' }
use_github_file <- function(
  repo_spec,
  path = NULL,
  save_as = NULL,
  ref = NULL,
  ignore = FALSE,
  open = FALSE,
  overwrite = FALSE,
  host = NULL
) {
  check_name(repo_spec)
  maybe_name(path)
  maybe_name(save_as)
  maybe_name(ref)
  check_bool(ignore)
  check_bool(open)
  check_bool(overwrite)
  maybe_name(host)

  dat <- parse_file_url(repo_spec)
  if (dat$parsed) {
    repo_spec <- dat$repo_spec
    path <- dat$path
    ref <- dat$ref
    host <- dat$host
  }

  save_as <- save_as %||% path_file(path)

  ref_string <- if (is.null(ref)) "" else glue("@{ref}")
  github_string <- glue("{repo_spec}/{path}{ref_string}")
  ui_bullets(c(
    "v" = "Saving {.val {github_string}} to {.path {pth(save_as)}}."
  ))

  lines <- read_github_file(
    repo_spec = repo_spec,
    path = path,
    ref = ref,
    host = host
  )
  new <- write_over(
    proj_path(save_as),
    lines,
    quiet = TRUE,
    overwrite = overwrite
  )

  if (ignore) {
    use_build_ignore(save_as)
  }

  if (open && new) {
    edit_file(proj_path(save_as))
  }

  invisible(new)
}

read_github_file <- function(repo_spec, path, ref = NULL, host = NULL) {
  # https://docs.github.com/en/rest/reference/repos#contents
  # https://docs.github.com/en/rest/reference/repos#if-the-content-is-a-symlink
  # If the requested {path} points to a symlink, and the symlink's target is a
  # normal file in the repository, then the API responds with the content of the
  # file....
  tf <- withr::local_tempfile()
  gh::gh(
    "/repos/{repo_spec}/contents/{path}",
    repo_spec = repo_spec,
    path = path,
    ref = ref,
    .api_url = host,
    .destfile = tf,
    .accept = "application/vnd.github.v3.raw"
  )
  read_utf8(tf)
}

# https://github.com/OWNER/REPO/blob/REF/path/to/some/file
# https://raw.githubusercontent.com/OWNER/REPO/REF/path/to/some/file
# https://github.acme.com/OWNER/REPO/blob/REF/path/to/some/file
# https://raw.github.acme.com/OWNER/REPO/REF/path/to/some/file
parse_file_url <- function(x) {
  out <- list(
    parsed = FALSE,
    repo_spec = x,
    path = NULL,
    ref = NULL,
    host = NULL
  )

  dat <- re_match(x, github_remote_regex)
  if (is.na(dat$.match)) {
    return(out)
  }

  # TODO: generalize here for GHE hosts that don't include 'github'
  if (!grepl("github", dat$host)) {
    ui_abort("URL doesn't seem to be associated with GitHub.")
  }

  if (
    !grepl("^(raw[.])?github", dat$host) ||
      !nzchar(dat$fragment) ||
      (grepl("^github", dat$host) && !grepl("^/blob/", dat$fragment))
  ) {
    ui_abort("Can't parse the URL provided via {.arg repo_spec}.")
  }
  out$parsed <- TRUE

  dat$host <- sub("^raw[.]", "", dat$host)
  dat$host <- sub("^githubusercontent", "github", dat$host)

  dat$fragment <- sub("^/(blob/)?", "", dat$fragment)
  dat_fragment <- re_match(dat$fragment, "^(?<ref>[^/]+)/(?<path>.+)$")

  out$repo_spec <- make_spec(owner = dat$repo_owner, repo = dat$repo_name)
  out$path <- dat_fragment$path
  out$ref <- dat_fragment$ref
  out$host <- glue_chr("https://{dat$host}")

  out
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/use_import_from.R ---
#' Import a function from another package
#'
#' @description
#' `use_import_from()` imports a function from another package by adding the
#' roxygen2 `@importFrom` tag to the package-level documentation (which can be
#' created with [`use_package_doc()`]). Importing a function from another
#' package allows you to refer to it without a namespace (e.g., `fun()` instead
#' of `package::fun()`).
#'
#' `use_import_from()` also re-documents the NAMESPACE, and re-load the current
#' package. This ensures that `fun` is immediately available in your development
#' session.
#'
#' @param package Package name
#' @param fun A vector of function names
#' @param load Logical. Re-load with [`pkgload::load_all()`]?
#' @return
#' Invisibly, `TRUE` if the package document has changed, `FALSE` if not.
#' @export
#' @examples
#' \dontrun{
#' use_import_from("glue", "glue")
#' }
use_import_from <- function(package, fun, load = is_interactive()) {
  if (!is_string(package)) {
    ui_abort("{.arg package} must be a single string.")
  }
  check_is_package("use_import_from()")
  check_uses_roxygen("use_import_from()")
  check_installed(package)
  check_has_package_doc("use_import_from()")
  check_functions_exist(package, fun)

  use_dependency(package, "Imports")
  changed <- roxygen_ns_append(glue("@importFrom {package} {fun}"))

  if (changed) {
    roxygen_update_ns(load)
  }

  invisible(changed)
}

check_functions_exist <- function(package, fun) {
  purrr::walk2(package, fun, check_fun_exists)
}

check_fun_exists <- function(package, fun) {
  if (exists(fun, envir = asNamespace(package))) {
    return()
  }
  name <- paste0(package, "::", fun)
  ui_abort("Can't find {.fun {name}}.")
}

check_has_package_doc <- function(whos_asking) {
  if (has_package_doc()) {
    return(invisible(TRUE))
  }

  whos_asking_fn <- sub("()", "", whos_asking, fixed = TRUE)
  msg <- c(
    "!" = "{.fun {whos_asking_fn}} requires package-level documentation.",
    " " = "Would you like to add it now?"
  )
  if (is_interactive() && ui_yep(msg)) {
    use_package_doc()
  } else {
    ui_abort(c(
      "{.fun {whos_asking_fn}} requires package-level documentation.",
      "You can add it by running {.run usethis::use_package_doc()}."
    ))
  }

  invisible(TRUE)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/use_standalone.R ---
#' Use a standalone file from another repo
#'
#' @description
#' A "standalone" file implements a minimum set of functionality in such a way
#' that it can be copied into another package. `use_standalone()` makes it easy
#' to get such a file into your own repo.
#'
#' It always overwrites an existing standalone file of the same name, making
#' it easy to update previously imported code.
#'
#' @section Supported fields:
#'

#' A standalone file has YAML frontmatter that provides additional information,
#' such as where the file originates from and when it was last updated. Here is
#' an example:
#'
#' ```
#' ---
#' repo: r-lib/rlang
#' file: standalone-types-check.R
#' last-updated: 2023-03-07
#' license: https://unlicense.org
#' dependencies: standalone-obj-type.R
#' imports: rlang (>= 1.1.0)
#' ---
#' ```
#'
#' Two of these fields are consulted by `use_standalone()`:
#'
#' - `dependencies`: A file or a list of files in the same repo that
#'   the standalone file depends on. These files are retrieved
#'   automatically by `use_standalone()`.
#'
#' - `imports`: A package or list of packages that the standalone file
#'    depends on. A minimal version may be specified in parentheses,
#'    e.g. `rlang (>= 1.0.0)`. These dependencies are passed to
#'    [use_package()] to ensure they are included in the `Imports:`
#'    field of the `DESCRIPTION` file.
#'
#' Note that lists are specified with standard YAML syntax, using
#' square brackets, for example: `imports: [rlang (>= 1.0.0), purrr]`.
#'
#' @inheritParams create_from_github
#' @inheritParams use_github_file
#' @param file Name of standalone file. The `standalone-` prefix and file
#'   extension are optional. If omitted, will allow you to choose from the
#'   standalone files offered by that repo.
#' @export
#' @examples
#' \dontrun{
#' use_standalone("r-lib/rlang", file = "types-check")
#' use_standalone("r-lib/rlang", file = "types-check", ref = "standalone-dep")
#' }
use_standalone <- function(repo_spec, file = NULL, ref = NULL, host = NULL) {
  check_is_project()
  maybe_name(file)
  maybe_name(host)
  maybe_name(ref)

  parsed_repo_spec <- parse_repo_url(repo_spec)
  if (!is.null(parsed_repo_spec$host)) {
    repo_spec <- parsed_repo_spec$repo_spec
    host <- parsed_repo_spec$host
  }

  if (is.null(file)) {
    file <- standalone_choose(repo_spec, ref = ref, host = host)
  } else {
    file <- as_standalone_file(file)
  }

  src_path <- path("R", file)
  dest_path <- path("R", as_standalone_dest_file(file))

  lines <- read_github_file(repo_spec, path = src_path, ref = ref, host = host)
  lines <- c(standalone_header(repo_spec, src_path, ref, host), lines)
  write_over(proj_path(dest_path), lines, overwrite = TRUE)

  dependencies <- standalone_dependencies(lines, path)

  for (dependency in dependencies$deps) {
    use_standalone(repo_spec, dependency, ref = ref, host = host)
  }

  imports <- dependencies$imports

  for (i in seq_len(nrow(imports))) {
    import <- imports[i, , drop = FALSE]

    if (is.na(import$ver)) {
      ver <- NULL
    } else {
      ver <- import$ver
    }
    ui_silence(
      use_package(import$pkg, min_version = ver)
    )
  }

  invisible()
}

standalone_choose <- function(
  repo_spec,
  ref = NULL,
  host = NULL,
  error_call = caller_env()
) {
  json <- gh::gh(
    "/repos/{repo_spec}/contents/{path}",
    repo_spec = repo_spec,
    ref = ref,
    .api_url = host,
    path = "R/"
  )

  names <- map_chr(json, "name")
  names <- names[grepl("^standalone-", names)]
  choices <- gsub("^standalone-|.[Rr]$", "", names)

  if (length(choices) == 0) {
    cli::cli_abort(
      "No standalone files found in {repo_spec}.",
      call = error_call
    )
  }

  if (!is_interactive()) {
    cli::cli_abort(
      c(
        "`file` is absent, but must be supplied.",
        i = "Possible options are {.or {choices}}."
      ),
      call = error_call
    )
  }

  choice <- utils::menu(
    choices = choices,
    title = "Which standalone file do you want to use (0 to exit)?"
  )
  if (choice == 0) {
    cli::cli_abort("Selection cancelled", call = error_call)
  }

  names[[choice]]
}

as_standalone_file <- function(file) {
  if (path_ext(file) == "") {
    file <- unclass(path_ext_set(file, "R"))
  }
  if (!grepl("standalone-", file)) {
    file <- paste0("standalone-", file)
  }
  file
}

as_standalone_dest_file <- function(file) {
  gsub("standalone-", "import-standalone-", file)
}

standalone_header <- function(repo_spec, path, ref = NULL, host = NULL) {
  ref_string <- ref %||% "HEAD"
  host_string <- host %||% "https://github.com"
  source_comment <-
    glue("# Source: {host_string}/{repo_spec}/blob/{ref_string}/{path}")

  path_string <- path_ext_remove(sub("^standalone-", "", path_file(path)))
  ref_string <- if (is.null(ref)) "" else glue(', ref = "{ref}"')
  host_string <- if (is.null(host) || host == "https://github.com") {
    ""
  } else {
    glue(', host = "{host}"')
  }
  code_hint <- glue(
    'usethis::use_standalone("{repo_spec}", "{path_string}"{ref_string}{host_string})'
  )
  generated_comment <- glue('# Generated by: {code_hint}')

  c(
    "# Standalone file: do not edit by hand",
    source_comment,
    generated_comment,
    paste0("# ", strrep("-", 72 - 2)),
    "#"
  )
}

standalone_dependencies <- function(lines, path, error_call = caller_env()) {
  dividers <- which(lines == "# ---")
  if (length(dividers) != 2) {
    cli::cli_abort(
      "Can't find yaml metadata in {.path {path}}.",
      call = error_call
    )
  }

  header <- lines[dividers[[1]]:dividers[[2]]]
  header <- gsub("^# ", "", header)

  temp <- withr::local_tempfile(lines = header)
  yaml <- rmarkdown::yaml_front_matter(temp)

  as_chr_field <- function(field) {
    if (!is.null(field) && !is.character(field)) {
      cli::cli_abort(
        "Invalid dependencies specification in {.path {path}}.",
        call = error_call
      )
    }

    field %||% character()
  }

  deps <- as_chr_field(yaml$dependencies)
  imports <- as_chr_field(yaml$imports)
  imports <- as_version_info(imports, error_call = error_call)

  if (any(stats::na.omit(imports$cmp) != ">=")) {
    cli::cli_abort(
      "Version specification must use {.code >=}.",
      call = error_call
    )
  }

  list(deps = deps, imports = imports)
}

as_version_info <- function(fields, error_call = caller_env()) {
  if (!length(fields)) {
    return(version_info_df())
  }

  if (any(grepl(",", fields))) {
    msg <- c(
      "Version field can't contain comma.",
      "i" = "Do you need to wrap in a list?"
    )
    cli::cli_abort(msg, call = error_call)
  }

  info <- lapply(fields, as_version_info_row, error_call = error_call)
  inject(rbind(!!!info))
}

as_version_info_row <- function(field, error_call = caller_env()) {
  version_regex <- "(.*) \\((.*)\\)$"
  has_ver <- grepl(version_regex, field)

  if (!has_ver) {
    return(version_info_df(field, NA, NA))
  }

  pkg <- sub(version_regex, "\\1", field)
  ver <- sub(version_regex, "\\2", field)

  ver <- strsplit(ver, " ")[[1]]

  if (!is_character(ver, n = 2) || anyNA(ver) || !all(nzchar(ver))) {
    cli::cli_abort(
      c(
        "Can't parse version `{field}` in `imports:` field.",
        "i" = "Example of expected version format: `rlang (>= 1.0.0)`."
      ),
      call = error_call
    )
  }

  version_info_df(pkg, ver[[1]], ver[[2]])
}

version_info_df <- function(pkg = chr(), cmp = chr(), ver = chr()) {
  df <- data.frame(
    pkg = as.character(pkg),
    cmp = as.character(cmp),
    ver = as.character(ver)
  )
  structure(df, class = c("tbl", "data.frame"))
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/usethis-deprecated.R ---
#' Deprecated tidyverse functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' * `use_tidy_style()` is deprecated because tidyverse packages are moving
#'   towards the use of [Air](https://posit-dev.github.io/air/) for formatting.
#'   See [use_air()] for how to start using Air. To continue using the styler
#'   package, see `styler::style_pkg()` and `styler::style_dir()`.
#'
#' @keywords internal
#' @name tidy-deprecated
NULL

#' @export
#' @rdname tidy-deprecated
use_tidy_style <- function(strict = TRUE) {
  lifecycle::deprecate_warn(
    when = "3.2.0",
    what = "use_tidy_style()",
    with = "use_air()",
    details = glue(
      "
      To continue using the styler package, call `styler::style_pkg()` or `styler::style_dir()` directly."
    )
  )
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/usethis-package.R ---
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import fs
#' @import rlang
#' @importFrom glue glue glue_collapse glue_data
#' @importFrom lifecycle deprecated
#' @importFrom purrr map map_chr map_lgl map_int
#' @importFrom utils available.packages
## usethis namespace: end
NULL

#' Options consulted by usethis
#'
#' @description
#' User-configurable options consulted by usethis, which provide a mechanism
#' for setting default behaviors for various functions.
#'
#' If the built-in defaults don't suit you, set one or more of these options.
#' Typically, this is done in the `.Rprofile` startup file, which you can open
#' for editing with [edit_r_profile()] - this will set the specified options for
#' all future R sessions. Your code will look something like:
#'
#' ```
#' options(
#'   usethis.description = list(
#'     "Authors@R" = utils::person(
#'       "Jane", "Doe",
#'       email = "jane@example.com",
#'       role = c("aut", "cre"),
#'       comment = c(ORCID = "YOUR-ORCID-ID")
#'     ),
#'     License = "MIT + file LICENSE"
#'   ),
#'   usethis.destdir = "/path/to/folder/", # for use_course(), create_from_github()
#'   usethis.protocol = "ssh", # Use ssh git protocol
#'   usethis.overwrite = TRUE # overwrite files in Git repos without confirmation
#' )
#' ```
#'
#' @section Options for the usethis package:
#'
#' - `usethis.description`: customize the default content of new `DESCRIPTION`
#'   files by setting this option to a named list.
#'   If you are a frequent package developer, it is worthwhile to pre-configure
#'   your preferred name, email, license, etc. See the example above and the
#'   [article on usethis setup](https://usethis.r-lib.org/articles/articles/usethis-setup.html)
#'   for more details.
#'
#' - `usethis.destdir`: Default directory in which to place new projects
#'   downloaded by [use_course()] and [create_from_github()].
#'   If this option is unset, the user's Desktop or similarly conspicuous place
#'   will be used.
#'
#' - `usethis.protocol`: specifies your preferred transport protocol for Git.
#'   Either "https" (default) or "ssh":
#'     * `usethis.protocol = "https"` implies `https://github.com/<OWNER>/<REPO>.git`
#'     * `usethis.protocol = "ssh"` implies `git@@github.com:<OWNER>/<REPO>.git`
#'
#'   You can also change this for the duration of your R session with
#'   [use_git_protocol()].
#'
#' - `usethis.overwrite`: If `TRUE`, usethis overwrites an existing file without
#'   asking for user confirmation if the file is inside a Git repo. The
#'   rationale is that the normal Git workflow makes it easy to see and
#'   selectively accept/discard any proposed changes.
#'
#' - `usethis.quiet`: Set to `TRUE` to suppress user-facing messages. Default
#'   `FALSE`.
#'
#' - `usethis.allow_nested_project`: Whether or not to allow
#'   you to create a project inside another project. This is rarely a good idea,
#'   so this option defaults to `FALSE`.
#'
#' @name usethis_options
NULL

release_bullets <- function() {
  c(
    "Check that `use_code_of_conduct()` is shipping the latest version of the Contributor Covenant (<https://www.contributor-covenant.org>)."
  )
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/utils-gh.R ---
# Functions that are in a grey area between usethis and gh

gh_tr <- function(tr) {
  force(tr)
  function(endpoint, ...) {
    gh::gh(
      endpoint,
      ...,
      owner = tr$repo_owner,
      repo = tr$repo_name,
      .api_url = tr$api_url
    )
  }
}

# Functions inlined from gh ----
get_baseurl <- function(url) {
  # https://github.uni.edu/api/v3/
  if (!any(grepl("^https?://", url))) {
    stop("Only works with HTTP(S) protocols")
  }
  prot <- sub("^(https?://).*$", "\\1", url) # https://
  rest <- sub("^https?://(.*)$", "\\1", url) #         github.uni.edu/api/v3/
  host <- sub("/.*$", "", rest) #         github.uni.edu
  paste0(prot, host) # https://github.uni.edu
}

# https://api.github.com --> https://github.com
# api.github.com --> github.com
normalize_host <- function(x) {
  sub("api[.]github[.]com", "github.com", x)
}

get_hosturl <- function(url) {
  url <- get_baseurl(url)
  normalize_host(url)
}

# (almost) the inverse of get_hosturl()
# https://github.com     --> https://api.github.com
# https://github.uni.edu --> https://github.uni.edu/api/v3
# fmt: skip
get_apiurl <- function(url) {
  host_url <- get_hosturl(url)
  prot_host <- strsplit(host_url, "://", fixed = TRUE)[[1]]
  if (is_github_dot_com(host_url)) {
    paste0(prot_host[[1]], "://api.github.com")
  } else if (is_github_enterprise(host_url)) {
    paste0(prot_host[[1]], "://api.", prot_host[[2]])
  } else {
    paste0(host_url, "/api/v3")
  }
}

is_github_dot_com <- function(url) {
  url <- get_baseurl(url)
  url <- normalize_host(url)
  grepl("^https?://github.com", url)
}

default_api_url <- function() {
  Sys.getenv("GITHUB_API_URL", unset = "https://api.github.com")
}

# handles GitHub Enterprise Cloud, but not GitHub Enterprise Server (which
# would, I think, require the ability to fully configure this)
# https://github.com/r-lib/usethis/issues/1897
is_github_enterprise <- function(url) {
  url <- get_baseurl(url)
  url <- normalize_host(url)
  grepl("^https?://.+ghe.com", url)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/utils-git.R ---
# gert -------------------------------------------------------------------------

gert_shush <- function(expr, regexp) {
  check_character(regexp)
  withCallingHandlers(
    gertMessage = function(cnd) {
      m <- map_lgl(regexp, \(x) grepl(x, cnd_message(cnd), perl = TRUE))
      if (any(m)) {
        cnd_muffle(cnd)
      }
    },
    expr
  )
}

# Repository -------------------------------------------------------------------
git_repo <- function() {
  check_uses_git()
  proj_get()
}

uses_git <- function() {
  repo <- tryCatch(
    gert::git_find(proj_get()),
    error = function(e) NULL
  )
  !is.null(repo)
}

check_uses_git <- function() {
  if (uses_git()) {
    return(invisible())
  }

  ui_abort(c(
    "Cannot detect that project is already a Git repository.",
    "Do you need to run {.run usethis::use_git()}?"
  ))
}

git_init <- function() {
  gert::git_init(proj_get())
}

# Config -----------------------------------------------------------------------

# `where = "de_facto"` means look at the values that are "in force", i.e. where
# local repo variables override global user-level variables, when both are
# defined
#
# `where = "local"` is strict, i.e. it only returns a value that is in the local
# config
git_cfg_get <- function(name, where = c("de_facto", "local", "global")) {
  where <- match.arg(where)

  if (where == "de_facto") {
    return(git_cfg_get(name, "local") %||% git_cfg_get(name, "global"))
  }

  if (where == "global" || !uses_git()) {
    dat <- gert::git_config_global()
  } else {
    dat <- gert::git_config(repo = git_repo())
  }

  if (where == "local") {
    dat <- dat[dat$level == "local", ]
  }

  out <- dat$value[tolower(dat$name) == tolower(name)]
  if (length(out) > 0) out else NULL
}

# more-specific case for user-name and -email
git_user_get <- function(where = c("de_facto", "local", "global")) {
  where <- match.arg(where)

  list(
    name = git_cfg_get("user.name", where),
    email = git_cfg_get("user.email", where)
  )
}

# translate from "usethis" terminology to "git" terminology
where_from_scope <- function(scope = c("user", "project")) {
  scope <- match.arg(scope)

  where_scope <- c(user = "global", project = "de_facto")

  where_scope[scope]
}

# ensures that core.excludesFile is configured
# if configured, leave well enough alone
# if not, check for existence of one of the Usual Suspects; if found, configure
# otherwise, configure as path_home(".gitignore")
ensure_core_excludesFile <- function() {
  path <- git_ignore_path(scope = "user")

  if (!is.null(path)) {
    return(invisible())
  }

  # .gitignore is most common, but .gitignore_global appears in prominent
  # places --> so we allow the latter, but prefer the former
  path <-
    path_first_existing(path_home(c(".gitignore", ".gitignore_global"))) %||%
    path_home(".gitignore")

  if (!is_windows()) {
    # express path relative to user's home directory, except on Windows
    path <- path("~", path_rel(path, path_home()))
  }
  ui_bullets(c(
    "v" = "Configuring {.field core.excludesFile}: {.path {pth(path)}}"
  ))
  gert::git_config_global_set("core.excludesFile", path)
  invisible()
}

# Status------------------------------------------------------------------------
git_status <- function(untracked) {
  check_bool(untracked)
  st <- gert::git_status(repo = git_repo())
  if (!untracked) {
    st <- st[st$status != "new", ]
  }
  st
}

# Commit -----------------------------------------------------------------------
git_ask_commit <- function(message, untracked, push = FALSE, paths = NULL) {
  if (!is_interactive() || !uses_git()) {
    return(invisible())
  }

  # this is defined here to encourage all commits to route through this function
  git_commit <- function(paths, message) {
    repo <- git_repo()
    ui_bullets(c("v" = "Adding files."))
    gert::git_add(paths, repo = repo)
    ui_bullets(c("v" = "Making a commit with message {.val {message}}."))
    gert::git_commit(message, repo = repo)
  }

  uncommitted <- git_status(untracked)$file
  if (is.null(paths)) {
    paths <- uncommitted
  } else {
    paths <- intersect(paths, uncommitted)
  }
  n <- length(paths)
  if (n == 0) {
    return(invisible())
  }

  paths <- sort(paths)
  ui_paths <- usethis_map_cli(paths, template = '{.path {pth("<<x>>")}}')
  file_hint <- "{cli::qty(n)}There {?is/are} {n} uncommitted file{?s}:"
  ui_bullets(c(
    "i" = file_hint,
    bulletize(ui_paths, n_show = 10)
  ))

  # Only push if no remote & a single change
  push <- push && git_can_push(max_local = 1)

  if (
    ui_yep(c(
      "!" = "Is it ok to commit {if (push) 'and push '} {cli::qty(n)} {?it/them}?"
    ))
  ) {
    git_commit(paths, message)
    if (push) {
      git_push()
    }
  } else {
    ui_bullets(c("x" = "Cancelling."))
  }

  invisible()
}

git_uncommitted <- function(untracked = FALSE) {
  nrow(git_status(untracked)) > 0
}

challenge_uncommitted_changes <- function(untracked = FALSE, msg = NULL) {
  if (!uses_git()) {
    return(invisible())
  }

  if (rstudioapi::hasFun("documentSaveAll")) {
    rstudioapi::documentSaveAll()
  }

  default_msg <- "
    There are uncommitted changes, which may cause problems or be lost when \\
    we push, pull, switch, or compare branches"
  msg <- glue(msg %||% default_msg)
  if (git_uncommitted(untracked = untracked)) {
    if (
      ui_yep(c(
        "!" = msg,
        " " = "Do you want to proceed anyway?"
      ))
    ) {
      return(invisible())
    } else {
      ui_abort("Uncommitted changes. Please commit before continuing.")
    }
  }
}

git_conflict_report <- function() {
  st <- git_status(untracked = FALSE)
  conflicted <- st$file[st$status == "conflicted"]
  n <- length(conflicted)
  if (n == 0) {
    return(invisible())
  }

  conflicted_paths <- usethis_map_cli(
    conflicted,
    template = '{.path {pth("<<x>>")}}'
  )
  file_hint <- "{cli::qty(n)}There {?is/are} {n} conflicted file{?s}:"
  ui_bullets(c(
    "i" = file_hint,
    bulletize(conflicted_paths, n_show = 10)
  ))

  yes <- "Yes, open the conflicted files for editing."
  yes_soft <- "Yes, but do not open the conflicted files."
  no <- "No, I want to abort this merge."
  choice <- utils::menu(
    title = "Do you want to proceed with this merge?",
    choices = c(yes, yes_soft, no)
  )

  if (choice < 1 || choice > 2) {
    gert::git_merge_abort(repo = git_repo())
    ui_abort("Abandoning the merge, since it will cause merge conflicts.")
  }

  if (choice == 1) {
    ui_silence(purrr::walk(conflicted, edit_file))
  }
  ui_abort(c(
    "Please fix each conflict, save, stage, and commit.",
    "To back out of this merge, run {.code gert::git_merge_abort()}
     (in R) or {.code git merge --abort} (in the shell)."
  ))
}

# Remotes ----------------------------------------------------------------------
## remref --> remote, branch
git_parse_remref <- function(remref) {
  regex <- paste0("^", names(git_remotes()), collapse = "|")
  regex <- glue("({regex})/(.*)")
  list(remote = sub(regex, "\\1", remref), branch = sub(regex, "\\2", remref))
}

remref_remote <- function(remref) git_parse_remref(remref)$remote
remref_branch <- function(remref) git_parse_remref(remref)$branch

# Pull -------------------------------------------------------------------------
# Pull from remref or upstream tracking. If neither given/exists, do nothing.
# Therefore, this does less than `git pull`.
git_pull <- function(remref = NULL, verbose = TRUE) {
  check_string(remref, allow_na = TRUE, allow_null = TRUE)
  repo <- git_repo()
  branch <- git_branch()
  remref <- remref %||% git_branch_tracking(branch)
  if (is.na(remref)) {
    if (verbose) {
      ui_bullets(c("v" = "No remote branch to pull from for {.val {branch}}."))
    }
    return(invisible())
  }
  if (verbose) {
    ui_bullets(c("v" = "Pulling from {.val {remref}}."))
  }
  gert::git_fetch(
    remote = remref_remote(remref),
    refspec = remref_branch(remref),
    repo = repo,
    verbose = FALSE
  )
  # this is pretty brittle, because I've hard-wired these messages
  # https://github.com/r-lib/gert/blob/main/R/merge.R
  # but at time of writing, git_merge() offers no verbosity control
  gert_shush(
    regexp = c(
      "Already up to date, nothing to merge",
      "Performing fast-forward merge, no commit needed"
    ),
    gert::git_merge(remref, repo = repo)
  )
  st <- git_status(untracked = TRUE)
  if (any(st$status == "conflicted")) {
    git_conflict_report()
  }

  invisible()
}

# Branch ------------------------------------------------------------------
git_branch <- function() {
  info <- gert::git_info(repo = git_repo())
  branch <- info$shorthand
  if (identical(branch, "HEAD")) {
    ui_abort("Detached head; can't continue.")
  }
  if (is.na(branch)) {
    ui_abort("On an unborn branch -- do you need to make an initial commit?")
  }
  branch
}

git_branch_tracking <- function(branch = git_branch()) {
  repo <- git_repo()
  if (!gert::git_branch_exists(branch, local = TRUE, repo = repo)) {
    ui_abort("There is no local branch named {.val {branch}}.")
  }
  gbl <- gert::git_branch_list(local = TRUE, repo = repo)
  sub("^refs/remotes/", "", gbl$upstream[gbl$name == branch])
}

git_branch_compare <- function(branch = git_branch(), remref = NULL) {
  remref <- remref %||% git_branch_tracking(branch)
  gert::git_fetch(
    remote = remref_remote(remref),
    refspec = remref_branch(remref),
    repo = git_repo(),
    verbose = FALSE
  )
  out <- gert::git_ahead_behind(
    upstream = remref,
    ref = branch,
    repo = git_repo()
  )
  list(local_only = out$ahead, remote_only = out$behind)
}

git_can_push <- function(
  max_local = Inf,
  branch = git_branch(),
  remref = NULL
) {
  remref <- remref %||% git_branch_tracking(branch)
  if (is.null(remref)) {
    return(FALSE)
  }
  comp <- git_branch_compare(branch, remref)
  comp$remote_only == 0 && comp$local_only <= max_local
}

git_push <- function(branch = git_branch(), remref = NULL, verbose = TRUE) {
  remref <- remref %||% git_branch_tracking(branch)
  if (verbose) {
    ui_bullets(c(
      "v" = "Pushing local {.val {branch}} branch to {.val {remref}}."
    ))
  }

  gert::git_push(
    remote = remref_remote(remref),
    refspec = glue("refs/heads/{branch}:refs/heads/{remref_branch(remref)}"),
    verbose = FALSE,
    repo = git_repo()
  )
}

git_push_first <- function(
  branch = git_branch(),
  remote = "origin",
  verbose = TRUE
) {
  if (verbose) {
    remref <- glue("{remote}/{branch}")
    ui_bullets(c(
      "v" = "Pushing {.val {branch}} branch to GitHub and setting
             {.val {remref}} as upstream branch."
    ))
  }
  gert::git_push(
    remote = remote,
    set_upstream = TRUE,
    verbose = FALSE,
    repo = git_repo()
  )
}

# Checks ------------------------------------------------------------------

check_current_branch <- function(is = NULL, is_not = NULL, message = NULL) {
  gb <- git_branch()

  if (!is.null(is)) {
    check_string(is)
    if (gb == is) {
      return(invisible())
    } else {
      if (is.null(message)) {
        message <- c("x" = "Must be on branch {.val {is}}, not {.val {gb}}.")
      }
      ui_abort(message)
    }
  }

  if (!is.null(is_not)) {
    check_string(is_not)
    if (gb != is_not) {
      return(invisible())
    } else {
      if (is.null(message)) {
        message <- c("x" = "Can't be on branch {.val {gb}}.")
      }
      ui_abort(message)
    }
  }

  invisible()
}

# examples of remref: upstream/main, origin/foofy
check_branch_up_to_date <- function(
  direction = c("pull", "push"),
  remref = NULL,
  use = NULL
) {
  direction <- match.arg(direction)
  branch <- git_branch()
  remref <- remref %||% git_branch_tracking(branch)
  use <- use %||% switch(direction, pull = "git pull", push = "git push")

  if (is.na(remref)) {
    ui_bullets(c(
      "i" = "Local branch {.val {branch}} is not tracking a remote branch."
    ))
    return(invisible())
  }

  if (direction == "pull") {
    ui_bullets(c(
      "v" = "Checking that local branch {.val {branch}} has the changes
             in {.val {remref}}."
    ))
  } else {
    ui_bullets(c(
      "v" = "Checking that remote branch {.val {remref}} has the changes
             in {.val {branch}}."
    ))
  }

  comparison <- git_branch_compare(branch, remref)

  if (direction == "pull") {
    if (comparison$remote_only == 0) {
      return(invisible())
    } else {
      ui_abort(c(
        "Local branch {.val {branch}} is behind {.val {remref}} by
         {comparison$remote_only} commit{?s}.",
        "Please use {.code {use}} to update."
      ))
    }
  } else {
    if (comparison$local_only == 0) {
      return(invisible())
    } else {
      # TODO: consider offering to push for them?
      ui_abort(c(
        "Local branch {.val {branch}} is ahead of {.val {remref}} by
         {comparison$remote_only} commit{?s}.",
        "Please use {.code {use}} to update."
      ))
    }
  }
}

check_branch_pulled <- function(remref = NULL, use = NULL) {
  check_branch_up_to_date(direction = "pull", remref = remref, use = use)
}

check_branch_pushed <- function(remref = NULL, use = NULL) {
  check_branch_up_to_date(direction = "push", remref = remref, use = use)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/utils-github.R ---
# OWNER/REPO --> OWNER, REPO
parse_repo_spec <- function(repo_spec) {
  repo_split <- strsplit(repo_spec, "/")[[1]]
  if (length(repo_split) != 2) {
    ui_abort("{.arg repo_spec} must be of the form {.val owner/repo}.")
  }
  list(owner = repo_split[[1]], repo = repo_split[[2]])
}

spec_owner <- function(repo_spec) parse_repo_spec(repo_spec)$owner
spec_repo <- function(repo_spec) parse_repo_spec(repo_spec)$repo

# OWNER, REPO --> OWNER/REPO
make_spec <- function(owner = NA, repo = NA) {
  no_spec <- is.na(owner) | is.na(repo)
  as.character(ifelse(no_spec, NA, glue("{owner}/{repo}")))
}

# named vector or list of GitHub URLs --> data frame of URL parts
# more general than the name suggests
# definitely designed for GitHub URLs but not overtly GitHub-specific
# https://stackoverflow.com/questions/2514859/regular-expression-for-git-repository
# https://git-scm.com/docs/git-clone#_git_urls
# https://stackoverflow.com/questions/27745/getting-parts-of-a-url-regex
github_remote_regex <- paste0(
  "^",
  "(?<protocol>\\w+://)?",
  "(?<user>.+@)?",
  "(?<host>[^/:]+)",
  "[/:]",
  "(?<repo_owner>[^/]+)",
  "/",
  "(?<repo_name>[^/#]+)",
  "(?<fragment>.*)",
  "$"
)

parse_github_remotes <- function(x) {
  # https://github.com/r-lib/usethis
  #                                    --> https, github.com,      rlib, usethis
  # https://github.com/r-lib/usethis.git
  #                                    --> https, github.com,      rlib, usethis
  # https://github.com/r-lib/usethis#readme
  #                                    --> https, github.com,      rlib, usethis
  # https://github.com/r-lib/usethis/issues/1169
  #                                    --> https, github.com,      rlib, usethis
  # https://github.acme.com/r-lib/devtools.git
  #                                    --> https, github.acme.com, rlib, usethis
  # git@github.com:r-lib/usethis.git
  #                                    --> ssh,   github.com,      rlib, usethis
  # ssh://git@github.com/rstudio/packrat.git
  #                                    --> ssh,   github.com,      rlib, usethis
  dat <- re_match(x, github_remote_regex)

  dat$protocol <- sub("://$", "", dat$protocol)
  dat$user <- sub("@$", "", dat$user)
  dat$repo_name <- sub("[.]git$", "", dat$repo_name)
  dat$url <- dat$.text

  # as.character() necessary for edge case of length-0 input
  dat$protocol <- as.character(ifelse(dat$protocol == "https", "https", "ssh"))
  dat$name <- if (rlang::is_named(x)) {
    names(x)
  } else {
    rep_len(NA_character_, length.out = nrow(dat))
  }

  dat[c("name", "url", "host", "repo_owner", "repo_name", "protocol")]
}

parse_repo_url <- function(x) {
  check_name(x)
  dat <- re_match(x, github_remote_regex)
  if (is.na(dat$.match)) {
    list(repo_spec = x, host = NULL)
  } else {
    dat <- parse_github_remotes(x)
    # TODO: generalize here for GHE hosts that don't include 'github'
    if (!grepl("github", dat$host)) {
      ui_abort("URL doesn't seem to be associated with GitHub: {.val {x}}")
    }
    list(
      repo_spec = make_spec(owner = dat$repo_owner, repo = dat$repo_name),
      host = glue("https://{dat$host}")
    )
  }
}

# Can be called in contexts where we have already asked user to choose between
# origin and upstsream and, therefore, we know the remote URL. We parse it
# regardless, because:
# (1) Could be SSH not HTTPS
# (2) Could be hosted on GHE not github.com
github_url_from_git_remotes <- function(url = NULL) {
  if (is.null(url)) {
    tr <- tryCatch(target_repo(github_get = NA), error = function(e) NULL)
    if (is.null(tr)) {
      return()
    }
    url <- tr$url
  }

  parsed <- parse_github_remotes(url)
  glue_data_chr(parsed, "https://{host}/{repo_owner}/{repo_name}")
}

#' Gather LOCAL data on GitHub-associated remotes
#'
#' Creates a data frame where each row represents a GitHub-associated remote.
#' The data frame is initialized via `gert::git_remote_list()`, possibly
#' filtered for specific remote names. The remote URLs are parsed into parts,
#' like `host` and `repo_owner`. This is filtered again for rows where the
#' `host` appears to be a GitHub deployment (currently a crude search for
#' "github" or "ghe"). Some of these parts are recombined or embellished to get
#' new columns (`host_url`, `api_url`, `repo_spec`). All operations are entirely
#' mechanical and local.
#'
#' @param these Intersect the list of remotes with `these` remote names. To keep
#'   all remotes, use `these = NULL` or `these = character()`.
#' @param x Data frame with character columns `name` and `url`. Exposed as an
#'   argument for internal reasons. It's so we can call the functions that
#'   marshal info about GitHub remotes with 0-row input to obtain a properly
#'   typed template without needing a Git repo or calling GitHub. We just want
#'   to get a data frame with zero rows, but with the column names and types
#'   implicit in our logic.
#' @keywords internal
#' @noRd
github_remote_list <- function(these = c("origin", "upstream"), x = NULL) {
  x <- x %||% gert::git_remote_list(repo = git_repo())
  check_character(these, allow_null = TRUE)
  check_data_frame(x)
  check_character(x$name)
  check_character(x$url)
  if (length(these) > 0) {
    x <- x[x$name %in% these, ]
  }

  parsed <- parse_github_remotes(set_names(x$url, x$name))
  # TODO: presumably more generalization is necessary to truly handle self-hosted GHE
  is_github <- grepl("github|ghe", parsed$host)
  parsed <- parsed[is_github, ]

  parsed$remote <- parsed$name
  parsed$host_url <- glue_chr("https://{parsed$host}")
  parsed$api_url <- map_chr(parsed$host_url, get_apiurl)
  parsed$repo_spec <- make_spec(parsed$repo_owner, parsed$repo_name)

  parsed[c(
    "remote",
    "url",
    "host_url",
    "api_url",
    "host",
    "protocol",
    "repo_owner",
    "repo_name",
    "repo_spec"
  )]
}

#' Gather LOCAL and (maybe) REMOTE data on GitHub-associated remotes
#'
#' Creates a data frame where each row represents a GitHub-associated remote,
#' starting with the output of `github_remote_list()` (local data). This
#' function's job is to (maybe) add information we can only get from the GitHub
#' API. If `github_get = FALSE`, we don't even attempt to call the API.
#' Otherwise, we try and will succeed if gh discovers a suitable token. The
#' resulting data, even if the API data is absent, is massaged into a data
#' frame.
#'
#' @inheritParams github_remote_list
#' @param github_get Whether to attempt to get repo info from the GitHub API. We
#'   try for `NA` (the default) and `TRUE`. If we aren't successful, we proceed
#'   anyway for `NA` but error for `TRUE`. When `FALSE`, no attempt is made to
#'   call the API.
#' @keywords internal
#' @noRd
github_remotes <- function(
  these = c("origin", "upstream"),
  github_get = NA,
  x = NULL
) {
  grl <- github_remote_list(these = these, x = x)
  get_gh_repo <- function(
    repo_owner,
    repo_name,
    api_url = "https://api.github.com"
  ) {
    if (isFALSE(github_get)) {
      f <- function(...) list()
    } else {
      f <- purrr::possibly(gh::gh, otherwise = list())
    }
    f(
      "GET /repos/{owner}/{repo}",
      owner = repo_owner,
      repo = repo_name,
      .api_url = api_url
    )
  }
  repo_info <- purrr::pmap(
    grl[c("repo_owner", "repo_name", "api_url")],
    get_gh_repo
  )
  # NOTE: these can be two separate matters:
  # 1. Did we call the GitHub API? Means we know `is_fork` and the parent repo.
  # 2. If so, did we call it with auth? Means we know if we can push.
  grl$github_got <- map_lgl(repo_info, \(x) length(x) > 0)
  if (isTRUE(github_get) && !all(grl$github_got)) {
    oops <- which(!grl$github_got)
    oops_remotes <- grl$remote[oops]
    oops_hosts <- unique(grl$host[oops])
    ui_abort(c(
      "Unable to get GitHub info for these remotes: {.val {oops_remotes}}.",
      "Are we offline? Is GitHub down? Has the repo been deleted?",
      "Otherwise, you probably need to configure a personal access token (PAT)
       for {.val {oops_hosts}}.",
      "See {.run usethis::gh_token_help()} for advice."
    ))
  }

  grl$default_branch <- map_chr(repo_info, "default_branch", .default = NA)
  grl$is_fork <- map_lgl(repo_info, "fork", .default = NA)
  # `permissions` is an example of data that is not present if the request
  # did not include a PAT
  grl$can_push <- map_lgl(repo_info, c("permissions", "push"), .default = NA)
  grl$can_admin <- map_lgl(repo_info, c("permissions", "admin"), .default = NA)
  grl$perm_known <- !is.na(grl$can_push)
  grl$parent_repo_owner <-
    map_chr(repo_info, c("parent", "owner", "login"), .default = NA)
  grl$parent_repo_name <-
    map_chr(repo_info, c("parent", "name"), .default = NA)
  grl$parent_repo_spec <- make_spec(grl$parent_repo_owner, grl$parent_repo_name)

  parent_info <- purrr::pmap(
    set_names(
      grl[c("parent_repo_owner", "parent_repo_name", "api_url")],
      \(x) sub("parent_", "", x)
    ),
    get_gh_repo
  )
  grl$can_push_to_parent <-
    map_lgl(parent_info, c("permissions", "push"), .default = NA)

  grl
}

#' Classify the GitHub remote configuration
#'
#' @description
#' Classify the active project's GitHub remote situation, so diagnostic and
#' other downstream functions can decide whether to proceed / abort / complain &
#' offer to fix.
#' We only consider the remotes where:
#' * Name is `origin` or `upstream` and the remote URL "looks like github"
#'   (github.com or a GHE deployment)
#'
#' We have to call the GitHub API to fully characterize the GitHub remote
#' situation. That's the only way to learn if the user can push to a remote,
#' whether a remote is a fork, and which repo is the parent of a fork.
#' `github_get` controls whether we make these API calls.
#'
#' Some functions can get by with the information that's available locally, i.e.
#' we can use simple logic to decide whether to target `origin` or `upstream` or
#' present the user with a choice. We can set `github_get = FALSE` in this case.
#' Other functions, like the `pr_*()` functions, are more demanding and we'll
#' always determine the config with `github_get = TRUE`.
#'
#' Most usethis functions should call the higher-level functions `target_repo()`
#' or `target_repo_spec()`.
#'
#' Only functions that really need full access to the GitHub remote config
#' should call this directly. Ways to work with a config:
#' * `cfg <- github_remote_config(github_get = )`
#' * `check_for_bad_config(cfg)` errors for obviously bad configs (by default)
#'   or you can specify the configs considered to be bad
#' * Emit a custom message then call `stop_bad_github_remote_config()` directly
#' * If the config is suboptimal-but-supported, use
#'   `ui_github_remote_config_wat()` to educate the user and give them a chance
#'   to back out.
#'
#' Fields in an instance of `github_remote_config`:
#' * `type`: explained below
#' * `pr_ready`: Logical. Do the `pr_*()` functions support it?
#' * `desc`: A description used in messages and menus.
#' * `origin`: Information about the `origin` GitHub remote.
#' * `upstream`: Information about the `upstream` GitHub remote.
#'
#' Possible GitHub remote configurations, the common cases:
#' * no_github: No `origin`, no `upstream`.
#' * ours: `origin` exists, is not a fork, and we can push to it. Owner of
#'   `origin` could be current user, another user, or an org. No `upstream`.
#'   - Less common variant: `upstream` exists, `origin` does not, and we can
#'     push to `upstream`. The fork-ness of `upstream` is not consulted.
#' * fork: `origin` exists and we can push to it. `origin` is a fork of the repo
#'   configured as `upstream`. We may or may not be able to push to `upstream`.
#' * theirs: Exactly one of `origin` and `upstream` exist and we can't push to
#'   it. The fork-ness of this remote repo is not consulted.
#'
#' Possible GitHub remote configurations, the peculiar ones:
#' * fork_upstream_is_not_origin_parent: `origin` exists, it's a fork, but its
#'   parent repo is not configured as `upstream`. Either there's no `upstream`
#'   or `upstream` exists but it's not the parent of `origin`.
#' * fork_cannot_push_origin: `origin` is a fork and its parent is configured
#'   as `upstream`. But we can't push to `origin`.
#' * upstream_but_origin_is_not_fork: `origin` and `upstream` both exist, but
#'   `origin` is not a fork of anything and, specifically, it's not a fork of
#'   `upstream`.
#'
#'  Remote configuration "guesses" we apply when `github_get = FALSE` or when
#'  we make unauthorized requests (no PAT found) and therefore have no info on
#'  permissions
#'  * maybe_ours_or_theirs: Exactly one of `origin` and `upstream` exists.
#'  * maybe_fork: Both `origin` and `upstream` exist.
#'
#' @inheritParams github_remotes
#' @keywords internal
#' @noRd
new_github_remote_config <- function() {
  ptype <- github_remotes(
    x = data.frame(
      name = character(),
      url = character(),
      stringsAsFactors = FALSE
    )
  )
  # 0-row df --> a well-named list of properly typed NAs
  ptype <- map(ptype, \(x) c(NA, x))
  structure(
    list(
      type = NA_character_,
      host_url = NA_character_,
      pr_ready = FALSE,
      desc = "Unexpected remote configuration.",
      origin = c(name = "origin", is_configured = FALSE, ptype),
      upstream = c(name = "upstream", is_configured = FALSE, ptype)
    ),
    class = "github_remote_config"
  )
}

github_remote_config <- function(github_get = NA) {
  cfg <- new_github_remote_config()
  grl <- github_remotes(github_get = github_get)

  if (nrow(grl) == 0) {
    return(cfg_no_github(cfg))
  }

  cfg$origin$is_configured <- "origin" %in% grl$remote
  cfg$upstream$is_configured <- "upstream" %in% grl$remote

  single_remote <- xor(cfg$origin$is_configured, cfg$upstream$is_configured)

  if (!single_remote) {
    if (length(unique(grl$host)) != 1) {
      ui_abort(c(
        "Internal error: Multiple GitHub hosts.",
        "{.val {grl$host}}"
      ))
    }
    if (length(unique(grl$github_got)) != 1) {
      ui_abort(c(
        "Internal error: Got GitHub API info for some remotes, but not all.",
        "Do all the remotes still exist? Do you still have access?"
      ))
    }
    if (length(unique(grl$perm_known)) != 1) {
      ui_abort(
        "
        Internal error: Know GitHub permissions for some remotes, but not all."
      )
    }
  }
  cfg$host_url <- unique(grl$host_url)
  github_got <- any(grl$github_got)
  perm_known <- any(grl$perm_known)

  if (cfg$origin$is_configured) {
    cfg$origin <-
      utils::modifyList(cfg$origin, grl[grl$remote == "origin", ])
  }

  if (cfg$upstream$is_configured) {
    cfg$upstream <-
      utils::modifyList(cfg$upstream, grl[grl$remote == "upstream", ])
  }

  if (github_got && !single_remote) {
    cfg$origin$parent_is_upstream <-
      identical(cfg$origin$parent_repo_spec, cfg$upstream$repo_spec)
  }

  if (!github_got || !perm_known) {
    if (single_remote) {
      return(cfg_maybe_ours_or_theirs(cfg))
    } else {
      return(cfg_maybe_fork(cfg))
    }
  }
  # `github_got` must be TRUE
  # `perm_known` must be TRUE

  # origin only
  if (single_remote && cfg$origin$is_configured) {
    if (cfg$origin$is_fork) {
      if (cfg$origin$can_push) {
        return(cfg_fork_upstream_is_not_origin_parent(cfg))
      } else {
        return(cfg_theirs(cfg))
      }
    } else {
      if (cfg$origin$can_push) {
        return(cfg_ours(cfg))
      } else {
        return(cfg_theirs(cfg))
      }
    }
  }

  # upstream only
  if (single_remote && cfg$upstream$is_configured) {
    if (cfg$upstream$can_push) {
      return(cfg_ours(cfg))
    } else {
      return(cfg_theirs(cfg))
    }
  }

  # origin and upstream
  if (cfg$origin$is_fork) {
    if (cfg$origin$parent_is_upstream) {
      if (cfg$origin$can_push) {
        return(cfg_fork(cfg))
      } else {
        return(cfg_fork_cannot_push_origin(cfg))
      }
    } else {
      return(cfg_fork_upstream_is_not_origin_parent(cfg))
    }
  } else {
    return(cfg_upstream_but_origin_is_not_fork(cfg))
  }
}

#' Select a target (GitHub) repo
#'
#' @description

#' Returns information about ONE GitHub repository. Used when we need to
#' designate which repo we will, e.g., open an issue on or activate a CI service
#' for. This information might be used in a GitHub API request or to form URLs.
#'

#' Examples:
#' * Badge URLs
#' * URLs where you can activate a CI service
#' * URLs for DESCRIPTION fields such as URL and BugReports

#' `target_repo()` passes `github_get` along to `github_remote_config()`. If
#' `github_get = TRUE`, `target_repo()` will error for configs other than
#' `"ours"` or `"fork"`. `target_repo()` always errors for bad configs. If
#' `github_get = NA` or `FALSE`, the "maybe" configs are tolerated.
#'
#' `target_repo_spec()` is a less capable function for when you just need an
#' `OWNER/REPO` spec. Currently, it does not set or offer control over
#' `github_get`, although I've considered explicitly setting `github_get =
#' FALSE` or adding this argument, defaulting to `FALSE`.
#'

#' @inheritParams github_remotes

#' @param cfg An optional GitHub remote configuration. Used to get the target
#'   repo when the function had some need for the full config.
#' @param role We use "source" to mean the principal repo where a project's
#'   development happens. We use "primary" to mean the principal repo this
#'   particular user interacts with or has the greatest power over. They can be
#'   the same or different. Examples:
#' * For a personal project you own, "source" and "primary" are the same.
#'   Presumably the `origin` remote.
#' * For a collaboratively developed project, an outside contributor must create
#'   a fork in order to make a PR. For such a person, their fork is "primary"
#'   (presumably `origin`) and the original repo that they forked is "source"
#'   (presumably `upstream`).
#' This is *almost* consistent with terminology used by the GitHub API. A fork
#' has a "source repo" and a "parent repo", which are usually the same. They
#' only differ when working with a fork of a repo that is itself a fork. In this
#' rare case, the parent is the immediate fork parent and the source is the
#' ur-parent, i.e. the root of this particular tree. The source repo is not a
#' fork.
#' @param ask In some configurations, if `ask = TRUE` and we're in an
#'   interactive session, user gets a choice between `origin` and `upstream`.
#' @keywords internal
#' @noRd
target_repo <- function(
  cfg = NULL,
  github_get = NA,
  role = c("source", "primary"),
  ask = is_interactive(),
  ok_configs = c("ours", "fork", "theirs")
) {
  cfg <- cfg %||% github_remote_config(github_get = github_get)
  stopifnot(inherits(cfg, "github_remote_config"))
  role <- match.arg(role)

  check_for_bad_config(cfg)

  if (isTRUE(github_get)) {
    check_for_config(cfg, ok_configs = ok_configs)
  }

  # upstream only
  if (cfg$upstream$is_configured && !cfg$origin$is_configured) {
    return(cfg$upstream)
  }

  # origin only
  if (cfg$origin$is_configured && !cfg$upstream$is_configured) {
    return(cfg$origin)
  }

  if (!ask || !is_interactive()) {
    return(switch(
      role,
      source = cfg$upstream,
      primary = cfg$origin
    ))
  }

  choices <- c(
    origin = ui_pre_glue("<<cfg$origin$repo_spec>> = {.val origin}"),
    upstream = ui_pre_glue("<<cfg$upstream$repo_spec>> = {.val upstream}")
  )
  choices_formatted <- map_chr(choices, cli::format_inline)
  title <- "Which repo should we target?"
  choice <- utils::menu(choices_formatted, graphics = FALSE, title = title)
  cfg[[names(choices)[choice]]]
}

target_repo_spec <- function(
  role = c("source", "primary"),
  ask = is_interactive()
) {
  tr <- target_repo(role = match.arg(role), ask = ask)
  tr$repo_spec
}

# formatting github remote configurations for humans ---------------------------
pre_format_remote <- function(remote) {
  effective_spec <- function(remote) {
    if (remote$is_configured) {
      ui_pre_glue("{.val <<remote$repo_spec>>}")
    } else {
      ui_special("not configured")
    }
  }
  push_clause <- function(remote) {
    if (!remote$is_configured || is.na(remote$can_push)) {
      return()
    }
    if (remote$can_push) " (can push)" else " (can not push)"
  }
  out <- c(
    glue("{remote$name} = {effective_spec(remote)}"),
    push_clause(remote),
    if (isTRUE(remote$is_fork)) {
      ui_pre_glue(" = fork of {.val <<remote$parent_repo_spec>>}")
    }
  )
  glue_collapse(out)
}

pre_format_fields <- function(cfg) {
  list(
    type = ui_pre_glue("Type = {.val <<cfg$type>>}"),
    host_url = ui_pre_glue("Host = {.val <<cfg$host_url>>}"),
    # extra brackets here ensure value is formatted as logical (vs string)
    pr_ready = ui_pre_glue(
      "Config supports a pull request = {.val {<<cfg$pr_ready>>}}"
    ),
    origin = pre_format_remote(cfg$origin),
    upstream = pre_format_remote(cfg$upstream),
    desc = cfg$desc
  )
}

#' @export
format.github_remote_config <- function(x, ...) {
  x_fmt <- pre_format_fields(x)
  x_fmt$desc <- map_chr(x_fmt$desc, cli::format_inline)
  x_fmt <- purrr::map_if(x_fmt, function(x) length(x) == 1, cli::format_inline)
  out <- unlist(unname(x_fmt))

  nms <- names2(out)
  nms <- ifelse(nzchar(nms), nms, "*")
  names(out) <- nms

  out
}

#' @export
print.github_remote_config <- function(x, ...) {
  withr::local_options(usethis.quiet = FALSE)
  ui_bullets(format(x, ...))
  invisible(x)
}

# refines output of format_fields() to create input better suited to
# ui_github_remote_config_wat() and stop_bad_github_remote_config()
github_remote_config_wat <- function(cfg, context = c("menu", "abort")) {
  context <- match.arg(context)
  adjective <- switch(context, menu = "Unexpected", abort = "Unsupported")
  out <- format(cfg)

  type_idx <- grep("^Type", out)
  out[type_idx] <- ui_pre_glue(
    "
    <<adjective>> GitHub remote configuration: {.val <<cfg$type>>}"
  )
  names(out)[type_idx] <- "x"

  pr_idx <- grep("pull request", out)
  out <- out[-pr_idx]

  unlist(out)
}

# returns TRUE if user selects "no" --> exit the calling function
# return FALSE if user select "yes" --> keep going, they've been warned
ui_github_remote_config_wat <- function(cfg) {
  ui_nah(
    github_remote_config_wat(cfg, context = "menu"),
    yes = "Yes, I want to proceed. I know what I'm doing.",
    no = "No, I want to stop and straighten out my GitHub remotes first.",
    shuffle = FALSE
  )
}

stop_bad_github_remote_config <- function(cfg) {
  ui_abort(
    github_remote_config_wat(cfg, context = "abort"),
    class = "usethis_error_bad_github_remote_config",
    cfg = cfg
  )
}

stop_maybe_github_remote_config <- function(cfg) {
  msg <- c(
    ui_pre_glue(
      "
      Pull request functions can't work with GitHub remote configuration:
      {.val <<cfg$type>>}."
    ),
    "The most likely problem is that we aren't discovering your GitHub personal
     access token.",
    github_remote_config_wat(cfg)
  )
  idx <- grep("Unexpected GitHub remote configuration", msg)
  msg <- msg[-idx]

  ui_abort(
    message = unlist(msg),
    class = "usethis_error_invalid_pr_config",
    cfg = cfg
  )
}

check_for_bad_config <- function(
  cfg,
  bad_configs = c(
    "no_github",
    "fork_upstream_is_not_origin_parent",
    "fork_cannot_push_origin",
    "upstream_but_origin_is_not_fork"
  )
) {
  if (cfg$type %in% bad_configs) {
    stop_bad_github_remote_config(cfg)
  }
  invisible()
}

check_for_maybe_config <- function(cfg) {
  maybe_configs <- grep("^maybe_", all_configs(), value = TRUE)
  if (cfg$type %in% maybe_configs) {
    stop_maybe_github_remote_config(cfg)
  }
  invisible()
}

check_for_config <- function(
  cfg = NULL,
  ok_configs = c("ours", "fork", "theirs")
) {
  cfg <- cfg %||% github_remote_config(github_get = TRUE)
  stopifnot(inherits(cfg, "github_remote_config"))

  if (cfg$type %in% ok_configs) {
    return(invisible(cfg))
  }

  check_for_maybe_config(cfg)

  bad_configs <- grep("^maybe_", all_configs(), invert = TRUE, value = TRUE)
  bad_configs <- setdiff(bad_configs, ok_configs)

  check_for_bad_config(cfg, bad_configs = bad_configs)

  ui_abort(
    "
    Internal error: Unexpected GitHub remote configuration: {.val {cfg$type}}."
  )
}

check_can_push <- function(
  tr = target_repo(github_get = TRUE),
  objective = "for this operation"
) {
  if (isTRUE(tr$can_push)) {
    return(invisible())
  }
  ui_abort(
    "
    You don't seem to have push access for {.val {tr$repo_spec}}, which
    is required {objective}."
  )
}

# github remote configurations -------------------------------------------------
all_configs <- function() {
  c(
    "no_github",
    "ours",
    "theirs",
    "maybe_ours_or_theirs",
    "fork",
    "maybe_fork",
    "fork_cannot_push_origin",
    "fork_upstream_is_not_origin_parent",
    "upstream_but_origin_is_not_fork"
  )
}

read_more <- function() {
  c(
    "i" = "Read more about the GitHub remote configurations that usethis supports at:",
    " " = "{.url https://happygitwithr.com/common-remote-setups.html}."
  )
}

read_more_maybe <- function() {
  c(
    "i" = "Read more about what this GitHub remote configuration means at:",
    " " = "{.url https://happygitwithr.com/common-remote-setups.html}."
  )
}

cfg_no_github <- function(cfg) {
  utils::modifyList(
    cfg,
    list(
      type = "no_github",
      pr_ready = FALSE,
      desc = c(
        "!" = "Neither {.val origin} nor {.val upstream} is a GitHub repo.",
        read_more()
      )
    )
  )
}

cfg_ours <- function(cfg) {
  utils::modifyList(
    cfg,
    list(
      type = "ours",
      pr_ready = TRUE,
      desc = c(
        "i" = "{.val origin} is both the source and primary repo.",
        read_more()
      )
    )
  )
}

cfg_theirs <- function(cfg) {
  configured <- if (cfg$origin$is_configured) "origin" else "upstream"
  utils::modifyList(
    cfg,
    list(
      type = "theirs",
      pr_ready = FALSE,
      desc = c(
        "!" = ui_pre_glue(
          "
                The only configured GitHub remote is {.val <<configured>>},
                which you cannot push to."
        ),
        "i" = "If your goal is to make a pull request, you must fork-and-clone.",
        "i" = "{.fun usethis::create_from_github} can do this.",
        read_more()
      )
    )
  )
}

cfg_maybe_ours_or_theirs <- function(cfg) {
  if (cfg$origin$is_configured) {
    configured <- "origin"
    not_configured <- "upstream"
  } else {
    configured <- "upstream"
    not_configured <- "origin"
  }
  utils::modifyList(
    cfg,
    list(
      type = "maybe_ours_or_theirs",
      pr_ready = NA,
      desc = c(
        "!" = ui_pre_glue(
          "
                {.val <<configured>>} is a GitHub repo and
                {.val <<not_configured>>} is either not configured or is not a
                GitHub repo."
        ),
        "i" = "We may be offline or you may need to configure a GitHub personal
               access token.",
        "i" = "{.run usethis::gh_token_help()} can help with that.",
        read_more_maybe()
      )
    )
  )
}

cfg_fork <- function(cfg) {
  utils::modifyList(
    cfg,
    list(
      type = "fork",
      pr_ready = TRUE,
      desc = c(
        "i" = ui_pre_glue(
          "
                {.val origin} is a fork of {.val <<cfg$upstream$repo_spec>>},
                which is configured as the {.val upstream} remote."
        ),
        read_more()
      )
    )
  )
}

cfg_maybe_fork <- function(cfg) {
  utils::modifyList(
    cfg,
    list(
      type = "maybe_fork",
      pr_ready = NA,
      desc = c(
        "!" = ui_pre_glue(
          "
                Both {.val origin} and {.val upstream} appear to be GitHub
                repos. However, we can't confirm their relationship to each
                other (e.g., fork and fork parent) or your permissions (e.g.
                push access)."
        ),
        "i" = "We may be offline or you may need to configure a GitHub personal
               access token.",
        "i" = "{.run usethis::gh_token_help()} can help with that.",
        read_more_maybe()
      )
    )
  )
}

cfg_fork_cannot_push_origin <- function(cfg) {
  utils::modifyList(
    cfg,
    list(
      type = "fork_cannot_push_origin",
      pr_ready = FALSE,
      desc = c(
        "!" = ui_pre_glue(
          "
                The {.val origin} remote is a fork, but you can't push to it."
        ),
        read_more()
      )
    )
  )
}

cfg_fork_upstream_is_not_origin_parent <- function(cfg) {
  utils::modifyList(
    cfg,
    list(
      type = "fork_upstream_is_not_origin_parent",
      pr_ready = FALSE,
      desc = c(
        "!" = ui_pre_glue(
          "
                The {.val origin} GitHub remote is a fork, but its parent is
                not configured as the {.val upstream} remote."
        ),
        read_more()
      )
    )
  )
}

cfg_upstream_but_origin_is_not_fork <- function(cfg) {
  utils::modifyList(
    cfg,
    list(
      type = "upstream_but_origin_is_not_fork",
      pr_ready = FALSE,
      desc = c(
        "!" = ui_pre_glue(
          "
                Both {.val origin} and {.val upstream} are GitHub remotes, but
                {.val origin} is not a fork and, in particular, is not a fork of
                {.val upstream}."
        ),
        read_more()
      )
    )
  )
}

# construct instances of `github_remote_config` for dev/testing purposes--------
new_no_github <- function() {
  cfg <- new_github_remote_config()
  cfg_no_github(cfg)
}

new_ours <- function() {
  remotes <- data.frame(
    name = "origin",
    url = "https://github.com/OWNER/REPO.git"
  )
  grl <- github_remotes(github_get = FALSE, x = remotes)
  grl$github_got <- grl$perm_known <- TRUE
  grl$default_branch <- "DEFAULT_BRANCH"

  grl$is_fork <- FALSE
  grl$can_push <- grl$can_admin <- TRUE

  cfg <- new_github_remote_config()
  cfg$origin <- utils::modifyList(cfg$origin, grl[grl$remote == "origin", ])
  cfg$host_url <- grl$host_url
  cfg$origin$is_configured <- TRUE
  cfg_ours(cfg)
}

new_theirs <- function() {
  remotes <- data.frame(
    name = "origin",
    url = "https://github.com/OWNER/REPO.git"
  )
  grl <- github_remotes(github_get = FALSE, x = remotes)
  grl$github_got <- grl$perm_known <- TRUE
  grl$default_branch <- "DEFAULT_BRANCH"

  grl$can_push <- grl$can_admin <- FALSE

  cfg <- new_github_remote_config()
  cfg$origin <- utils::modifyList(cfg$origin, grl[grl$remote == "origin", ])
  cfg$host_url <- grl$host_url
  cfg$origin$is_configured <- TRUE
  cfg_theirs(cfg)
}

new_fork <- function() {
  remotes <- data.frame(
    name = c("origin", "upstream"),
    url = c(
      "https://github.com/CONTRIBUTOR/REPO.git",
      "https://github.com/OWNER/REPO.git"
    )
  )
  grl <- github_remotes(github_get = FALSE, x = remotes)
  grl$github_got <- grl$perm_known <- TRUE
  grl$default_branch <- "DEFAULT_BRANCH"

  grl$is_fork <- c(TRUE, FALSE)
  grl$parent_repo_owner <- c("OWNER", NA)
  grl$parent_repo_name <- c("REPO", NA)
  grl$can_push_to_parent <- c(FALSE, NA)
  grl$parent_repo_spec <- make_spec(grl$parent_repo_owner, grl$parent_repo_name)

  grl$can_push <- grl$can_admin <- c(TRUE, FALSE)

  cfg <- new_github_remote_config()
  cfg$origin <- utils::modifyList(cfg$origin, grl[grl$remote == "origin", ])
  cfg$upstream <- utils::modifyList(
    cfg$upstream,
    grl[grl$remote == "upstream", ]
  )
  cfg$host_url <- grl$host_url[1]
  cfg$origin$is_configured <- cfg$upstream$is_configured <- TRUE
  cfg$origin$parent_is_upstream <- TRUE
  cfg_fork(cfg)
}

new_maybe_ours_or_theirs <- function() {
  remotes <- data.frame(
    name = "origin",
    url = "https://github.com/OWNER/REPO.git"
  )
  grl <- github_remotes(github_get = FALSE, x = remotes)
  grl$github_got <- grl$perm_known <- FALSE
  grl$default_branch <- "DEFAULT_BRANCH"

  cfg <- new_github_remote_config()
  cfg$origin <- utils::modifyList(cfg$origin, grl[grl$remote == "origin", ])
  cfg$host_url <- grl$host_url
  cfg$origin$is_configured <- TRUE
  cfg_maybe_ours_or_theirs(cfg)
}

new_maybe_fork <- function() {
  remotes <- data.frame(
    name = c("origin", "upstream"),
    url = c(
      "https://github.com/CONTRIBUTOR/REPO.git",
      "https://github.com/OWNER/REPO.git"
    )
  )
  grl <- github_remotes(github_get = FALSE, x = remotes)
  grl$github_got <- grl$perm_known <- FALSE
  grl$default_branch <- "DEFAULT_BRANCH"

  cfg <- new_github_remote_config()
  cfg$origin <- utils::modifyList(cfg$origin, grl[grl$remote == "origin", ])
  cfg$upstream <- utils::modifyList(
    cfg$upstream,
    grl[grl$remote == "upstream", ]
  )
  cfg$host_url <- grl$host_url[1]
  cfg$origin$is_configured <- cfg$upstream$is_configured <- TRUE
  cfg_maybe_fork(cfg)
}

new_fork_cannot_push_origin <- function() {
  remotes <- data.frame(
    name = c("origin", "upstream"),
    url = c(
      "https://github.com/CONTRIBUTOR/REPO.git",
      "https://github.com/OWNER/REPO.git"
    )
  )
  grl <- github_remotes(github_get = FALSE, x = remotes)
  grl$github_got <- grl$perm_known <- TRUE
  grl$default_branch <- "DEFAULT_BRANCH"

  cfg <- new_github_remote_config()
  cfg$origin <- utils::modifyList(cfg$origin, grl[grl$remote == "origin", ])
  cfg$upstream <- utils::modifyList(
    cfg$upstream,
    grl[grl$remote == "upstream", ]
  )
  cfg$host_url <- grl$host_url[1]
  cfg$origin$is_configured <- cfg$upstream$is_configured <- TRUE

  cfg$origin$parent_is_upstream <- FALSE

  cfg_fork_cannot_push_origin(cfg)
}

new_fork_upstream_is_not_origin_parent <- function() {
  remotes <- data.frame(
    name = c("origin", "upstream"),
    url = c(
      "https://github.com/CONTRIBUTOR/REPO.git",
      "https://github.com/OLD_OWNER/REPO.git"
    )
  )
  grl <- github_remotes(github_get = FALSE, x = remotes)
  grl$github_got <- grl$perm_known <- TRUE
  grl$default_branch <- "DEFAULT_BRANCH"

  grl$is_fork <- c(TRUE, FALSE)
  grl$parent_repo_owner <- c("NEW_OWNER", NA)
  grl$parent_repo_name <- c("REPO", NA)
  grl$can_push_to_parent <- c(FALSE, NA)
  grl$parent_repo_spec <- make_spec(grl$parent_repo_owner, grl$parent_repo_name)
  grl$can_push <- grl$can_admin <- c(TRUE, FALSE)

  cfg <- new_github_remote_config()
  cfg$origin <- utils::modifyList(cfg$origin, grl[grl$remote == "origin", ])
  cfg$upstream <- utils::modifyList(
    cfg$upstream,
    grl[grl$remote == "upstream", ]
  )
  cfg$host_url <- grl$host_url[1]
  cfg$origin$is_configured <- cfg$upstream$is_configured <- TRUE

  cfg_fork_upstream_is_not_origin_parent(cfg)
}

new_upstream_but_origin_is_not_fork <- function() {
  remotes <- data.frame(
    name = c("origin", "upstream"),
    url = c(
      "https://github.com/CONTRIBUTOR/REPO.git",
      "https://github.com/OWNER/REPO.git"
    )
  )
  grl <- github_remotes(github_get = FALSE, x = remotes)
  grl$github_got <- grl$perm_known <- TRUE
  grl$default_branch <- "DEFAULT_BRANCH"

  grl$is_fork <- FALSE

  cfg <- new_github_remote_config()
  cfg$origin <- utils::modifyList(cfg$origin, grl[grl$remote == "origin", ])
  cfg$upstream <- utils::modifyList(
    cfg$upstream,
    grl[grl$remote == "upstream", ]
  )
  cfg$host_url <- grl$host_url[1]
  cfg$origin$is_configured <- cfg$upstream$is_configured <- TRUE

  cfg$origin$parent_is_upstream <- FALSE

  cfg_upstream_but_origin_is_not_fork(cfg)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/utils-glue.R ---
# wrappers that apply as.character() to glue functions

glue_chr <- function(...) {
  as.character(glue(..., .envir = parent.frame(1)))
}

glue_data_chr <- function(.x, ...) {
  as.character(glue_data(.x = .x, ..., .envir = parent.frame(1)))
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/utils-rematch2.R ---
# inlined from
# https://github.com/r-lib/rematch2/commit/aab858e3411810fa107d20db6f936c6b10cbdf3f
# EXCEPT I don't return a tibble

re_match <- function(text, pattern, perl = TRUE, ...) {
  check_string(pattern)
  text <- as.character(text)

  match <- regexpr(pattern, text, perl = perl, ...)

  start <- as.vector(match)
  length <- attr(match, "match.length")
  end <- start + length - 1L

  matchstr <- substring(text, start, end)
  matchstr[start == -1] <- NA_character_

  res <- data.frame(
    stringsAsFactors = FALSE,
    .text = text,
    .match = matchstr
  )

  if (!is.null(attr(match, "capture.start"))) {
    gstart <- attr(match, "capture.start")
    glength <- attr(match, "capture.length")
    gend <- gstart + glength - 1L

    groupstr <- substring(text, gstart, gend)
    groupstr[gstart == -1] <- NA_character_
    dim(groupstr) <- dim(gstart)

    res <- cbind(groupstr, res, stringsAsFactors = FALSE)
  }

  names(res) <- c(attr(match, "capture.names"), ".text", ".match")
  #class(res) <- c("tbl_df", "tbl", class(res))
  res
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/utils-roxygen.R ---
# functions to help reduce duplication and increase consistency in the docs

# repo_spec ----
param_repo_spec <- function(...) {
  template <- glue(
    "
    @param repo_spec \\
    Optional GitHub repo specification in this form: `owner/repo`. \\
    This can usually be inferred from the GitHub remotes of active \\
    project.
    "
  )
  dots <- list2(...)
  if (length(dots) > 0) {
    template <- c(template, dots)
  }
  glue_collapse(template, sep = " ")
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/utils-ui.R ---
# usethis theme ----------------------------------------------------------------
usethis_theme <- function() {
  list(
    # add a "todo" bullet, which is intended to be seen as an unchecked checkbox
    ".bullets .bullet-_" = list(
      "text-exdent" = 2,
      before = function(x) paste0(cli::col_red(cli::symbol$checkbox_off), " ")
    ),
    # historically, usethis has used yellow for this
    ".bullets .bullet-i" = list(
      "text-exdent" = 2,
      before = function(x) paste0(cli::col_yellow(cli::symbol$info), " ")
    ),
    # we have enough color going on already, don't add color to `*` bullets
    ".bullets .bullet-*" = list(
      "text-exdent" = 2,
      before = function(x) paste0(cli::symbol$bullet, " ")
    ),
    # apply quotes to `.field` if we can't style it with color
    span.field = list(transform = single_quote_if_no_color)
  )
}

single_quote_if_no_color <- function(x) quote_if_no_color(x, "'")

quote_if_no_color <- function(x, quote = "'") {
  # copied from googledrive
  # TODO: if a better way appears in cli, use it
  # @gabor says: "if you want to have before and after for the no-color case
  # only, we can have a selector for that, such as:
  # span.field::no-color
  # (but, at the time I write this, cli does not support this yet)
  if (cli::num_ansi_colors() > 1) {
    x
  } else {
    paste0(quote, x, quote)
  }
}

# silence -----------------------------------------------------------------
#' Suppress usethis's messaging
#'
#' Execute a bit of code without usethis's normal messaging.
#'
#' @param code Code to execute with usual UI output silenced.
#'
#' @returns Whatever `code` returns.
#' @export
#' @examples
#' # compare the messaging you see from this:
#' browse_github("usethis")
#' # vs. this:
#' ui_silence(
#'   browse_github("usethis")
#' )
ui_silence <- function(code) {
  withr::with_options(list(usethis.quiet = TRUE), code)
}

is_quiet <- function() {
  isTRUE(getOption("usethis.quiet", default = FALSE))
}

# bullets, helpers, and friends ------------------------------------------------
ui_bullets <- function(text, .envir = parent.frame()) {
  if (is_quiet()) {
    return(invisible())
  }
  cli::cli_div(theme = usethis_theme())
  cli::cli_bullets(text, .envir = .envir)
}

ui_path_impl <- function(x, base = NULL) {
  is_directory <- is_dir(x) | grepl("/$", x)
  if (is.null(base)) {
    x <- proj_rel_path(x)
  } else if (!identical(base, NA)) {
    x <- path_rel(x, base)
  }

  # rationalize trailing slashes
  x <- path_tidy(x)
  x[is_directory] <- paste0(x[is_directory], "/")

  unclass(x)
}

# shorter form for compactness, because this is typical usage:
# ui_bullets("blah blah {.path {pth(some_path)}}")
pth <- ui_path_impl

ui_code_snippet <- function(
  x,
  copy = rlang::is_interactive(),
  language = c("R", ""),
  interpolate = TRUE,
  .envir = parent.frame()
) {
  language <- arg_match(language)

  indent <- function(x, first = "  ", indent = first) {
    x <- gsub("\n", paste0("\n", indent), x)
    paste0(first, x)
  }

  x <- glue_collapse(x, "\n")
  if (interpolate) {
    x <- glue(x, .envir = .envir)
    # what about literal `{` or `}`?
    # use `interpolate = FALSE`, if appropriate
    # double them, i.e. `{{` or `}}`
    # open issue/PR about adding `.open` and `.close`
  }

  if (!is_quiet()) {
    # the inclusion of `.envir = .envir` leads to test failure
    # I'm consulting with Gabor on this
    # leaving it out seems fine for my use case
    # cli::cli_code(indent(x), language = language, .envir = .envir)
    cli::cli_code(indent(x), language = language)
  }

  if (copy && clipr::clipr_available()) {
    x_no_ansi <- cli::ansi_strip(x)
    clipr::write_clip(x_no_ansi)
    style_subtle <- cli::combine_ansi_styles(
      cli::make_ansi_style("grey"),
      cli::style_italic
    )
    ui_bullets(c(" " = style_subtle("[Copied to clipboard]")))
  }

  invisible(x)
}

# inspired by gargle::gargle_map_cli() and gargle::bulletize()
usethis_map_cli <- function(x, ...) UseMethod("usethis_map_cli")

#' @export
usethis_map_cli.default <- function(x, ...) {
  ui_abort(c(
    "x" = "Don't know how to {.fun usethis_map_cli} an object of class
           {.obj_type_friendly {x}}."
  ))
}

#' @export
usethis_map_cli.NULL <- function(x, ...) NULL

#' @export
usethis_map_cli.character <- function(
  x,
  template = "{.val <<x>>}",
  .open = "<<",
  .close = ">>",
  ...
) {
  as.character(glue(template, .open = .open, .close = .close))
}

ui_pre_glue <- function(..., .envir = parent.frame()) {
  glue(..., .open = "<<", .close = ">>", .envir = .envir)
}

ui_escape_glue <- function(x) {
  gsub("([{}])", "\\1\\1", x)
}


bulletize <- function(x, bullet = "*", n_show = 5, n_fudge = 2) {
  n <- length(x)
  n_show_actual <- compute_n_show(n, n_show, n_fudge)
  out <- utils::head(x, n_show_actual)
  n_not_shown <- n - n_show_actual

  out <- set_names(out, rep_along(out, bullet))

  if (n_not_shown == 0) {
    out
  } else {
    c(out, " " = glue("{cli::symbol$ellipsis} and {n_not_shown} more"))
  }
}

# I don't want to do "... and x more" if x is silly, i.e. 1 or 2
compute_n_show <- function(n, n_show_nominal = 5, n_fudge = 2) {
  if (n > n_show_nominal && n - n_show_nominal > n_fudge) {
    n_show_nominal
  } else {
    n
  }
}

kv_line <- function(key, value, .envir = parent.frame()) {
  cli::cli_div(theme = usethis_theme())

  key_fmt <- cli::format_inline(key, .envir = .envir)

  # this must happen first, before `value` has been forced
  value_fmt <- cli::format_inline("{.val {value}}")
  # but we might actually want something other than value_fmt
  if (is.null(value)) {
    value <- ui_special()
  }
  if (inherits(value, "AsIs")) {
    value_fmt <- cli::format_inline(value, .envir = .envir)
  }

  ui_bullets(c("*" = "{key_fmt}: {value_fmt}"))
}

ui_special <- function(x = "unset") {
  force(x)
  I(glue("{cli::col_grey('<[x]>')}", .open = "[", .close = "]"))
}

# errors -----------------------------------------------------------------------
ui_abort <- function(message, ..., class = NULL, .envir = parent.frame()) {
  cli::cli_div(theme = usethis_theme())

  nms <- names2(message)
  default_nms <- rep_along(message, "i")
  default_nms[1] <- "x"
  nms <- ifelse(nzchar(nms), nms, default_nms)
  names(message) <- nms

  cli::cli_abort(
    message,
    class = c(class, "usethis_error"),
    .envir = .envir,
    ...
  )
}

# questions --------------------------------------------------------------------
ui_yep <- function(
  x,
  yes = c(
    "Yes",
    "Definitely",
    "For sure",
    "Yup",
    "Yeah",
    "I agree",
    "Absolutely"
  ),
  no = c("No way", "Not now", "Negative", "No", "Nope", "Absolutely not"),
  n_yes = 1,
  n_no = 2,
  shuffle = TRUE,
  .envir = parent.frame()
) {
  if (!is_interactive()) {
    ui_abort(c(
      "User input required, but session is not interactive.",
      "Query: {.val {x}}"
    ))
  }

  n_yes <- min(n_yes, length(yes))
  n_no <- min(n_no, length(no))

  qs <- c(sample(yes, n_yes), sample(no, n_no))

  if (shuffle) {
    qs <- sample(qs)
  }

  cli::cli_inform(x, .envir = .envir)
  out <- utils::menu(qs)
  out != 0L && qs[[out]] %in% yes
}

ui_nah <- function(
  x,
  yes = c(
    "Yes",
    "Definitely",
    "For sure",
    "Yup",
    "Yeah",
    "I agree",
    "Absolutely"
  ),
  no = c("No way", "Not now", "Negative", "No", "Nope", "Absolutely not"),
  n_yes = 1,
  n_no = 2,
  shuffle = TRUE,
  .envir = parent.frame()
) {
  # TODO(jennybc): is this correct in the case of no selection / cancelling?
  !ui_yep(
    x = x,
    yes = yes,
    no = no,
    n_yes = n_yes,
    n_no = n_no,
    shuffle = shuffle,
    .envir = .envir
  )
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/utils.R ---
can_overwrite <- function(path) {
  if (!file_exists(path)) {
    return(TRUE)
  }

  if (getOption("usethis.overwrite", FALSE)) {
    # don't activate a project
    # don't assume `path` is in the active project
    if (is_in_proj(path) && uses_git()) {
      # path is in active project
      return(TRUE)
    }
    if (
      possibly_in_proj(path) && # path is some other project
        with_project(proj_find(path), uses_git(), quiet = TRUE)
    ) {
      return(TRUE)
    }
  }

  if (is_interactive()) {
    ui_yep(c("!" = "Overwrite pre-existing file {.path {pth(path)}}?"))
  } else {
    FALSE
  }
}

check_is_named_list <- function(x, nm = deparse(substitute(x))) {
  if (!is_list(x)) {
    ui_abort("{.code {nm}} must be a list, not {.obj_type_friendly {x}}.")
  }
  if (!is_dictionaryish(x)) {
    ui_abort(
      "Names of {.code {nm}} must be non-missing, non-empty, and non-duplicated."
    )
  }
  x
}

dots <- function(...) {
  eval(substitute(alist(...)))
}

asciify <- function(x) {
  check_character(x)
  gsub("[^a-zA-Z0-9_-]+", "-", x)
}

compact <- function(x) {
  is_empty <- vapply(x, function(x) length(x) == 0, logical(1))
  x[!is_empty]
}

# Needed for mocking
is_installed <- function(pkg) {
  rlang::is_installed(pkg)
}

isFALSE <- function(x) {
  identical(x, FALSE)
}

isNA <- function(x) {
  length(x) == 1 && is.na(x)
}

path_first_existing <- function(paths) {
  # manual loop with explicit use of `[[` to retain "fs" class
  for (i in seq_along(paths)) {
    path <- paths[[i]]
    if (file_exists(path)) {
      return(path)
    }
  }

  NULL
}

is_online <- function(host) {
  bare_host <- sub("^https?://(.*)$", "\\1", host)
  !is.null(curl::nslookup(bare_host, error = FALSE))
}

year <- function() format(Sys.Date(), "%Y")

pluck_lgl <- function(.x, ...) {
  as.logical(purrr::pluck(.x, ..., .default = NA))
}

pluck_chr <- function(.x, ...) {
  as.character(purrr::pluck(.x, ..., .default = NA))
}

pluck_int <- function(.x, ...) {
  as.integer(purrr::pluck(.x, ..., .default = NA))
}

is_windows <- function() {
  .Platform$OS.type == "windows"
}

is_linux <- function() {
  identical(tolower(Sys.info()[["sysname"]]), "linux")
}

# For stability of `stringsAsFactors` across versions
data.frame <- function(..., stringsAsFactors = FALSE) {
  base::data.frame(..., stringsAsFactors = stringsAsFactors)
}

# wrapper around check_name() from import-standalone-types-check.R
# for the common case when NULL is allowed (often default)
maybe_name <- function(x, ..., arg = caller_arg(x), call = caller_env()) {
  check_name(x, ..., allow_null = TRUE, arg = arg, call = call)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/version.R ---
#' Increment package version
#'
#' @description
#'

#' usethis supports semantic versioning, which is described in more detail in
#' the [version
#' section](https://r-pkgs.org/lifecycle.html#sec-lifecycle-version-number) of [R
#' Packages](https://r-pkgs.org). A version number breaks down like so:
#'
#' ```
#' <major>.<minor>.<patch>       (released version)
#' <major>.<minor>.<patch>.<dev> (dev version)
#' ```

#' `use_version()` increments the "Version" field in `DESCRIPTION`, adds a new
#' heading to `NEWS.md` (if it exists), commits those changes (if package uses
#' Git), and optionally pushes (if safe to do so). It makes the same update to a
#' line like `PKG_version = "x.y.z";` in `src/version.c` (if it exists).
#'

#' `use_dev_version()` increments to a development version, e.g. from 1.0.0 to
#' 1.0.0.9000. If the existing version is already a development version with
#' four components, it does nothing. Thin wrapper around `use_version()`.
#'

#' @param which A string specifying which level to increment, one of: "major",
#'   "minor", "patch", "dev". If `NULL`, user can choose interactively.

#'
#' @seealso The [version
#'   section](https://r-pkgs.org/lifecycle.html#sec-lifecycle-version-number) of [R
#'   Packages](https://r-pkgs.org).
#'
#' @examples
#' \dontrun{
#' ## for interactive selection, do this:
#' use_version()
#'
#' ## request a specific type of increment
#' use_version("minor")
#' use_dev_version()
#' }
#'
#' @name use_version
NULL

#' @rdname use_version
#' @param push If `TRUE`, also attempts to push the commits to the remote
#'   branch.
#' @export
use_version <- function(which = NULL, push = FALSE) {
  if (is.null(which) && !is_interactive()) {
    return(invisible(FALSE))
  }

  check_is_package("use_version()")
  challenge_uncommitted_changes(
    msg = "There are uncommitted changes and you're about to bump version"
  )

  new_ver <- choose_version("What should the new version be?", which)
  if (is.null(new_ver)) {
    return(invisible(FALSE))
  }

  proj_desc_field_update("Version", new_ver, overwrite = TRUE)
  if (names(new_ver) == "dev") {
    use_news_heading("(development version)")
  } else {
    use_news_heading(new_ver)
  }

  use_c_version(new_ver)

  git_ask_commit(
    glue("Increment version number to {new_ver}"),
    untracked = TRUE,
    push = push,
    paths = c("DESCRIPTION", "NEWS.md", path("src", "version.c"))
  )

  invisible(TRUE)
}

#' @rdname use_version
#' @export
use_dev_version <- function(push = FALSE) {
  check_is_package("use_dev_version()")
  if (is_dev_version()) {
    return(invisible())
  }
  use_version(which = "dev", push = push)
}

choose_version <- function(message, which = NULL) {
  versions <- bump_version()
  rtypes <- names(versions)
  which <- which %||% rtypes
  which <- arg_match(which, values = rtypes, multiple = TRUE)
  versions <- versions[which]

  if (length(versions) == 1) {
    return(versions)
  }

  choice <- utils::menu(
    choices = glue(
      "{format(names(versions), justify = 'right')} --> {versions}"
    ),
    title = glue(
      "Current version is {proj_version()}.\n",
      "{message} (0 to exit)"
    )
  )

  if (choice == 0) {
    invisible()
  } else {
    # Not using `[[` even though there is only 1 `choice`,
    # because that removes the names from `versions`
    versions[choice]
  }
}

bump_version <- function(ver = proj_version()) {
  bumps <- c("major", "minor", "patch", "dev")
  vapply(bumps, bump_, character(1), ver = ver)
}

bump_ <- function(x, ver) {
  d <- desc::desc(text = paste0("Version: ", ver))
  suppressMessages(d$bump_version(x)$get("Version")[[1]])
}

use_c_version <- function(ver) {
  version_path <- proj_path("src", "version.c")

  if (!file_exists(version_path)) {
    return()
  }

  hint <- glue("{project_name()}_version")
  ui_bullets(c(
    "v" = "Setting {.field {hint}} to {.val {ver}} {.path {pth(version_path)}}."
  ))

  lines <- read_utf8(version_path)

  re <- glue("(^.*{project_name()}_version = \")([0-9.]+)(\";$)")
  lines <- gsub(re, glue("\\1{ver}\\3"), lines)

  write_utf8(version_path, lines)
}

is_dev_version <- function(version = proj_version()) {
  ver <- package_version(version)
  length(unlist(ver)) > 3
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/vignette.R ---
#' Create a vignette or article
#'
#' Creates a new vignette or article in `vignettes/`. Articles are a special
#' type of vignette that appear on pkgdown websites, but are not included
#' in the package itself (because they are added to `.Rbuildignore`
#' automatically).
#'
#' @section General setup:
#' * Adds needed packages to `DESCRIPTION`.
#' * Adds `inst/doc` to `.gitignore` so built vignettes aren't tracked.
#' * Adds `vignettes/*.html` and `vignettes/*.R` to `.gitignore` so
#'   you never accidentally track rendered vignettes.
#' * For `*.qmd`, adds Quarto-related patterns to `.gitignore` and
#'   `.Rbuildignore`.
#' @param name File name to use for new vignette. Should consist only of
#'   numbers, letters, `_` and `-`. Lower case is recommended. Can include the
#'   `".Rmd"` or `".qmd"` file extension, which also dictates whether to place
#'   an R Markdown or Quarto vignette. R Markdown (`".Rmd"`) is the current
#'   default, but it is anticipated that Quarto (`".qmd"`) will become the
#'   default in the future.
#' @param title The title of the vignette. If not provided, a title is generated
#'   from `name`.
#' @seealso
#' * The [vignettes chapter](https://r-pkgs.org/vignettes.html) of
#'   [R Packages](https://r-pkgs.org)
#' * The pkgdown vignette on Quarto:
#'   `vignette("quarto", package = "pkgdown")`
#' * The quarto (as in the R package) vignette on HTML vignettes:
#'   `vignette("hello", package = "quarto")`
#' @export
#' @examples
#' \dontrun{
#' use_vignette("how-to-do-stuff", "How to do stuff")
#' use_vignette("r-markdown-is-classic.Rmd", "R Markdown is classic")
#' use_vignette("quarto-is-cool.qmd", "Quarto is cool")
#' }
use_vignette <- function(name, title = NULL) {
  check_is_package("use_vignette()")
  check_required(name)
  maybe_name(title)

  ext <- get_vignette_extension(name)
  if (ext == "qmd") {
    check_installed("quarto")
    check_installed("pkgdown", version = "2.1.0")
  }

  name <- path_ext_remove(name)
  check_vignette_name(name)
  title <- title %||% name

  use_dependency("knitr", "Suggests")
  use_git_ignore("inst/doc")

  if (tolower(ext) == "rmd") {
    use_dependency("rmarkdown", "Suggests")
    proj_desc_field_update(
      "VignetteBuilder",
      "knitr",
      overwrite = TRUE,
      append = TRUE
    )
    use_vignette_template("vignette.Rmd", name, title)
  } else {
    use_dependency("quarto", "Suggests")
    proj_desc_field_update(
      "VignetteBuilder",
      "quarto",
      overwrite = TRUE,
      append = TRUE
    )
    use_vignette_template("vignette.qmd", name, title)
  }

  invisible()
}

#' @export
#' @rdname use_vignette
use_article <- function(name, title = NULL) {
  check_is_package("use_article()")
  check_required(name)
  maybe_name(title)

  ext <- get_vignette_extension(name)
  if (ext == "qmd") {
    check_installed("quarto")
    check_installed("pkgdown", version = "2.1.0")
  }

  name <- path_ext_remove(name)
  title <- title %||% name

  if (tolower(ext) == "rmd") {
    proj_desc_field_update(
      "Config/Needs/website",
      "rmarkdown",
      overwrite = TRUE,
      append = TRUE
    )
    use_vignette_template("article.Rmd", name, title, subdir = "articles")
  } else {
    use_dependency("quarto", "Suggests")
    proj_desc_field_update(
      "Config/Needs/website",
      "quarto",
      overwrite = TRUE,
      append = TRUE
    )
    use_vignette_template("article.qmd", name, title, subdir = "articles")
  }
  use_build_ignore("vignettes/articles")

  invisible()
}

use_vignette_template <- function(template, name, title, subdir = NULL) {
  check_name(template)
  check_name(name)
  check_name(title)
  maybe_name(subdir)

  ext <- get_vignette_extension(template)

  if (is.null(subdir)) {
    target_dir <- "vignettes"
  } else {
    target_dir <- path("vignettes", subdir)
  }

  use_directory(target_dir)

  use_git_ignore(c("*.html", "*.R"), directory = target_dir)
  if (ext == "qmd") {
    use_git_ignore("**/.quarto/")
    use_git_ignore("*_files", target_dir)
    use_build_ignore(path(target_dir, ".quarto"))
    use_build_ignore(path(target_dir, "*_files"))
  }

  path <- path(target_dir, asciify(name), ext = ext)

  data <- list(
    Package = project_name(),
    vignette_title = title,
    braced_vignette_title = glue("{{{title}}}")
  )

  use_template(template, save_as = path, data = data, open = TRUE)

  path
}

check_vignette_name <- function(name) {
  if (!valid_vignette_name(name)) {
    ui_abort(c(
      "{.val {name}} is not a valid filename for a vignette. It must:",
      "Start with a letter.",
      "Contain only letters, numbers, '_', and '-'."
    ))
  }
}

# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Writing-package-vignettes
# "To ensure that they can be accessed from a browser (as an HTML index is
# provided), the file names should start with an ASCII letter and be comprised
# entirely of ASCII letters or digits or hyphen or underscore."
valid_vignette_name <- function(x) {
  grepl("^[[:alpha:]][[:alnum:]_-]*$", x)
}

check_vignette_extension <- function(ext) {
  # Quietly accept "rmd" here, tho we'll always write ".Rmd" in such a filepath
  if (!ext %in% c("Rmd", "rmd", "qmd")) {
    valid_exts_cli <- cli::cli_vec(
      c("Rmd", "qmd"),
      style = list("vec-sep2" = " or ")
    )
    ui_abort(c(
      "Unsupported file extension: {.val {ext}}",
      "usethis can only create a vignette or article with one of these
       extensions: {.val {valid_exts_cli}}."
    ))
  }
}

get_vignette_extension <- function(name) {
  ext <- path_ext(name)
  if (nzchar(ext)) {
    check_vignette_extension(ext)
  } else {
    ext <- "Rmd"
  }
  ext
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/vscode.R ---
# unexported function we are experimenting with
use_vscode_debug <- function(open = rlang::is_interactive()) {
  create_vscode_directory(ignore = TRUE)

  deps <- proj_deps()
  lt_pkgs <- deps$package[deps$type == "LinkingTo"]
  possibly_path_package <- purrr::possibly(path_package, otherwise = NA)
  lt_paths <- map_chr(lt_pkgs, \(x) possibly_path_package(x, "include"))
  lt_paths <- purrr::discard(lt_paths, is.na)
  # this is a bit fiddly, but it produces the desired JSON when lt_paths has
  # length 0 or > 0
  # I should probably come back and use jsonlite here instead of use_template()
  lt_paths <- encodeString(lt_paths, quote = '"')
  lt_paths <- glue("        {lt_paths},")
  lt_paths <- glue_collapse(lt_paths, sep = "\n")
  if (length(lt_paths) > 0) {
    lt_paths <- paste0("\n", lt_paths)
  }

  use_template(
    "vscode-c_cpp_properties.json",
    save_as = path(".vscode", "c_cpp_properties.json"),
    data = list(linking_to_includes = lt_paths),
    ignore = FALSE, # the .vscode directory is already ignored
    open = open
  )
  use_template(
    "vscode-launch.json",
    save_as = path(".vscode", "launch.json"),
    ignore = FALSE, # the .vscode directory is already ignored
    open = open
  )

  usethis::use_directory("debug", ignore = TRUE)
  use_template(
    "vscode-debug.R",
    save_as = path("debug", "debug.R"),
    ignore = FALSE, # the debug directory is already ignored
    open = open
  )

  invisible(TRUE)
}

create_vscode_directory <- function(ignore = FALSE) {
  use_directory(".vscode", ignore = ignore)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/R/write.R ---
#' Write into or over a file
#'
#' Helpers to write into or over a new or pre-existing file. Designed mostly for
#' for internal use. File is written with UTF-8 encoding.
#'
#' @name write-this
#' @param path Path to target file. It is created if it does not exist, but the
#'   parent directory must exist.
#' @param lines Character vector of lines. For `write_union()`, these are lines
#'   to add to the target file, if not already present. For `write_over()`,
#'   these are the exact lines desired in the target file.
#' @param quiet Logical. Whether to message about what is happening.
#' @return Logical indicating whether a write occurred, invisibly.
#' @keywords internal
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' write_union("a_file", letters[1:3])
#' readLines("a_file")
#' write_union("a_file", letters[1:5])
#' readLines("a_file")
#'
#' write_over("another_file", letters[1:3])
#' readLines("another_file")
#' write_over("another_file", letters[1:3])
#' \dontrun{
#' ## will error if user isn't present to approve the overwrite
#' write_over("another_file", letters[3:1])
#' }
#'
#' ## clean up
#' file.remove("a_file", "another_file")
#' \dontshow{
#' setwd(.old_wd)
#' }
NULL

#' @describeIn write-this writes lines to a file, taking the union of what's
#'   already there, if anything, and some new lines. Note, there is no explicit
#'   promise about the line order. Designed to modify simple config files like
#'   `.Rbuildignore` and `.gitignore`.
#' @export
write_union <- function(path, lines, quiet = FALSE) {
  check_name(path)
  check_character(lines)
  check_bool(quiet)
  path <- user_path_prep(path)

  if (file_exists(path)) {
    existing_lines <- read_utf8(path)
  } else {
    existing_lines <- character()
  }

  new <- setdiff(lines, existing_lines)
  if (length(new) == 0) {
    return(invisible(FALSE))
  }

  if (!quiet) {
    ui_bullets(c("v" = "Adding {.val {new}} to {.path {pth(path)}}."))
  }

  all <- c(existing_lines, new)
  write_utf8(path, all)
}

#' @describeIn write-this writes a file with specific lines, creating it if
#'   necessary or overwriting existing, if proposed contents are not identical
#'   and user is available to give permission.
#' @param overwrite Force overwrite of existing file?
#' @export
write_over <- function(path, lines, quiet = FALSE, overwrite = FALSE) {
  check_name(path)
  check_character(lines)
  stopifnot(length(lines) > 0)
  check_bool(quiet)
  check_bool(overwrite)
  path <- user_path_prep(path)

  if (same_contents(path, lines)) {
    return(invisible(FALSE))
  }

  if (overwrite || can_overwrite(path)) {
    if (!quiet) {
      ui_bullets(c("v" = "Writing {.path {pth(path)}}."))
    }
    write_utf8(path, lines)
  } else {
    if (!quiet) {
      ui_bullets(c("i" = "Leaving {.path {pth(path)}} unchanged."))
    }
    invisible(FALSE)
  }
}

read_utf8 <- function(path, n = -1L) {
  base::readLines(path, n = n, encoding = "UTF-8", warn = FALSE)
}

write_utf8 <- function(path, lines, append = FALSE, line_ending = NULL) {
  check_name(path)
  check_character(lines)

  file_mode <- if (append) "ab" else "wb"
  con <- file(path, open = file_mode, encoding = "utf-8")
  withr::defer(close(con))

  if (is.null(line_ending)) {
    if (is_in_proj(path)) {
      # path is in active project
      line_ending <- proj_line_ending()
    } else if (possibly_in_proj(path)) {
      # path is some other project
      line_ending <-
        with_project(proj_find(path), proj_line_ending(), quiet = TRUE)
    } else {
      line_ending <- platform_line_ending()
    }
  }

  # convert embedded newlines
  lines <- gsub("\r?\n", line_ending, lines)
  base::writeLines(enc2utf8(lines), con, sep = line_ending, useBytes = TRUE)

  invisible(TRUE)
}

same_contents <- function(path, contents) {
  if (!file_exists(path)) {
    return(FALSE)
  }

  identical(read_utf8(path), contents)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/inst/templates/citation-template.R ---
bibentry(
  bibtype = "Article",
  title = ,
  author = ,
  journal = ,
  year = ,
  volume = ,
  number = ,
  pages = ,
  doi =
)


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/inst/templates/junit-testthat.R ---
library(testthat)
library({{{ name }}})

if (requireNamespace("xml2")) {
  test_check("{{{ name }}}", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
} else {
  test_check("{{{ name }}}")
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/inst/templates/packagename-data-prep.R ---
## code to prepare `{{{name}}}` dataset goes here

usethis::use_data({{{name}}}, overwrite = TRUE)


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/inst/templates/packagename-package.R ---
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end

## mockable bindings: start
## mockable bindings: end
NULL


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/inst/templates/pipe.R ---
#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/inst/templates/test-example-2.1.R ---
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/inst/templates/testthat.R ---
# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library({{{ name }}})

test_check("{{{ name }}}")


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/inst/templates/vscode-debug.R ---
devtools::clean_dll()
devtools::load_all()

1 + 1


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/man/roxygen/templates/double-auth.R ---
#' @section Git/GitHub Authentication:

#' Many usethis functions, including those documented here, potentially interact
#' with GitHub in two different ways:

#' * Via the GitHub REST API. Examples: create a repo, a fork, or a pull
#' request.

#' * As a conventional Git remote. Examples: clone, fetch, or push.
#'

#' Therefore two types of auth can happen and your credentials must be
#' discoverable. Which credentials do we mean?
#'

#' * A GitHub personal access token (PAT) must be discoverable by the gh
#'   package, which is used for GitHub operations via the REST API. See
#'   [gh_token_help()] for more about getting and configuring a PAT.

#' * If you use the HTTPS protocol for Git remotes, your PAT is also used for
#'   Git operations, such as `git push`. Usethis uses the gert package for this,
#'   so the PAT must be discoverable by gert. Generally gert and gh will
#'   discover and use the same PAT. This ability to "kill two birds with one
#'   stone" is why HTTPS + PAT is our recommended auth strategy for those new
#'   to Git and GitHub and PRs.
#' * If you use SSH remotes, your SSH keys must also be discoverable, in
#'   addition to your PAT. The public key must be added to your GitHub account.
#'
#' Git/GitHub credential management is covered in a dedicated article:
#' [Managing Git(Hub) Credentials](https://usethis.r-lib.org/articles/articles/git-credentials.html)


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/manual/manual-create-from-github-private-repo.R ---
devtools::load_all("~/rrr/usethis")
library(testthat)
library(fs)

repo_name <- "crewsocks"
gh_account <- gh::gh_whoami()
(me <- gh_account$login)

# remove any pre-existing repo
gh::gh(
  "DELETE /repos/:username/:pkg",
  username = me,
  pkg = repo_name
)
dir_delete(path(usethis:::conspicuous_place(), repo_name))
expect_false(dir_exists(path(usethis:::conspicuous_place(), repo_name)))

# create the repo
gh::gh(
  "POST /user/repos",
  name = repo_name,
  description = "usethis manual test repo",
  auto_init = TRUE, # note this means default branch will be `main`
  private = TRUE
)

## this should work
x <- create_from_github(paste0(me, "/", repo_name), open = FALSE)

expect_equal(path_file(x), "crewsocks")
expect_true(dir_exists(x))
expect_true(file_exists(path(x, "crewsocks.Rproj")))
expect_match(
  gert::git_remote_list(repo = x)$url,
  "^https"
)

## cleanup
dir_delete(x)

gh::gh(
  "DELETE /repos/:username/:pkg",
  username = me,
  pkg = repo_name
)


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/manual/manual-create-from-github.R ---
devtools::load_all("~/rrr/usethis")
library(fs)

# maybe set a protocol
#use_git_protocol("ssh")
git_protocol()

# check that a GitHub PAT is configured
(gh_account <- gh::gh_whoami())
(me <- gh_account$login)

# this repo was chosen because it was first one listed for the cran gh user
# the day I made this, i.e., it's totally arbitrary

# make sure local copy does not exist; this will error if doesn't pre-exist
dir_delete("~/tmp/TailRank")

# make sure user doesn't have pre-existing fork; this will 404 if not
gh::gh(
  "DELETE /repos/:username/:pkg",
  username = me,
  pkg = "TailRank"
)

# create from repo I do not have push access to
# fork = FALSE
x <- create_from_github(
  "cran/TailRank",
  destdir = "~/tmp",
  fork = FALSE,
  open = FALSE
)
with_project(x, git_sitrep())
dir_delete(x)

# create from repo I do not have push access to
# fork = TRUE
x <- create_from_github(
  "cran/TailRank",
  destdir = "~/tmp",
  fork = TRUE,
  open = FALSE
)
# fork and clone --> should see origin and upstream remotes
gert::git_branch_list(x, repo = x)
expect_setequal(
  gert::git_remote_list(repo = x)$name,
  c("origin", "upstream")
)
expect_equal(
  with_project(x, usethis:::git_branch_tracking()),
  "upstream/master"
)
dir_delete(x)
gh::gh(
  "DELETE /repos/:username/:pkg",
  username = me,
  pkg = "TailRank"
)

# create from repo I do not have push access to
# fork = NA
x <- create_from_github(
  "cran/TailRank",
  destdir = "~/tmp",
  fork = NA,
  open = FALSE
)
# fork and clone --> should see origin and upstream remotes
expect_setequal(
  gert::git_remote_list(repo = x)$name,
  c("origin", "upstream")
)
dir_delete(x)

gh::gh(
  "DELETE /repos/:username/:pkg",
  username = me,
  pkg = "TailRank"
)

# a repo I created just for testing, make sure local copy doesn't pre-exist
dir_delete("~/tmp/ethel")

# create from repo I DO have push access to
# fork = FALSE
x <- create_from_github("jennybc/ethel", "~/tmp", fork = FALSE, open = TRUE)
# go make a local edit and push to confirm origin remote is properly setup
dir_delete(x)

# create from repo I do have push access to
# fork = TRUE
x <- create_from_github(
  "jennybc/ethel",
  destdir = "~/tmp",
  fork = TRUE,
  open = FALSE
)
# expect error because I own it and can't fork it
# make sure we didn't leave an empty directory behind
expect_false(dir_exists("~/tmp/ethel"))

# create from repo I do have push access to
# fork = NA
x <- create_from_github(
  "jennybc/ethel",
  destdir = "~/tmp",
  fork = NA,
  open = FALSE
)
# gets created, as clone but no fork
dir_delete(x)

# explore "no token" situations
gitcreds::gitcreds_delete()
gh::gh_whoami()

dir_delete("~/tmp/TailRank")

# create from repo I do not have push access to
# fork = FALSE
x <- create_from_github(
  "cran/TailRank",
  destdir = "~/tmp",
  fork = FALSE,
  open = FALSE
)
# created, clone, origin remote is cran/TailRank
dat <- gert::git_remote_list(repo = x)
expect_equal(dat$name, "origin")
#expect_equal(dat$url, "git@github.com:cran/TailRank.git")
expect_equal(dat$url, "https://github.com/cran/TailRank.git")

dir_delete(x)

# create from repo I do not have push access to
# fork = TRUE
x <- create_from_github(
  "cran/TailRank",
  destdir = "~/tmp",
  fork = TRUE,
  open = FALSE
)
# expect error because PAT not available

# create from repo I do not have push access to
# fork = NA
x <- create_from_github(
  "cran/TailRank",
  destdir = "~/tmp",
  fork = NA,
  open = FALSE
)
# expect error because PAT not available AND fork = NA


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/manual/manual-fork-upstream-is-not-origin-parent.R ---
pkgload::unload("devtools")
devtools::load_all("~/rrr/usethis")
attachNamespace("devtools")
library(testthat)

x <- create_from_github(
  "r-lib/gh",
  destdir = "~/tmp",
  fork = TRUE,
  open = FALSE,
  protocol = "https"
)
local_project(x)

(r <- git_remotes())
# r-pkgs is what r-lib used to be called
use_git_remote("upstream", sub("r-lib", "r-pkgs", r$upstream), overwrite = TRUE)

(r <- git_remotes())
expect_equal(r$origin, "https://github.com/jennybc/gh.git")
expect_equal(r$upstream, "https://github.com/r-pkgs/gh.git")

check_pr_readiness()
err <- rlang::last_error()

expect_s3_class(err, class = "usethis_error_bad_github_config")

# capture github remote data to use in an actual test
# datapasta::dpasta(github_remotes())

withr::deferred_run()
fs::dir_delete(x)


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/manual/manual-git-sitrep.R ---
# capturing some manual tests re: detecting missing user email or name
# https://github.com/r-lib/usethis/pull/1721

dat <- gert::git_config_global()
if ("user.name" %in% dat$name) {
  old_name <- dat$value[dat$name == "user.name"]
  usethis::use_git_config(user.name = NULL)
  withr::defer(usethis::use_git_config(user.name = old_name))
}
if ("user.email" %in% dat$name) {
  old_email <- dat$value[dat$name == "user.email"]
  usethis::use_git_config(user.email = NULL)
  withr::defer(usethis::use_git_config(user.email = old_email))
}
usethis::git_sitrep(scope = "user")
usethis::git_sitrep(scope = "project")
usethis::git_sitrep()
withr::deferred_run()

dat <- gert::git_config_global()
if ("user.name" %in% dat$name) {
  old_name <- dat[dat$name == "user.name", ]$value
  usethis::use_git_config(user.name = NULL)
  withr::defer(usethis::use_git_config(user.name = old_name))
}
usethis::git_sitrep(scope = "user")
usethis::git_sitrep(scope = "project")
usethis::git_sitrep()
withr::deferred_run()

dat <- gert::git_config_global()
if ("user.email" %in% dat$name) {
  old_email <- dat[dat$name == "user.email", ]$value
  usethis::use_git_config(user.email = NULL)
  withr::defer(usethis::use_git_config(user.email = old_email))
}
usethis::git_sitrep(scope = "user")
usethis::git_sitrep(scope = "project")
usethis::git_sitrep()
withr::deferred_run()


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/manual/manual-list-ghe-repos.R ---
# this is not a manual test per se, but can help me find repos with specific
# properties when I need to test against GHE
# e.g. repos I have access to but do not own

library(tidyverse)

Sys.setenv(GITHUB_API_URL = "https://github.ubc.ca")

x <- gh::gh("GET /user/repos", .limit = 100)
length(x)
dat <- tibble(payload = x)
dat |>
  hoist(payload, "full_name") |>
  print(n = Inf)

create_from_github("github-administration/migration", destdir = "~/tmp")

create_from_github(
  "https://github.ubc.ca/github-administration/migration",
  destdir = "~/tmp",
  fork = FALSE
)


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/manual/manual-pr-functions.R ---
devtools::load_all("~/rrr/usethis")

pkgname <- "grumpy-llama"
(pkgpath <- path_temp(pkgname))
create_local_package(pkgpath)
proj_sitrep()

# say YES to the commit
use_git()

# say YES to the commit
use_github(private = TRUE)

# no non-default branches
pr_resume()
# should exit w/ no big fuss

# no open PRs
pr_fetch()
# should exit w/ no big fuss

pr_init("feature")
use_readme_md(open = FALSE)
gert::git_add("README.md")
gert::git_commit("Add README")

pr_view()
# doesn't work, because current branch no yet associated with a PR

pr_pause()

pr_resume()
# offers to switch to the single existing branch

# remember to actually create the PR in the browser
pr_push()

pr_view()
browse_github_pulls()

pr_fetch()
# presents my one existing PR (the branch I'm on), which I can select

pr_pause()

pr_resume()

# agree to the commit (or not, depending on what you want to test)
use_news_md(open = FALSE)

pr_finish()

# restore initial project, working directory, delete local repo
withr::deferred_run()

## delete local and remote repo
(gh_account <- gh::gh_whoami())
pkgname
gh::gh(
  "DELETE /repos/:username/:pkg",
  username = gh_account$login,
  pkg = pkgname
)


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/manual/manual-upstream-only.R ---
pkgload::unload("devtools")
devtools::load_all("~/rrr/usethis")
attachNamespace("devtools")
library(testthat)

x <- create_from_github(
  "r-lib/gh",
  destdir = "~/tmp",
  fork = FALSE,
  open = FALSE,
  protocol = "https"
)
local_project(x)

r <- git_remotes()
expect_equal(r, list(origin = "https://github.com/r-lib/gh.git"))

use_git_remote("upstream", r$origin)
use_git_remote("origin", url = NULL, overwrite = TRUE)

r <- git_remotes()
expect_equal(r, list(upstream = "https://github.com/r-lib/gh.git"))

check_pr_readiness()
err <- rlang::last_error()

expect_s3_class(err, class = "usethis_error_bad_github_config")

# capture github remote data to use in an actual test
# datapasta::dpasta(github_remotes())

withr::deferred_run()
fs::dir_delete(x)


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/manual/manual-use-course.R ---
devtools::load_all()
library(fs)
library(testthat)

options(usethis.destdir = NULL)

cp <- function(x = "") path(usethis:::conspicuous_place(), x)
cleanup <- function(regexp) {
  outdir <- dir_ls(cp(), regexp = regexp, type = "directory")
  dir_delete(outdir)
  outfile <- dir_ls(cp(), regexp = regexp, type = "file")
  file_delete(outfile)
  invisible()
}

## use_course() simple usage ----
# Should see:
# 1. Menu confirming download to conspicuous place
# 2. Menu approving deletion of ZIP file
use_course("r-lib/rematch2")

cleanup("rematch2-")

## use_course() overwriting existing file ----
use_zip("r-lib/rematch2", destdir = cp(), cleanup = FALSE)
use_course("r-lib/rematch2", destdir = cp())
# Should see:
# Query whether to overwrite pre-existing file
# "No" aborts
# "Yes" proceeds
cleanup("rematch2-")

# download of a DropBox folder
# usethis-manual-test folder JB created for development
dropbox <- "https://www.dropbox.com/sh/iep7x58py4vpa9n/AAAju4kvYCjjD6s8WJqyICHBa?dl=1"
use_zip(dropbox, destdir = cp())
expect_true(dir_exists(cp("usethis-manual-test")))
cleanup("usethis-manual-test")

## the ZIP URL favored by devtools
gh_url <- "http://github.com/r-lib/rematch2/zipball/master/"
folder <- use_zip(gh_url, destdir = cp(), cleanup = FALSE)
(zipfile <- dir_ls(cp(), regexp = "r-lib-rematch2-.*[.]zip"))
expect_length(zipfile, 1)
cleanup("r-lib-rematch2-")


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/manual/manual-use-github-enterprise.R ---
devtools::load_all("~/rrr/usethis")

ghe_host <- "https://github.ubc.ca"
whoami <- gh::gh_whoami(.api_url = ghe_host)
(user <- whoami$login)
pkgname <- "lazy-marmot"
git_protocol()

(pkgpath <- path_temp(pkgname))
create_local_package(pkgpath)
proj_sitrep()

# say YES to the commit
use_git()

use_github(host = ghe_host)

# delete the GitHub repo
gh::gh(
  "DELETE /repos/{username}/{pkg}",
  username = user,
  pkg = pkgname,
  .api_url = ghe_host
)

# restore initial project, working directory, delete local repo
withr::deferred_run()

# let's do it again by setting an env var
Sys.getenv("GITHUB_API_URL")
withr::local_envvar(GITHUB_API_URL = ghe_host)

create_local_package(pkgpath)
proj_sitrep()

# say YES to the commit
use_git()

use_github()

# delete the GitHub repo
gh::gh(
  "DELETE /repos/{username}/{pkg}",
  username = user,
  pkg = pkgname,
  .api_url = ghe_host
)

# restore initial project, working directory, delete local repo
withr::deferred_run()
proj_sitrep()
Sys.getenv("GITHUB_API_URL")


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/manual/manual-use-github-pages.R ---
devtools::load_all("~/rrr/usethis")
library(testthat)
library(fs)

# comment / uncomment to test against GHE
# Sys.setenv(GITHUB_API_URL = "https://github.ubc.ca")
# lesson learned: UBC is still running GHE 2.21 and the syntax around
# source branch and path has changed in GHE 2.22, which seems to match
# github.com
# some of this stuff works, but not all
# not going to worry about supporting older versions of GHE fully

repo_name <- "stacey"
gh_account <- gh::gh_whoami()
(me <- gh_account$login)

# remove any pre-existing repo and local project
gh::gh(
  "DELETE /repos/{username}/{pkg}",
  username = me,
  pkg = repo_name
)
dir_delete(path_home("tmp", repo_name))
expect_false(dir_exists(path_home("tmp", repo_name)))

# create the package
create_local_package(path_home("tmp", "stacey"))
use_git()
use_github()

# should fail because this branch does not exist
use_github_pages(branch = "nope")

# should work
use_github_pages()

# change branch and path
use_github_pages(branch = git_default_branch(), path = "/docs")

# go back to default branch and path
use_github_pages()

# customize domain name
use_github_pages(cname = "example.org")

# clear custom domain name, change path
use_github_pages(path = "/docs", cname = NULL)

# clean up
gh::gh(
  "DELETE /repos/{username}/{pkg}",
  username = me,
  pkg = repo_name
)
withr::deferred_run()
expect_false(dir_exists(path_home("tmp", repo_name)))


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/manual/manual-use-github.R ---
devtools::load_all("~/rrr/usethis")

pkgname <- "taciturn-tern"
#use_git_protocol("ssh")
#use_git_protocol("https")
git_protocol()

(pkgpath <- path_temp(pkgname))
create_local_package(pkgpath)
proj_sitrep()

# should fail, not a git repo yet
use_github(private = TRUE)

# say YES to the commit
use_git()

# set 'origin'
use_git_remote("origin", "fake-origin-url")

# should fail early because 'origin' is already configured
use_github(private = TRUE)

# remove the 'origin' remote
use_git_remote("origin", NULL, overwrite = TRUE)

# should work
use_github(private = TRUE)

# make sure this reflects ssh vs. https, as appropriate
git_remotes()

# remove the 'origin' remote
use_git_remote("origin", NULL, overwrite = TRUE)

# should fail because GitHub repo already exists
use_github(private = TRUE)

# delete the GitHub repo
whoami <- gh::gh_whoami()
gh::gh(
  "DELETE /repos/{username}/{pkg}",
  username = whoami$login,
  pkg = pkgname
)

# this should work!
use_github(private = TRUE)

# 'master' should have 'origin/master' as upstream
info <- gert::git_info()
expect_equal(info$upstream, "origin/master")

# URL and BugReports should be populated
URL <- paste0("https://github.com/", whoami$login, "/", pkgname)
BugReports <- paste0(URL, "/", "issues")
expect_match(desc::desc_get_urls(), URL)
expect_match(desc::desc_get_field("BugReports"), BugReports)

# remove the GitHub links
desc::desc_del(c("BugReports", "URL"))
expect_true(!any(desc::desc_has_fields(c("BugReports", "URL"))))

# restore the GitHub links
# should see a warning that `host` is deprecated and ignored
use_github_links(host = "blah")
expect_match(desc::desc_get_urls(), URL)
expect_match(desc::desc_get_field("BugReports"), BugReports)

# overwrite the GitHub links
desc::desc_set_urls("http://example.org")
desc::desc_set(BugReports = "http://example.org")
use_github_links(overwrite = TRUE)
expect_match(desc::desc_get_urls(), URL)
expect_match(desc::desc_get_field("BugReports"), BugReports)


gh::gh(
  "DELETE /repos/{username}/{pkg}",
  username = whoami$login,
  pkg = pkgname
)
usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)

# only do this if you're willing to restore your PAT
gitcreds::gitcreds_delete()
# should error, because no PAT
use_github(private = TRUE)
# don't forget to restore your PAT
gitcreds::gitcreds_set()

# restore initial project, working directory, delete local repo
withr::deferred_run()

## delete local and remote repo
gh::gh(
  "DELETE /repos/:username/:pkg",
  username = whoami$login,
  pkg = pkgname
)


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/manual/manual-use-tidy-pkgdown.R ---
devtools::load_all("~/rrr/usethis")
library(testthat)
library(fs)

repo_name <- "stacey"
gh_account <- gh::gh_whoami()
(me <- gh_account$login)

# remove any pre-existing repo and local project
gh::gh(
  "DELETE /repos/{username}/{pkg}",
  username = me,
  pkg = repo_name
)
dir_delete(path_home("tmp", repo_name))
expect_false(dir_exists(path_home("tmp", repo_name)))

# create the package
create_local_package(path_home("tmp", "stacey"))
use_git()
use_github()

use_tidy_pkgdown()

# clean up
gh::gh(
  "DELETE /repos/{username}/{pkg}",
  username = me,
  pkg = repo_name
)
withr::deferred_run()
expect_false(dir_exists(path_home("tmp", repo_name)))


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/manual/manual-use-tidy-thanks.R ---
## this is annoyingly slow to have in the automated tests
## not to mention, a bit fragile

library(testthat)

## test use_tidy_thanks() on a repo with contributors and releases
thanks <- use_tidy_thanks(
  "r-lib/usethis",
  from = "2017-12-01",
  to = "v1.2.0"
)
expect_type(thanks, "character")
expect_true(all(c("jennybc", "hadley", "batpigandme") %in% thanks))


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/spelling.R ---
if (requireNamespace("spelling", quietly = TRUE)) {
  spelling::spell_check_test(
    vignettes = TRUE,
    error = FALSE,
    skip_on_cran = TRUE
  )
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat.R ---
library(testthat)
library(usethis)

test_check("usethis")


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/helper-mocks.R ---
local_cran_version <- function(version, .env = caller_env()) {
  local_mocked_bindings(cran_version = function() version, .env = .env)
}

local_check_installed <- function(.env = caller_env()) {
  local_mocked_bindings(check_installed = function(...) NULL, .env = .env)
}

local_rstudio_available <- function(val, .env = caller_env()) {
  local_mocked_bindings(rstudio_available = function(...) val, .env = .env)
}

local_target_repo_spec <- function(spec, .env = caller_env()) {
  local_mocked_bindings(target_repo_spec = function(...) spec, .env = .env)
}

local_roxygen_update_ns <- function(.env = caller_env()) {
  local_mocked_bindings(roxygen_update_ns = function(...) NULL, .env = .env)
}

local_check_fun_exists <- function(.env = caller_env()) {
  local_mocked_bindings(check_fun_exists = function(...) NULL, .env = .env)
}

local_ui_yep <- function(.env = caller_env()) {
  local_mocked_bindings(ui_yep = function(...) TRUE, .env = .env)
}

local_git_default_branch_remote <- function(.env = caller_env()) {
  local_mocked_bindings(
    git_default_branch_remote = function(cfg, remote) {
      list(
        name = remote,
        is_configured = TRUE,
        url = NA_character_,
        repo_spec = NA_character_,
        default_branch = as.character(glue("default-branch-of-{remote}"))
      )
    },
    .env = .env
  )
}


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/helper.R ---
## If session temp directory appears to be, or be within, a project, there
## will be large scale, spurious test failures.
## The IDE sometimes leaves .Rproj files behind in session temp directory or
## one of its parents.
## Delete such files manually.
session_temp_proj <- proj_find(path_temp())
if (!is.null(session_temp_proj)) {
  Rproj_files <- fs::dir_ls(session_temp_proj, glob = "*.Rproj")
  ui_bullets(c(
    "x" = "Rproj {cli::qty(length(Rproj_files))} file{?s} found at or above session temp dir:",
    bulletize(usethis_map_cli(Rproj_files)),
    "!" = "Expect this to cause spurious test failures."
  ))
}

create_local_package <- function(
  dir = file_temp(pattern = "testpkg"),
  env = parent.frame(),
  rstudio = FALSE
) {
  create_local_thing(dir, env, rstudio, "package")
}

create_local_project <- function(
  # it is convenient if `dir` produces a project name that would be allowed for
  # a CRAN package, even for a generic project test fixture
  dir = file_temp(pattern = "testproj"),
  env = parent.frame(),
  rstudio = FALSE
) {
  create_local_thing(dir, env, rstudio, "project")
}

create_local_quarto_project <- function(
  dir = file_temp(pattern = "test-quarto-proj"),
  env = parent.frame(),
  rstudio = FALSE
) {
  create_local_thing(dir, env, rstudio, "quarto_project")
}

create_local_thing <- function(
  dir = file_temp(pattern = pattern),
  env = parent.frame(),
  rstudio = FALSE,
  thing = c("package", "project", "quarto_project")
) {
  thing <- match.arg(thing)
  if (fs::dir_exists(dir)) {
    ui_abort("Target {.arg dir} {.path {pth(dir)}} already exists.")
  }

  old_project <- proj_get_() # this could be `NULL`, i.e. no active project
  old_wd <- getwd() # not necessarily same as `old_project`

  withr::defer(
    {
      ui_bullets(c("v" = "Deleting temporary project: {.path {dir}}"))
      fs::dir_delete(dir)
    },
    envir = env
  )
  ui_silence(
    switch(
      thing,
      package = create_package(
        dir,
        # This is for the sake of interactive development of snapshot tests.
        # When the active usethis project is a package created with this
        # function, testthat learns its edition from *that* package, not from
        # usethis. So, by default, opt in to testthat 3e in these ephemeral test
        # packages.
        fields = list("Config/testthat/edition" = "3"),
        rstudio = rstudio,
        open = FALSE,
        check_name = FALSE
      ),
      project = create_project(dir, rstudio = rstudio, open = FALSE),
      quarto_project = create_quarto_project(
        dir,
        rstudio = rstudio,
        open = FALSE
      )
    )
  )

  withr::defer(proj_set(old_project, force = TRUE), envir = env)
  proj_set(dir)

  withr::defer(
    {
      ui_bullets(c(
        "v" = "Restoring original working directory: {.path {old_wd}}"
      ))
      setwd(old_wd)
    },
    envir = env
  )
  setwd(proj_get())

  invisible(proj_get())
}

scrub_testpkg <- function(message) {
  gsub("testpkg[a-zA-Z0-9]+", "{TESTPKG}", message, perl = TRUE)
}

scrub_testproj <- function(message) {
  gsub("testproj[a-zA-Z0-9]+", "{TESTPROJ}", message, perl = TRUE)
}

skip_if_not_ci <- function() {
  ci_providers <- c("GITHUB_ACTIONS", "TRAVIS", "APPVEYOR")
  ci <- any(toupper(Sys.getenv(ci_providers)) == "TRUE")
  if (ci) {
    return(invisible(TRUE))
  }
  skip("Not on GitHub Actions, Travis, or Appveyor")
}

skip_if_no_git_user <- function() {
  user_name <- git_cfg_get("user.name")
  user_email <- git_cfg_get("user.email")
  user_name_exists <- !is.null(user_name)
  user_email_exists <- !is.null(user_email)
  if (user_name_exists && user_email_exists) {
    return(invisible(TRUE))
  }
  skip("No Git user configured")
}

# CRAN's mac builder sets $HOME to a read-only ram disk, so tests can fail if
# you even tickle something that might try to lock its own config file during
# the operation (e.g. git) or if you simply test for writeability
skip_on_cran_macos <- function() {
  sysname <- tolower(Sys.info()[["sysname"]])
  on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
  if (on_cran && sysname == "darwin") {
    skip("On CRAN and on macOS")
  }
  invisible(TRUE)
}

expect_usethis_error <- function(...) {
  expect_error(..., class = "usethis_error")
}

is_build_ignored <- function(pattern, ..., base_path = proj_get()) {
  lines <- read_utf8(path(base_path, ".Rbuildignore"))
  length(grep(pattern, x = lines, fixed = TRUE, ...)) > 0
}

test_file <- function(fname) testthat::test_path("ref", fname)

expect_proj_file <- function(...) expect_true(file_exists(proj_path(...)))
expect_proj_dir <- function(...) expect_true(dir_exists(proj_path(...)))


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/setup.R ---
withr::local_options(usethis.quiet = TRUE, .local_envir = teardown_env())


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-addin.R ---
test_that("use_addin() creates the first addins.dcf as promised", {
  create_local_package()
  use_addin("addin.test")

  addin_dcf <- read_utf8(proj_path("inst", "rstudio", "addins.dcf"))
  expected_file <- path_package("usethis", "templates", "addins.dcf")
  addin_dcf_expected <- read_utf8(expected_file)
  addin_dcf_expected[3] <- "Binding: addin.test"
  addin_dcf_expected[5] <- ""
  expect_equal(addin_dcf, addin_dcf_expected)
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-air.R ---
test_that("creates correct default package files", {
  create_local_package()

  withr::local_options(usethis.quiet = FALSE)
  expect_snapshot(use_air())

  # Empty, but should exist
  expect_proj_file("air.toml")

  ignore <- read_utf8(proj_path(".Rbuildignore"))
  expect_in(air_toml_regex(), ignore)
  expect_in(escape_path(".vscode"), ignore)

  settings <- jsonlite::read_json(proj_path(".vscode", "settings.json"))
  expect_true(settings[["[r]"]][["editor.formatOnSave"]])
  expect_identical(
    settings[["[r]"]][["editor.defaultFormatter"]],
    "Posit.air-vscode"
  )
  expect_true(settings[["[quarto]"]][["editor.formatOnSave"]])
  expect_identical(
    settings[["[quarto]"]][["editor.defaultFormatter"]],
    "quarto.quarto"
  )

  settings <- jsonlite::read_json(proj_path(".vscode", "extensions.json"))
  recommendations <- settings[["recommendations"]]
  expect_identical(recommendations, list("Posit.air-vscode"))

  # Snapshot exact details to look at indent level and prettyfication
  expect_snapshot(
    writeLines(read_utf8(proj_path(".vscode", "settings.json")))
  )
  expect_snapshot(
    writeLines(read_utf8(proj_path(".vscode", "extensions.json")))
  )
})

test_that("creates correct default project files", {
  create_local_project()

  use_air()

  # Empty, but should exist
  expect_proj_file("air.toml")

  # Does not add to `.Rbuildignore` in projects
  expect_false(file_exists(proj_path(".Rbuildignore")))

  settings <- jsonlite::read_json(proj_path(".vscode", "settings.json"))
  expect_true(settings[["[r]"]][["editor.formatOnSave"]])
  expect_identical(
    settings[["[r]"]][["editor.defaultFormatter"]],
    "Posit.air-vscode"
  )
  expect_true(settings[["[quarto]"]][["editor.formatOnSave"]])
  expect_identical(
    settings[["[quarto]"]][["editor.defaultFormatter"]],
    "quarto.quarto"
  )

  settings <- jsonlite::read_json(proj_path(".vscode", "extensions.json"))
  recommendations <- settings[["recommendations"]]
  expect_identical(recommendations, list("Posit.air-vscode"))
})

test_that("respects existing `settings.json`, but overwrites settings we own", {
  create_local_project()

  dir_create(proj_path(".vscode"))
  path <- file_create(proj_path(".vscode", "settings.json"))

  settings <- list(
    "setting" = list(1L, 2L),
    "[r]" = list(
      "editor.formatOnSave" = FALSE,
      "editor.defaultFormatter" = "not-air"
    ),
    "[rust]" = list(
      "editor.formatOnSave" = FALSE
    ),
    "[quarto]" = list(
      "editor.wordWrap" = "wordWrapColumn"
    )
  )

  jsonlite::write_json(settings, path, auto_unbox = TRUE)

  use_air()

  # Here is all that should change
  settings[["[r]"]][["editor.formatOnSave"]] <- TRUE
  settings[["[r]"]][["editor.defaultFormatter"]] <- "Posit.air-vscode"
  settings[["[quarto]"]][["editor.formatOnSave"]] <- TRUE
  settings[["[quarto]"]][["editor.defaultFormatter"]] <- "quarto.quarto"

  actual_settings <- jsonlite::read_json(path)

  expect_identical(actual_settings, settings)
})

test_that("respects existing `extensions.json`", {
  create_local_project()

  dir_create(proj_path(".vscode"))
  path <- file_create(proj_path(".vscode", "extensions.json"))

  settings <- list(
    "recommendations" = list("this", "that")
  )

  jsonlite::write_json(settings, path, auto_unbox = TRUE)

  use_air()

  settings <- list(
    "recommendations" = list("this", "that", "Posit.air-vscode")
  )

  actual_settings <- jsonlite::read_json(path)

  expect_identical(actual_settings, settings)
})

test_that("does not add to `extensions.json` if already there", {
  create_local_project()

  dir_create(proj_path(".vscode"))
  path <- file_create(proj_path(".vscode", "extensions.json"))

  settings <- list(
    "recommendations" = list("this", "Posit.air-vscode", "that")
  )

  jsonlite::write_json(settings, path, auto_unbox = TRUE)

  use_air()

  actual_settings <- jsonlite::read_json(path)

  expect_identical(actual_settings, settings)
})

test_that("respects existing `.air.toml`", {
  create_local_project()

  content <- c("[format]", "line-width = 90")

  write_utf8(proj_path(".air.toml"), content)

  use_air()

  # Does not make un-dotted form
  expect_false(file_exists(proj_path("air.toml")))

  expect_identical(read_utf8(proj_path(".air.toml")), content)
})

test_that("respects `vscode` option", {
  create_local_package()
  use_air(vscode = FALSE)
  expect_false(dir_exists(proj_path(".vscode")))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-author.R ---
test_that("Can add an author and then another", {
  withr::local_options(usethis.description = NULL)
  create_local_package()

  local_interactive(FALSE)
  use_author(
    "Jennifer",
    "Bryan",
    email = "jenny@posit.co",
    comment = c(ORCID = "0000-0002-6983-2759")
  )

  d <- proj_desc()
  ctb <- d$get_author(role = "ctb")
  expect_equal(ctb$given, "Jennifer")
  expect_equal(ctb$family, "Bryan")
  expect_equal(ctb$email, "jenny@posit.co")
  expect_equal(ctb$comment, c(ORCID = "0000-0002-6983-2759"))

  use_author(
    "Hadley",
    "Wickham",
    email = "hadley@posit.co",
    role = c("rev", "fnd")
  )

  d <- proj_desc()
  rev <- d$get_author(role = "rev")
  fnd <- d$get_author(role = "fnd")
  expect_equal(rev$given, "Hadley")
  expect_equal(rev$family, "Wickham")
  expect_equal(fnd$given, "Hadley")
  expect_equal(fnd$family, "Wickham")
})

test_that("Legacy author fields are challenged", {
  withr::local_options(usethis.description = NULL)
  create_local_package()

  d <- proj_desc()
  # I'm sort of deliberately leaving Authors@R there, just to make things
  # even less ideal. But one could do:
  # d$del("Authors@R")

  # used BH as of 2023-04-19 as my example of a package that uses
  # Author and Maintainer and does not use Authors@R
  d$set(Maintainer = "Dirk Eddelbuettel <edd@debian.org>")
  d$set(Author = "Dirk Eddelbuettel, John W. Emerson and Michael J. Kane")
  d$write()

  local_interactive(FALSE)
  withr::local_options(usethis.quiet = FALSE)
  expect_snapshot(challenge_legacy_author_fields(), error = TRUE)
})

test_that("Decline to tweak an existing author", {
  withr::local_options(
    usethis.description = list(
      "Authors@R" = utils::person(
        "Jennifer",
        "Bryan",
        email = "jenny@posit.co",
        role = c("aut", "cre"),
        comment = c(ORCID = "0000-0002-6983-2759")
      )
    )
  )
  create_local_package()

  withr::local_options(usethis.quiet = FALSE)
  expect_snapshot(
    use_author("Jennifer", "Bryan", role = "cph"),
    error = TRUE
  )
})

test_that("Placeholder author is challenged", {
  withr::local_options(usethis.description = NULL)
  create_local_package()

  local_interactive(FALSE)
  withr::local_options(usethis.quiet = FALSE)
  expect_snapshot(use_author("Charlie", "Brown"))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-badge.R ---
test_that("use_[cran|bioc]_badge() don't error", {
  create_local_package()
  expect_no_error(use_cran_badge())
  expect_no_error(use_bioc_badge())
})

test_that("use_lifecycle_badge() handles bad and good input", {
  create_local_package()

  expect_snapshot(error = TRUE, {
    use_lifecycle_badge("eperimental")
  })

  expect_no_error(use_lifecycle_badge("stable"))
})

test_that("use_binder_badge() needs a github repository", {
  skip_if_no_git_user()
  create_local_project()
  use_git()
  expect_error(
    use_binder_badge(),
    class = "usethis_error_bad_github_remote_config"
  )
})

test_that("use_r_universe_badge() needs to know the owner", {
  skip_if_no_git_user()
  local_interactive(FALSE)
  withr::local_options(usethis.quiet = FALSE)
  create_local_package()

  expect_snapshot(
    error = TRUE,
    use_r_universe_badge(),
    transform = scrub_testpkg
  )

  expect_snapshot(
    use_r_universe_badge("OWNER_DIRECT/SCRUBBED"),
    transform = scrub_testpkg
  )

  desc::desc_set_urls("https://github.com/OWNER_DESCRIPTION/SCRUBBED")
  expect_snapshot(
    use_r_universe_badge(),
    transform = scrub_testpkg
  )

  use_git()
  use_git_remote("origin", "https://github.com/OWNER_ORIGIN/SCRUBBED.git")
  expect_snapshot(
    use_r_universe_badge(),
    transform = scrub_testpkg
  )
})

test_that("use_posit_cloud_badge() handles bad and good input", {
  create_local_project()
  expect_snapshot(use_posit_cloud_badge(), error = TRUE)
  expect_snapshot(use_posit_cloud_badge(123), error = TRUE)
  expect_snapshot(use_posit_cloud_badge("http://posit.cloud/123"), error = TRUE)
  expect_no_error(use_posit_cloud_badge("https://posit.cloud/content/123"))
  expect_no_error(use_posit_cloud_badge(
    "https://posit.cloud/spaces/123/content/123"
  ))
})

test_that("use_badge() does nothing if badge seems to pre-exist", {
  create_local_package()
  href <- "https://cran.r-project.org/package=foo"
  writeLines(href, proj_path("README.md"))
  expect_false(use_badge("foo", href, "SRC"))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-block.R ---
test_that("block_append() only writes unique lines", {
  path <- withr::local_tempfile()
  writeLines(block_create(), path)

  block_append("---", c("x", "y"), path)
  block_append("---", c("y", "x"), path)
  expect_equal(block_show(path), c("x", "y"))
})

test_that("block_append() can sort, if requested", {
  path <- withr::local_tempfile()
  writeLines(block_create(), path)

  block_append("---", c("z", "y"), path)
  block_append("---", "x", path, sort = TRUE)
  expect_equal(block_show(path), c("x", "y", "z"))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-browse.R ---
test_that("github_url() errors if no project", {
  withr::local_dir(path_temp())
  local_project(NULL, force = TRUE, setwd = TRUE)
  expect_usethis_error(github_url(), "not.*inside a valid project")
})

test_that("github_url() works on active project", {
  create_local_project()
  local_interactive(FALSE)
  use_git()

  expect_usethis_error(github_url(), "no DESCRIPTION")
  expect_usethis_error(github_url(), "no GitHub remotes")

  use_description()
  proj_desc_field_update("URL", "https://example.com")
  expect_usethis_error(github_url(), "no GitHub remotes")

  issues <- "https://github.com/OWNER/REPO_BUGREPORTS/issues"
  proj_desc_field_update("BugReports", issues)
  expect_equal(github_url(), "https://github.com/OWNER/REPO_BUGREPORTS")

  origin <- "https://github.com/OWNER/REPO_ORIGIN"
  use_git_remote("origin", origin)

  expect_equal(github_url(), "https://github.com/OWNER/REPO_ORIGIN")
})

test_that("github_url() strips everything after USER/REPO", {
  expect_equal(github_url("usethis"), "https://github.com/r-lib/usethis")
  expect_equal(github_url("gh"), "https://github.com/r-lib/gh")
})

test_that("github_url() has fall back for CRAN packages", {
  expect_warning(out <- github_url("utils"), "CRAN mirror")
  expect_equal(out, "https://github.com/cran/utils")
})

test_that("github_url() errors for nonexistent package", {
  expect_usethis_error(github_url("1234"), "Can't find")
})

test_that("cran_home() produces canonical URL", {
  pkg <- create_local_package(file_temp("abc"))
  expect_match(cran_home(), "https://cran.r-project.org/package=abc")
  expect_match(cran_home("bar"), "https://cran.r-project.org/package=bar")
})

test_that("desc_urls() returns NULL if no project", {
  withr::local_dir(path_temp())
  local_project(NULL, force = TRUE, setwd = TRUE)
  expect_null(desc_urls())
})

test_that("desc_urls() returns NULL if no DESCRIPTION", {
  create_local_project()
  expect_null(desc_urls())
})

test_that("desc_urls() returns empty data frame if no URLs", {
  create_local_project()
  use_description()
  expect_equal(
    desc_urls(),
    data.frame(
      url = character(),
      desc_field = character(),
      is_github = logical(),
      stringsAsFactors = FALSE
    )
  )
})

test_that("desc_urls() returns data frame for locally installed package", {
  out <- desc_urls("curl")
  expect_true(nrow(out) > 1)
})

test_that("desc_urls() returns data frame for an uninstalled package", {
  skip_if_offline()

  pkg <- "devoid"
  if (requireNamespace(pkg, quietly = TRUE)) {
    skip(paste0(pkg, " is installed locally"))
  }

  out <- desc_urls(pkg)
  expect_true(nrow(out) > 1)
})

test_that("desc_urls() returns NULL for an nonexistent package", {
  skip_if_offline()

  expect_null(desc_urls("1234"))
})

test_that("browse_XXX() goes to correct URL", {
  local_interactive(FALSE)
  g <- function(x) paste0("https://github.com/", x)

  expect_equal(browse_github("gh"), g("r-lib/gh"))

  expect_match(browse_github_issues("gh"), g("r-lib/gh/issues"))
  expect_equal(browse_github_issues("gh", 1), g("r-lib/gh/issues/1"))
  expect_equal(browse_github_issues("gh", "new"), g("r-lib/gh/issues/new"))

  expect_match(browse_github_pulls("gh"), g("r-lib/gh/pulls"))
  expect_equal(browse_github_pulls("gh", 1), g("r-lib/gh/pull/1"))

  expect_match(browse_github_actions("gh"), g("r-lib/gh/actions"))

  expect_equal(
    browse_cran("usethis"),
    "https://cran.r-project.org/package=usethis"
  )
})

test_that("browse_package() errors if no project", {
  withr::local_dir(path_temp())
  local_project(NULL, force = TRUE, setwd = TRUE)
  expect_usethis_error(browse_project(), "not.*inside a valid project")
})

test_that("browse_package() returns URLs", {
  create_local_project()
  use_git()

  expect_equal(browse_package(), character())

  origin <- "https://github.com/OWNER/REPO"
  use_git_remote("origin", origin)
  foofy <- "https://github.com/SOMEONE_ELSE/REPO"
  use_git_remote("foofy", foofy)

  use_description()
  pkgdown <- "https://example.com"
  proj_desc_field_update("URL", pkgdown)
  issues <- "https://github.com/OWNER/REPO/issues"
  proj_desc_field_update("BugReports", issues)

  out <- browse_package()
  expect_setequal(out, c(origin, foofy, pkgdown, issues))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-ci.R ---
test_that("use_circleci() configures CircleCI", {
  skip_if_no_git_user()

  local_interactive(FALSE)
  create_local_package()
  use_git()

  local_target_repo_spec("OWNER/REPO")

  use_circleci(browse = FALSE)

  expect_true(is_build_ignored("^\\.circleci$"))

  expect_proj_dir(".circleci")
  expect_proj_file(".circleci/config.yml")
  yml <- yaml::yaml.load_file(proj_path(".circleci", "config.yml"))
  expect_identical(
    yml$jobs$build$steps[[7]]$store_artifacts$path,
    paste0(project_name(), ".Rcheck/")
  )

  # use_circleci() properly formats keys for cache
  expect_identical(
    yml$jobs$build$steps[[1]]$restore_cache$keys,
    c("r-pkg-cache-{{ arch }}-{{ .Branch }}", "r-pkg-cache-{{ arch }}-")
  )
  expect_identical(
    yml$jobs$build$steps[[8]]$save_cache$key,
    "r-pkg-cache-{{ arch }}-{{ .Branch }}"
  )

  dir_delete(proj_path(".circleci"))
  docker <- "rocker/r-ver:3.5.3"

  use_circleci(browse = FALSE, image = docker)

  yml <- yaml::yaml.load_file(proj_path(".circleci", "config.yml"))
  expect_identical(yml$jobs$build$docker[[1]]$image, docker)
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-citation.R ---
test_that("use_citation() creates promised file", {
  create_local_package()
  use_citation()
  expect_proj_file("inst", "CITATION")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-code-of-conduct.R ---
test_that("use_code_of_conduct() creates promised file", {
  create_local_project()
  use_code_of_conduct("test@example.com")
  expect_proj_file("CODE_OF_CONDUCT.md")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-course.R ---
## download_url ----

test_that("download_url() retry logic works as advertised", {
  faux_download <- function(n_failures) {
    i <- 0
    function(url, destfile, quiet, mode, handle) {
      i <<- i + 1
      if (i <= n_failures) simpleError(paste0("try ", i)) else "success"
    }
  }
  withr::local_options(list(usethis.quiet = FALSE))

  # succeed on first try
  local_mocked_bindings(
    try_download = faux_download(0)
  )
  expect_snapshot(out <- download_url(url = "URL", destfile = "destfile"))
  expect_s3_class(out, "curl_handle")

  # fail, then succeed
  local_mocked_bindings(
    try_download = faux_download(1)
  )
  expect_snapshot(out <- download_url(url = "URL", destfile = "destfile"))
  expect_s3_class(out, "curl_handle")

  # fail, fail, then succeed (default n_tries = 3, so should allow)
  local_mocked_bindings(
    try_download = faux_download(2)
  )
  expect_snapshot(out <- download_url(url = "URL", destfile = "destfile"))
  expect_s3_class(out, "curl_handle")

  # fail, fail, fail (exceed n_failures > n_tries = 3)
  local_mocked_bindings(
    try_download = faux_download(5)
  )
  expect_snapshot(
    out <- download_url(url = "URL", destfile = "destfile", n_tries = 3),
    error = TRUE
  )

  # fail, fail, fail, succeed (make sure n_tries is adjustable)
  local_mocked_bindings(
    try_download = faux_download(3)
  )
  expect_snapshot(
    out <- download_url(url = "URL", destfile = "destfile", n_tries = 10)
  )
  expect_s3_class(out, "curl_handle")
})

## tidy_download ----

test_that("tidy_download() errors early if destdir is not a directory", {
  tmp <- fs::path_temp("I_am_just_a_file")
  withr::defer(fs::file_delete(tmp))

  expect_usethis_error(tidy_download("URL", destdir = tmp), "does not exist")

  fs::file_create(tmp)
  expect_usethis_error(tidy_download("URL", destdir = tmp), "not a directory")
})

test_that("tidy_download() works", {
  skip_if_offline("github.com")
  local_interactive(FALSE)

  tmp <- withr::local_tempdir(pattern = "tidy-download-test-")

  gh_url <- "https://github.com/r-lib/rematch2/archive/main.zip"
  expected <- fs::path(tmp, "rematch2-main.zip")

  capture.output(
    out <- tidy_download(gh_url, destdir = tmp)
  )
  expect_true(fs::file_exists(expected))
  expect_identical(out, expected, ignore_attr = TRUE)
  expect_identical(attr(out, "content-type"), "application/zip")

  # refuse to overwrite when non-interactive
  # snapshot impractical due to
  # (1) output beyond usethis's control re: download progress
  # (2) a temp file path that would need to be scrubbed (possible but won't
  #     bother due to (1))
  expect_usethis_error(tidy_download(gh_url, destdir = tmp))
})

## tidy_unzip ----

test_that("tidy_unzip(): explicit parent, file example", {
  local_interactive(FALSE)

  zipfile <- withr::local_tempfile(fileext = ".zip")
  file_copy(test_file("foo-explicit-parent.zip"), zipfile)
  dest <- tidy_unzip(zipfile)
  withr::defer(dir_delete(dest))
  expect_equal(path_file(dest), "foo")

  explicit_parent_files <- path_file(dir_ls(dest, recurse = TRUE))
  expect_equal(explicit_parent_files, "file.txt")
})

test_that("tidy_unzip(): explicit parent, folders example", {
  local_interactive(FALSE)
  files <- c("subdir1", "file1.txt", "subdir2", "file2.txt")

  zipfile <- withr::local_tempfile(fileext = ".zip")
  file_copy(test_file("yo-explicit-parent.zip"), zipfile)
  dest <- tidy_unzip(zipfile)
  withr::defer(dir_delete(dest))
  expect_equal(path_file(dest), "yo")

  explicit_parent_files <- path_file(dir_ls(dest, recurse = TRUE))
  expect_setequal(explicit_parent_files, files)
})

test_that("tidy_unzip(): implicit parent, file example", {
  local_interactive(FALSE)

  zipfile <- withr::local_tempfile(fileext = ".zip")
  file_copy(test_file("foo-implicit-parent.zip"), zipfile)
  dest <- tidy_unzip(zipfile)
  withr::defer(dir_delete(dest))
  expect_equal(path_file(dest), "foo")

  implicit_parent_files <- path_file(dir_ls(dest, recurse = TRUE))
  expect_equal(implicit_parent_files, "file.txt")
})

test_that("tidy_unzip(): implicit parent, folders example", {
  local_interactive(FALSE)
  files <- c("subdir1", "file1.txt", "subdir2", "file2.txt")

  zipfile <- withr::local_tempfile(fileext = ".zip")
  file_copy(test_file("yo-implicit-parent.zip"), zipfile)
  dest <- tidy_unzip(zipfile)
  withr::defer(dir_delete(dest))
  expect_equal(path_file(dest), "yo")

  implicit_parent_files <- path_file(dir_ls(dest, recurse = TRUE))
  expect_setequal(implicit_parent_files, files)
})

test_that("tidy_unzip(): no parent, file example", {
  local_interactive(FALSE)

  zipfile <- withr::local_tempfile(fileext = ".zip")
  file_copy(test_file("foo-no-parent.zip"), zipfile)
  dest <- tidy_unzip(zipfile)
  withr::defer(dir_delete(dest))
  expect_equal(path_file(dest), path_ext_remove(path_file(zipfile)))

  no_parent_files <- path_file(dir_ls(dest, recurse = TRUE))
  expect_setequal(no_parent_files, "file.txt")
})

test_that("tidy_unzip(): no parent, folders example", {
  local_interactive(FALSE)
  files <- c("subdir1", "file1.txt", "subdir2", "file2.txt")

  zipfile <- withr::local_tempfile(fileext = ".zip")
  file_copy(test_file("yo-no-parent.zip"), zipfile)
  dest <- tidy_unzip(zipfile)
  withr::defer(dir_delete(dest))
  expect_equal(path_file(dest), path_ext_remove(path_file(zipfile)))

  no_parent_files <- path_file(dir_ls(dest, recurse = TRUE))
  expect_setequal(no_parent_files, files)
})

test_that("tidy_unzip(): DropBox, file example", {
  local_interactive(FALSE)

  zipfile <- withr::local_tempfile(fileext = ".zip")
  file_copy(test_file("foo-loose-dropbox.zip"), zipfile)
  dest <- tidy_unzip(zipfile)
  withr::defer(dir_delete(dest))
  expect_equal(path_file(dest), path_ext_remove(path_file(zipfile)))

  loose_dropbox_files <- path_file(dir_ls(dest, recurse = TRUE))
  expect_setequal(loose_dropbox_files, "file.txt")
})

test_that("tidy_unzip(): DropBox, folders example", {
  local_interactive(FALSE)
  files <- c("subdir1", "file1.txt", "subdir2", "file2.txt")

  zipfile <- withr::local_tempfile(fileext = ".zip")
  file_copy(test_file("yo-loose-dropbox.zip"), zipfile)
  dest <- tidy_unzip(zipfile)
  withr::defer(dir_delete(dest))
  expect_equal(path_file(dest), path_ext_remove(path_file(zipfile)))

  loose_dropbox_files <- path_file(dir_ls(dest, recurse = TRUE))
  expect_setequal(loose_dropbox_files, files)
})

test_that("path_before_slash() works", {
  expect_equal(path_before_slash(""), "")
  expect_equal(path_before_slash("/"), "")
  expect_equal(path_before_slash("a/"), "a")
  expect_equal(path_before_slash("a/b"), "a")
  expect_equal(path_before_slash("a/b/c"), "a")
  expect_equal(path_before_slash("a/b/c/"), "a")
})

## helpers ----
test_that("create_download_url() works", {
  expect_equal(
    create_download_url("https://rstudio.com"),
    "https://rstudio.com"
  )
  expect_equal(
    create_download_url("https://drive.google.com/open?id=123456789xxyyyzzz"),
    "https://drive.google.com/uc?export=download&id=123456789xxyyyzzz"
  )
  expect_equal(
    create_download_url(
      "https://drive.google.com/file/d/123456789xxxyyyzzz/view"
    ),
    "https://drive.google.com/uc?export=download&id=123456789xxxyyyzzz"
  )
  expect_equal(
    create_download_url("https://www.dropbox.com/sh/12345abcde/6789wxyz?dl=0"),
    "https://www.dropbox.com/sh/12345abcde/6789wxyz?dl=1"
  )

  # GitHub
  usethis_url <- "https://github.com/r-lib/usethis/zipball/HEAD"
  expect_equal(
    create_download_url("https://github.com/r-lib/usethis"),
    usethis_url
  )
  expect_equal(
    create_download_url("https://github.com/r-lib/usethis/issues"),
    usethis_url
  )
  expect_equal(
    create_download_url("https://github.com/r-lib/usethis#readme"),
    usethis_url
  )
})

test_that("normalize_url() prepends https:// (or not)", {
  expect_snapshot(normalize_url(1), error = TRUE)
  expect_identical(normalize_url("http://bit.ly/abc"), "http://bit.ly/abc")
  expect_identical(normalize_url("bit.ly/abc"), "https://bit.ly/abc")
  expect_identical(
    normalize_url("https://github.com/r-lib/rematch2/archive/main.zip"),
    "https://github.com/r-lib/rematch2/archive/main.zip"
  )
  expect_identical(
    normalize_url("https://rstd.io/usethis-src"),
    "https://rstd.io/usethis-src"
  )
  expect_identical(
    normalize_url("rstd.io/usethis-src"),
    "https://rstd.io/usethis-src"
  )
})

test_that("shortlinks pass through", {
  url1 <- "bit.ly/usethis-shortlink-example"
  url2 <- "rstd.io/usethis-shortlink-example"
  expect_equal(normalize_url(url1), paste0("https://", url1))
  expect_equal(normalize_url(url2), paste0("https://", url2))
  expect_equal(
    normalize_url(paste0("https://", url1)),
    paste0("https://", url1)
  )
  expect_equal(normalize_url(paste0("http://", url1)), paste0("http://", url1))
})

test_that("github links get expanded", {
  expect_equal(
    normalize_url("OWNER/REPO"),
    "https://github.com/OWNER/REPO/zipball/HEAD"
  )
})

test_that("conspicuous_place() returns a writeable directory", {
  skip_on_cran_macos() # even $HOME is not writeable on CRAN macOS builder
  expect_no_error(x <- conspicuous_place())
  expect_true(is_dir(x))
  expect_true(file_access(x, mode = "write"))
})

test_that("conspicuous_place() uses `usethis.destdir` when set", {
  destdir <- withr::local_tempdir(pattern = "destdir_temp")
  withr::local_options(list(usethis.destdir = destdir))
  expect_no_error(x <- conspicuous_place())
  expect_equal(path_tidy(destdir), x)
})

test_that("use_course() errors if MIME type is not 'application/zip'", {
  skip_if_offline()

  path <- withr::local_tempdir()
  expect_usethis_error(
    use_course("https://example.com", destdir = path),
    "does not have MIME type"
  )
})

test_that("parse_content_disposition() parses Content-Description", {
  ## typical DropBox
  expect_identical(
    parse_content_disposition(
      "attachment; filename=\"foo.zip\"; filename*=UTF-8''foo.zip\""
    ),
    c(
      "filename" = "\"foo.zip\"",
      "filename*" = "UTF-8''foo.zip\""
    )
  )
  ## typical GitHub
  expect_identical(
    parse_content_disposition("attachment; filename=foo-main.zip"),
    c("filename" = "foo-main.zip")
  )
})

test_that("parse_content_disposition() errors on ill-formed `content-disposition` header", {
  expect_usethis_error(
    parse_content_disposition("aa;bb=cc;dd"),
    "doesn't start with"
  )
})

test_that("make_filename() gets name from `content-disposition` header", {
  ## DropBox
  expect_identical(
    make_filename(
      c(
        "filename" = "\"usethis-test.zip\"",
        "filename*" = "UTF-8''usethis-test.zip\""
      )
    ),
    "usethis-test.zip"
  )
  ## GitHub
  expect_identical(
    make_filename(c("filename" = "buzzy-main.zip")),
    "buzzy-main.zip"
  )
})

test_that("make_filename() uses fallback if no `content-disposition` header", {
  expect_match(make_filename(NULL), "^file[0-9a-z]+$")
})

test_that("keep_lgl() keeps and drops correct files", {
  keepers <- c("foo", ".gitignore", "a/.gitignore", "foo.Rproj", ".here")
  expect_true(all(keep_lgl(keepers)))

  droppers <- c(
    ".git",
    "/.git",
    "/.git/",
    ".git/",
    "foo/.git",
    ".git/config",
    ".git/objects/06/3d3gysle",
    ".Rproj.user",
    ".Rproj.user/123jkl/persistent-state",
    ".Rhistory",
    ".RData"
  )
  expect_false(any(keep_lgl(droppers)))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-coverage.R ---
test_that("we use specific URLs in a codecov badge", {
  create_local_package()
  withr::local_options(usethis.quiet = FALSE)
  expect_snapshot(use_codecov_badge("OWNER/REPO"))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-cpp11.R ---
test_that("use_cpp11() requires a package", {
  create_local_project()
  local_check_installed()
  expect_usethis_error(use_cpp11(), "not an R package")
})

test_that("use_cpp11() creates files/dirs, edits DESCRIPTION and .gitignore", {
  create_local_package()
  use_roxygen_md()
  use_package_doc() # needed for use_cpp11()

  local_interactive(FALSE)
  local_check_installed()
  local_mocked_bindings(check_cpp_register_deps = function() invisible())

  use_cpp11()
  expect_match(desc::desc_get("LinkingTo"), "cpp11")
  expect_proj_dir("src")
  expect_proj_file("src", "code.cpp")

  ignores <- read_utf8(proj_path("src", ".gitignore"))
  expect_contains(ignores, c("*.o", "*.so", "*.dll"))
})

test_that("check_cpp_register_deps is silent if all installed, emits todo if not", {
  withr::local_options(list(usethis.quiet = FALSE))
  local_mocked_bindings(
    get_cpp_register_deps = function() c("brio", "decor", "vctrs"),
    is_installed = function(package) TRUE
  )

  expect_no_message(
    check_cpp_register_deps()
  )

  local_mocked_bindings(
    is_installed = function(package) identical(package, "brio")
  )

  expect_snapshot(
    check_cpp_register_deps()
  )
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-cran.R ---
test_that("use_cran_comments() requires a package", {
  create_local_project()
  expect_usethis_error(use_cran_comments(), "not an R package")
})

test_that("use_cran_comments() creates and ignores the promised file", {
  create_local_package()
  use_cran_comments()
  expect_proj_file("cran-comments.md")
  expect_true(is_build_ignored("^cran-comments\\.md$"))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-create.R ---
test_that("create_package() creates a package", {
  dir <- create_local_package()
  expect_true(possibly_in_proj(dir))
  expect_true(is_package(dir))
})

test_that("create_project() creates a non-package project", {
  dir <- create_local_project()
  expect_true(possibly_in_proj(dir))
  expect_false(is_package(dir))
})

test_that("create_*(open = FALSE) returns path to new proj, restores active proj", {
  path <- file_temp()
  cur_proj <- proj_get_()

  out_path <- create_package(path, open = FALSE)
  expect_equal(proj_get_(), cur_proj)
  expect_equal(proj_path_prep(path), out_path)
  dir_delete(out_path)

  out_path <- create_project(path, open = FALSE)
  expect_equal(proj_get_(), cur_proj)
  expect_equal(proj_path_prep(path), out_path)
  dir_delete(out_path)
})

test_that("nested package is disallowed, by default", {
  dir <- create_local_package()
  expect_usethis_error(create_package(path(dir, "abcde")), "anyway")
})

test_that("nested project is disallowed, by default", {
  dir <- create_local_project()
  expect_usethis_error(create_project(path(dir, "abcde")), "anyway")
})

test_that("nested package can be created if user really, really wants to", {
  parent <- create_local_package()
  child_path <- path(parent, "fghijk")

  # since user can't approve interactively, use the backdoor
  withr::local_options("usethis.allow_nested_project" = TRUE)

  child_result <- create_package(child_path)

  expect_equal(child_path, child_result)
  expect_true(possibly_in_proj(child_path))
  expect_true(is_package(child_path))
  expect_equal(project_name(child_path), "fghijk")
})

test_that("nested project can be created if user really, really wants to", {
  parent <- create_local_project()
  child_path <- path(parent, "fghijk")

  # since user can't approve interactively, use the backdoor
  withr::local_options("usethis.allow_nested_project" = TRUE)

  child_result <- create_project(child_path)

  expect_equal(child_path, child_result)
  expect_true(possibly_in_proj(child_path))
  expect_equal(project_name(child_path), "fghijk")
})

test_that("can create package in current directory (literally in '.')", {
  target_path <- dir_create(file_temp("mypackage"))
  withr::defer(dir_delete(target_path))
  withr::local_dir(target_path)
  orig_proj <- proj_get_()
  orig_wd <- path_wd()

  expect_no_error(
    out_path <- create_package(".", open = FALSE)
  )
  expect_equal(path_wd(), orig_wd)
  expect_equal(proj_get_(), orig_proj)
})

## https://github.com/r-lib/usethis/issues/227
test_that("create_* works w/ non-existing rel path, open = FALSE case", {
  sandbox <- path_real(dir_create(file_temp("sandbox")))
  orig_proj <- proj_get_()
  orig_wd <- path_wd()
  withr::defer(dir_delete(sandbox))
  withr::defer(proj_set(orig_proj, force = TRUE))
  withr::local_dir(sandbox)

  rel_path_pkg <- path_file(file_temp(pattern = "abc"))
  expect_no_error(
    out_path <- create_package(rel_path_pkg, open = FALSE)
  )
  expect_true(dir_exists(rel_path_pkg))
  expect_equal(out_path, proj_path_prep(rel_path_pkg))
  expect_equal(proj_get_(), orig_proj)
  expect_equal(path_wd(), sandbox)

  rel_path_proj <- path_file(file_temp(pattern = "def"))
  expect_no_error(
    out_path <- create_project(rel_path_proj, open = FALSE)
  )
  expect_true(dir_exists(rel_path_proj))
  expect_equal(out_path, proj_path_prep(rel_path_proj))
  expect_equal(proj_get_(), orig_proj)
  expect_equal(path_wd(), sandbox)
})

# https://github.com/r-lib/usethis/issues/1122
test_that("create_*() works w/ non-existing rel path, open = TRUE, not in RStudio", {
  sandbox <- path_real(dir_create(file_temp("sandbox")))
  orig_proj <- proj_get_()
  withr::defer(dir_delete(sandbox))
  withr::defer(proj_set(orig_proj, force = TRUE))
  withr::local_dir(sandbox)
  local_rstudio_available(FALSE)

  # package
  rel_path_pkg <- path_file(file_temp(pattern = "ghi"))
  expect_no_error(
    out_path <- create_package(rel_path_pkg, open = TRUE)
  )
  exp_path_pkg <- path(sandbox, rel_path_pkg)
  expect_equal(out_path, exp_path_pkg)
  expect_equal(path_wd(), out_path)
  expect_equal(proj_get(), out_path)

  setwd(sandbox)

  # project
  rel_path_proj <- path_file(file_temp(pattern = "jkl"))
  expect_no_error(
    out_path <- create_project(rel_path_proj, open = TRUE)
  )
  exp_path_proj <- path(sandbox, rel_path_proj)
  expect_equal(out_path, exp_path_proj)
  expect_equal(path_wd(), out_path)
  expect_equal(proj_get(), out_path)
})

test_that("we discourage project creation in home directory", {
  local_interactive(FALSE)
  expect_usethis_error(create_package(path_home()), "create anyway")
  expect_usethis_error(create_project(path_home()), "create anyway")

  if (is_windows()) {
    expect_usethis_error(create_package(path_home_r()), "create anyway")
    expect_usethis_error(create_project(path_home_r()), "create anyway")
  }
})

test_that("create_quarto_project() works for basic usage", {
  skip_if_not_installed("quarto")
  skip_if_not(quarto::quarto_available(error = FALSE))

  dir <- create_local_quarto_project()
  expect_proj_file("_quarto.yml")
  expect_true(possibly_in_proj(dir))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-data-table.R ---
test_that("use_data_table() requires a package", {
  create_local_project()
  expect_usethis_error(use_data_table(), "not an R package")
})

test_that("use_data_table() Imports data.table", {
  create_local_package()
  use_package_doc()
  local_check_installed()
  local_roxygen_update_ns()
  local_check_fun_exists()

  use_data_table()

  expect_match(proj_desc()$get("Imports"), "data.table")
  expect_snapshot(roxygen_ns_show())
})

test_that("use_data_table() blocks use of Depends", {
  local_interactive(FALSE)

  create_local_package()
  use_package_doc()
  desc::desc_set("Depends", "data.table")
  local_check_installed()
  local_roxygen_update_ns()
  local_check_fun_exists()

  withr::local_options(list(usethis.quiet = FALSE))
  expect_snapshot(
    use_data_table(),
    transform = scrub_testpkg
  )

  expect_match(desc::desc_get("Imports"), "data.table")
  expect_snapshot(roxygen_ns_show())
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-data.R ---
test_that("use_data() errors for a non-package project", {
  create_local_project()
  expect_usethis_error(use_data(letters), "not an R package")
})

test_that("use_data() stores new, non-internal data", {
  pkg <- create_local_package()
  letters2 <- letters
  month.abb2 <- month.abb
  expect_false(desc::desc_has_fields("LazyData"))
  use_data(letters2, month.abb2)
  expect_true(desc::desc_has_fields("LazyData"))
  rm(letters2, month.abb2)

  load(proj_path("data", "letters2.rda"))
  load(proj_path("data", "month.abb2.rda"))
  expect_identical(letters2, letters)
  expect_identical(month.abb2, month.abb)
})

test_that("use_data() honors `overwrite` for non-internal data", {
  pkg <- create_local_package()
  letters2 <- letters
  use_data(letters2)

  expect_usethis_error(
    use_data(letters2),
    ".*data/letters2.rda.* already exist"
  )

  letters2 <- rev(letters)
  use_data(letters2, overwrite = TRUE)

  load(proj_path("data", "letters2.rda"))
  expect_identical(letters2, rev(letters))
})

test_that("use_data() stores new internal data", {
  pkg <- create_local_package()
  letters2 <- letters
  month.abb2 <- month.abb
  use_data(letters2, month.abb2, internal = TRUE)
  rm(letters2, month.abb2)

  load(proj_path("R", "sysdata.rda"))
  expect_identical(letters2, letters)
  expect_identical(month.abb2, month.abb)
})

test_that("use_data() honors `overwrite` for internal data", {
  pkg <- create_local_package()
  letters2 <- letters
  use_data(letters2, internal = TRUE)
  rm(letters2)

  expect_usethis_error(
    use_data(letters2, internal = TRUE),
    ".*R/sysdata.rda.* already exist"
  )

  letters2 <- rev(letters)
  use_data(letters2, internal = TRUE, overwrite = TRUE)

  load(proj_path("R", "sysdata.rda"))
  expect_identical(letters2, rev(letters))
})

test_that("use_data() writes version 3 by default", {
  create_local_package()

  x <- letters
  use_data(x, internal = TRUE, compress = FALSE)
  expect_identical(
    rawToChar(readBin(proj_path("R", "sysdata.rda"), n = 4, what = "raw")),
    "RDX3"
  )
})

test_that("use_data() can enforce `ascii = TRUE`", {
  create_local_package()

  x <- "h\u00EF"

  use_data(x)
  expect_false(tools::checkRdaFiles("data/x.rda")[["ASCII"]])

  use_data(x, ascii = TRUE, overwrite = TRUE)
  expect_true(tools::checkRdaFiles("data/x.rda")[["ASCII"]])
})

test_that("use_data_raw() does setup", {
  create_local_package()
  use_data_raw(open = FALSE)
  expect_proj_file(path("data-raw", "DATASET.R"))

  use_data_raw("daisy", open = FALSE)
  expect_proj_file(path("data-raw", "daisy.R"))

  expect_true(is_build_ignored("^data-raw$"))
})

test_that("use_data() does not decrease minimum version of R itself", {
  create_local_package()

  use_package("R", "depends", "4.1")
  original_minimum_r_version <- pkg_minimum_r_version()

  use_data(letters)

  expect_true(pkg_minimum_r_version() >= original_minimum_r_version)
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-description.R ---
# use_description_defaults() ----------------------------------------------

test_that("user's fields > usethis defaults", {
  d <- use_description_defaults(
    "pkg",
    fields = list(Title = "TEST1", URL = "TEST1")
  )
  expect_equal(d$Title, "TEST1")
  expect_equal(d$URL, "TEST1")
  expect_equal(d$Version, "0.0.0.9000")
})

test_that("usethis options > usethis defaults", {
  withr::local_options(list(
    usethis.description = list(License = "TEST")
  ))

  d <- use_description_defaults()
  expect_equal(d$License, "TEST")
  expect_equal(d$Version, "0.0.0.9000")
})

test_that("usethis options > usethis defaults, even for Authors@R", {
  withr::local_options(list(
    usethis.description = list(
      "Authors@R" = utils::person("Jane", "Doe")
    )
  ))
  d <- use_description_defaults()
  expect_equal(
    d$`Authors@R`,
    "person(given = \"Jane\",\n       family = \"Doe\")"
  )
  expect_match(d$`Authors@R`, '^person[(]given = "Jane"')
  expect_match(d$`Authors@R`, '"Doe"[)]$')
})

test_that("user's fields > options > defaults", {
  withr::local_options(list(
    usethis.description = list(License = "TEST1", Title = "TEST1")
  ))

  d <- use_description_defaults("pkg", fields = list(Title = "TEST2"))
  expect_equal(d$Title, "TEST2")
  expect_equal(d$License, "TEST1")
  expect_equal(d$Version, "0.0.0.9000")
})

test_that("automatically converts person object to text", {
  d <- use_description_defaults(
    "pkg",
    fields = list(`Authors@R` = person("H", "W"))
  )
  expect_match(d$`Authors@R`, '^person[(]given = "H"')
  expect_match(d$`Authors@R`, '"W"[)]$')
})

test_that("can set package", {
  d <- use_description_defaults(package = "TEST")
  expect_equal(d$Package, "TEST")
})

test_that("`roxygen = FALSE` is honoured", {
  d <- use_description_defaults(roxygen = FALSE)
  expect_null(d[["Roxygen"]])
  expect_null(d[["RoxygenNote"]])
})

# use_description ---------------------------------------------------------

test_that("creation succeeds even if options are broken", {
  withr::local_options(list(
    usethis.description = list(
      `Authors@R` = "person("
    )
  ))
  create_local_project()

  expect_no_error(use_description())
})

test_that("default description is tidy", {
  withr::local_options(list(usethis.description = NULL, devtools.desc = NULL))
  create_local_package()

  before <- readLines(proj_path("DESCRIPTION"))
  use_tidy_description()
  after <- readLines(proj_path("DESCRIPTION"))
  expect_equal(before, after)
})

test_that("valid CRAN names checked", {
  withr::local_options(list(usethis.description = NULL, devtools.desc = NULL))
  create_local_package(dir = file_temp(pattern = "invalid_pkg_name"))

  expect_no_error(use_description(check_name = FALSE))
  expect_usethis_error(
    use_description(check_name = TRUE),
    "is not a valid package name"
  )
})

test_that("proj_desc_field_update() can address an existing field", {
  pkg <- create_local_package()
  orig <- tools::md5sum(proj_path("DESCRIPTION"))

  ## specify existing value of existing field --> should be no op
  proj_desc_field_update(
    key = "Version",
    value = proj_version(),
    overwrite = FALSE
  )
  expect_identical(orig, tools::md5sum(proj_path("DESCRIPTION")))

  expect_usethis_error(
    proj_desc_field_update(
      key = "Version",
      value = "1.1.1",
      overwrite = FALSE
    ),
    "has a different value"
  )

  ## overwrite existing field
  proj_desc_field_update(
    key = "Version",
    value = "1.1.1",
    overwrite = TRUE
  )
  expect_identical(proj_version(), "1.1.1")
})

test_that("proj_desc_field_update() can add new field", {
  pkg <- create_local_package()
  proj_desc_field_update(key = "foo", value = "bar")
  expect_identical(proj_desc()$get_field("foo"), "bar")
})

test_that("proj_desc_field_update() ignores whitespace", {
  pkg <- create_local_package()
  proj_desc_field_update(key = "foo", value = "\n bar")
  proj_desc_field_update(key = "foo", value = "bar", overwrite = FALSE)
  expect_identical(proj_desc()$get_field("foo", trim_ws = FALSE), "\n bar")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-directory.R ---
test_that("create_directory() doesn't bother a pre-existing target dir", {
  tmp <- file_temp()
  dir_create(tmp)
  expect_true(is_dir(tmp))
  expect_no_error(create_directory(tmp))
  expect_true(is_dir(tmp))
})

test_that("create_directory() creates a directory", {
  tmp <- file_temp("yes")
  create_directory(tmp)
  expect_true(is_dir(tmp))
})

# check_path_is_directory -------------------------------------------------

test_that("no false positive for trailing slash", {
  pwd <- sub("/$", "", getwd())
  expect_no_error(check_path_is_directory(paste0(pwd, "/")))
})

test_that("symlink to directory is directory", {
  base <- dir_create(file_temp())
  base_a <- dir_create(path(base, "a"))
  base_b <- link_create(base_a, path(base, "b"))

  expect_no_error(check_path_is_directory(base_b))
})

# https://github.com/r-lib/usethis/issues/2069
test_that("relative symlink to directory is directory", {
  # It appears that creating links on Windows is tricky w.r.t. permissions:
  # Error: Error: [EPERM] Failed to link 'sub_dir' to 'relative_link_to_sub_dir': operation not permitted

  # See also https://github.com/r-lib/fs/pull/397 re: relative links

  # The original issue arose on macOS, so I'm willing to skip this test
  # on Windows.
  # If it's this hard for me to create the situation on Windows, presumably the
  # situation won't come up a lot in real life either.
  skip_on_os("windows")

  base_dir <- withr::local_tempdir()
  sub_dir <- dir_create(path(base_dir, "sub_dir"))
  withr::with_dir(
    base_dir,
    link_create("sub_dir", "relative_link_to_sub_dir")
  )

  relative_linky_path <- path(base_dir, "relative_link_to_sub_dir")
  expect_no_error(check_path_is_directory(relative_linky_path))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-documentation.R ---
test_that("use_package_doc() requires a package", {
  create_local_project()
  expect_false(has_package_doc())
  expect_usethis_error(use_package_doc(), "not an R package")
})

test_that("use_package_doc() creates the promised file", {
  create_local_package()
  use_package_doc()
  expect_proj_file("R", paste0(project_name(), "-package.R"))
  expect_true(has_package_doc())
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-edit.R ---
expect_r_file <- function(...) {
  expect_true(file_exists(path_home_r(...)))
}

expect_fs_file <- function(...) {
  expect_true(file_exists(path_home(...)))
}


test_that("edit_file() creates new directory and another and a file within", {
  tmp <- file_temp()
  expect_false(dir_exists(tmp))
  capture.output(new_file <- edit_file(path(tmp, "new_dir", "new_file")))
  expect_true(dir_exists(tmp))
  expect_true(dir_exists(path(tmp, "new_dir")))
  expect_true(file_exists(path(tmp, "new_dir", "new_file")))
})

test_that("edit_file() creates new file in existing directory", {
  tmp <- file_temp()
  dir_create(tmp)
  capture.output(new_file <- edit_file(path(tmp, "new_file")))
  expect_true(file_exists(path(tmp, "new_file")))
})

test_that("edit_file() copes with path to existing file", {
  tmp <- file_temp()
  dir_create(tmp)
  existing <- file_create(path(tmp, "a_file"))
  capture.output(res <- edit_file(path(tmp, "a_file")))
  expect_identical(existing, res)
})

test_that("edit_template() can create a new template", {
  create_local_package()

  edit_template("new_template")
  expect_proj_file("inst/templates/new_template")
})

## testing edit_XXX("user") only on travis and appveyor, because I don't want to
## risk creating user-level files de novo for an actual user, which would
## obligate me to some nerve-wracking clean up

test_that("edit_r_XXX() and edit_git_XXX() have default scope", {
  skip_if_no_git_user()
  ## run these manually if you already have these files or are happy to
  ## have them or delete them
  skip_if_not_ci()

  ## on Windows, under R CMD check, some env vars are set to sentinel values
  ## https://github.com/wch/r-source/blob/78da6e06aa0017564ec057b768f98c5c79e4d958/src/library/tools/R/check.R#L257
  ## we need to explicitly ensure R_ENVIRON_USER="" here
  withr::local_envvar(list(R_ENVIRON_USER = ""))

  expect_no_error(edit_r_profile())
  expect_no_error(edit_r_buildignore())
  expect_no_error(edit_r_environ())
  expect_no_error(edit_r_makevars())
  expect_no_error(edit_git_config())
  expect_no_error(edit_git_ignore())
})

test_that("edit_r_XXX('user') ensures the file exists", {
  ## run these manually if you already have these files or are happy to
  ## have them or delete them
  skip_if_not_ci()

  ## on Windows, under R CMD check, some env vars are set to sentinel values
  ## https://github.com/wch/r-source/blob/78da6e06aa0017564ec057b768f98c5c79e4d958/src/library/tools/R/check.R#L257
  ## we need to explicitly ensure R_ENVIRON_USER="" here
  withr::local_envvar(list(R_ENVIRON_USER = ""))

  edit_r_environ("user")
  expect_r_file(".Renviron")

  edit_r_profile("user")
  expect_r_file(".Rprofile")

  edit_r_makevars("user")
  expect_r_file(".R", "Makevars")
})

test_that("edit_r_buildignore() only works with packages", {
  create_local_project()

  expect_usethis_error(edit_r_buildignore(), "not an R package")

  use_description()
  edit_r_buildignore()
  expect_proj_file(".Rbuildignore")
})

test_that("can edit snippets", {
  path <- withr::local_tempdir()
  withr::local_envvar(c("XDG_CONFIG_HOME" = path))

  path <- edit_rstudio_snippets(type = "R")
  expect_true(file_exists(path))

  expect_snapshot(
    edit_rstudio_snippets("not-existing-type"),
    error = TRUE
  )
})

test_that("edit_r_profile() respects R_PROFILE_USER", {
  path1 <- user_path_prep(file_temp())
  withr::local_envvar(list(R_PROFILE_USER = path1))

  path2 <- edit_r_profile("user")
  expect_equal(path1, path2)
})


test_that("edit_git_XXX('user') ensures the file exists", {
  skip_if_no_git_user()
  ## run these manually if you already have these files or are happy to
  ## have them or delete them
  skip_if_not_ci()

  edit_git_config("user")
  expect_fs_file(".gitconfig")

  edit_git_ignore("user")
  expect_fs_file(".gitignore")
  expect_match(
    git_cfg_get("core.excludesfile", where = "global"),
    "gitignore"
  )
})

test_that("edit_r_profile() ensures .Rprofile exists in project", {
  create_local_package()
  edit_r_profile("project")
  expect_proj_file(".Rprofile")

  create_local_project()
  edit_r_profile("project")
  expect_proj_file(".Rprofile")
})

test_that("edit_r_environ() ensures .Renviron exists in project", {
  create_local_package()
  edit_r_environ("project")
  expect_proj_file(".Renviron")

  create_local_project()
  edit_r_environ("project")
  expect_proj_file(".Renviron")
})

test_that("edit_r_makevars() ensures .R/Makevars exists in package", {
  create_local_package()
  edit_r_makevars("project")
  expect_proj_file(".R", "Makevars")
})

test_that("edit_git_config() ensures git ignore file exists in project", {
  create_local_package()
  edit_git_config("project")
  expect_proj_file(".git", "config")

  create_local_project()
  edit_git_config("project")
  expect_proj_file(".git", "config")
})

test_that("edit_git_ignore() ensures .gitignore exists in project", {
  create_local_package()
  edit_git_ignore("project")
  expect_proj_file(".gitignore")

  create_local_project()
  edit_git_ignore("project")
  expect_proj_file(".gitignore")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-git-default-branch.R ---
test_that("git_default_branch() consults the default branch candidates, in order", {
  skip_on_cran()
  skip_if_no_git_user()
  local_interactive(FALSE)

  create_local_project()
  use_git()
  repo <- git_repo()

  gert::git_add(".gitignore", repo = repo)
  gert::git_commit("a commit, so we are not on an unborn branch", repo = repo)

  # singleton branch, with weird name
  git_default_branch_rename(from = git_branch(), to = "foofy")
  expect_equal(git_default_branch(), "foofy")

  # two weirdly named branches, but one matches init.defaultBranch (local) config
  gert::git_branch_create("blarg", checkout = TRUE, repo = repo)
  use_git_config("project", `init.defaultBranch` = "blarg")
  expect_equal(git_default_branch(), "blarg")

  # one of the Usual Suspects shows up
  gert::git_branch_create("master", checkout = TRUE, repo = repo)
  expect_equal(git_default_branch(), "master")

  # and another Usual Suspect shows up
  gert::git_branch_create("main", checkout = TRUE, repo = repo)
  expect_equal(git_default_branch(), "main")

  # finally, prefer something that matches what upstream says is default
  gert::git_branch_create(
    "default-branch-of-upstream",
    checkout = TRUE,
    repo = repo
  )
  local_git_default_branch_remote()
  expect_equal(git_default_branch(), "default-branch-of-upstream")
})

test_that("git_default_branch() errors if can't find obvious local default branch", {
  skip_on_cran()
  skip_if_no_git_user()
  local_interactive(FALSE)

  create_local_project()
  use_git()
  repo <- git_repo()

  gert::git_add(".gitignore", repo = repo)
  gert::git_commit("a commit, so we are not on an unborn branch", repo = repo)
  git_default_branch_rename(from = git_branch(), to = "foofy")

  gert::git_branch_create("blarg", checkout = TRUE, repo = repo)

  expect_error(git_default_branch(), class = "error_default_branch")
})

test_that("git_default_branch() errors for local vs remote mismatch", {
  skip_on_cran()
  skip_if_no_git_user()
  local_interactive(FALSE)

  create_local_project()
  use_git()
  repo <- git_repo()

  gert::git_add(".gitignore", repo = repo)
  gert::git_commit("a commit, so we are not on an unborn branch", repo = repo)
  git_default_branch_rename(from = git_branch(), to = "foofy")
  local_git_default_branch_remote()

  expect_error(git_default_branch(), class = "error_default_branch")

  gert::git_branch_create("blarg", checkout = TRUE, repo = repo)
  local_git_default_branch_remote()
  expect_error(git_default_branch(), class = "error_default_branch")
})

test_that("git_default_branch_rename() surfaces files that smell fishy", {
  skip_on_cran()
  skip_if_no_git_user()
  local_interactive(FALSE)

  # for snapshot purposes, I don't want a random project name
  create_local_project(path(path_temp(), "abcde"))
  use_git()
  repo <- git_repo()

  gert::git_add(".gitignore", repo = repo)
  gert::git_commit("a commit, so we are not on an unborn branch", repo = repo)

  # make sure we start with default branch = 'master'
  git_default_branch_rename(from = git_branch(), to = "master")
  expect_equal(git_default_branch(), "master")

  badge_lines <- c(
    "<!-- badges: start -->",
    "[![Codecov test coverage](https://codecov.io/gh/OWNER/REPO/branch/master/graph/badge.svg)](https://codecov.io/gh/OWNER/REPO?branch=master)",
    "<!-- badges: end -->"
  )
  cli::cat_line(badge_lines, file = proj_path("README.md"))

  gha_lines <- c(
    "on:",
    "  push:",
    "    branches:",
    "      - master"
  )
  create_directory(".github/workflows")
  cli::cat_line(gha_lines, file = path(".github", "workflows", "blah.yml"))

  create_directory("whatever/foo")
  cli::cat_line(
    "edit: https://github.com/OWNER/REPO/edit/master/%s",
    file = path("whatever", "foo", "_bookdown.yaml")
  )

  withr::local_options(usethis.quiet = FALSE)
  expect_snapshot(
    git_default_branch_rename()
  )
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-git.R ---
test_that("uses_git() works", {
  skip_if_no_git_user()

  create_local_package()
  expect_false(uses_git())
  expect_usethis_error(check_uses_git())

  git_init()

  expect_true(uses_git())
  expect_no_error(check_uses_git())
})

test_that('use_git_config(scope = "project") errors if project not using git', {
  create_local_package()
  expect_usethis_error(
    use_git_config(scope = "project", user.name = "USER.NAME"),
    "Cannot detect that project is already a Git repository"
  )
})

test_that("use_git_config() can set local config", {
  skip_if_no_git_user()

  create_local_package()
  use_git()
  use_git_config(
    scope = "project",
    user.name = "Jane",
    user.email = "jane@example.org",
    init.defaultBranch = "main"
  )
  r <- git_repo()
  expect_identical(git_cfg_get("user.name", "local"), "Jane")
  expect_identical(git_cfg_get("user.email", "local"), "jane@example.org")
  expect_identical(git_cfg_get("init.defaultBranch", "local"), "main")
  expect_identical(git_cfg_get("init.defaultbranch", "local"), "main")
})

test_that("use_git_config() can set a non-existing config field", {
  skip_if_no_git_user()

  create_local_package()
  use_git()

  expect_null(git_cfg_get("aaa.bbb"))
  use_git_config(scope = "project", aaa.bbb = "ccc")
  expect_identical(git_cfg_get("aaa.bbb"), "ccc")
})

test_that("use_git_config() facilitates round trips", {
  skip_if_no_git_user()

  create_local_package()
  use_git()

  orig <- use_git_config(scope = "project", aaa.bbb = "ccc")
  expect_null(orig$aaa.bbb)
  expect_identical(git_cfg_get("aaa.bbb"), "ccc")

  new <- use_git_config(scope = "project", aaa.bbb = NULL)
  expect_identical(new$aaa.bbb, "ccc")
  expect_null(git_cfg_get("aaa.bbb"))
})

test_that("use_git_hook errors if project not using git", {
  create_local_package()
  expect_usethis_error(
    use_git_hook(
      "pre-commit",
      render_template("readme-rmd-pre-commit.sh")
    ),
    "Cannot detect that project is already a Git repository"
  )
})

test_that("git remote handlers work", {
  skip_if_no_git_user()

  create_local_package()
  use_git()

  expect_null(git_remotes())

  use_git_remote(name = "foo", url = "foo_url")
  expect_identical(git_remotes(), list(foo = "foo_url"))

  use_git_remote(name = "foo", url = "new_url", overwrite = TRUE)
  expect_identical(git_remotes(), list(foo = "new_url"))

  use_git_remote(name = "foo", url = NULL, overwrite = TRUE)
  expect_null(git_remotes())
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-github-actions.R ---
test_that("use_github_action() allows for custom urls", {
  skip_if_no_git_user()
  skip_if_offline("github.com")

  local_interactive(FALSE)

  create_local_package()
  use_git()
  use_git_remote(name = "origin", url = "https://github.com/OWNER/REPO")
  use_readme_md()

  withr::local_options(usethis.quiet = FALSE)
  expect_snapshot(
    use_github_action(
      url = "https://raw.githubusercontent.com/r-lib/actions/v2/examples/check-full.yaml",
      readme = "https://github.com/r-lib/actions/blob/v2/examples/README.md"
    )
  )
  expect_proj_dir(".github")
  expect_proj_dir(".github/workflows")
  expect_proj_file(".github/workflows/R-CMD-check.yaml")
})

test_that("use_github_action() still errors in non-interactive environment", {
  expect_snapshot(use_github_action(), error = TRUE)
})

test_that("use_github_action() appends yaml in name if missing", {
  skip_if_no_git_user()
  skip_if_offline("github.com")
  local_interactive(FALSE)

  create_local_package()
  use_git()
  use_git_remote(name = "origin", url = "https://github.com/OWNER/REPO")

  use_github_action("check-full")

  expect_proj_dir(".github")
  expect_proj_dir(".github/workflows")
  expect_proj_file(".github/workflows/R-CMD-check.yaml")
})

test_that("use_github_action() accepts a ref", {
  skip_if_no_git_user()
  skip_if_offline("github.com")
  local_interactive(FALSE)

  create_local_package()
  use_git()
  use_git_remote(name = "origin", url = "https://github.com/OWNER/REPO")

  use_github_action("check-full", ref = "v1")
  expect_snapshot(
    read_utf8(proj_path(".github/workflows/R-CMD-check.yaml"), n = 1)
  )
})

test_that("uses_github_action() reports usage of GitHub Actions", {
  skip_if_no_git_user()
  skip_if_offline("github.com")
  local_interactive(FALSE)

  create_local_package()
  expect_false(uses_github_actions())

  use_git()
  use_git_remote(name = "origin", url = "https://github.com/OWNER/REPO")

  local_mocked_bindings(
    use_github_actions_badge = function(name, repo_spec) NULL
  )

  use_github_action("check-standard")

  expect_true(uses_github_actions())
})

test_that("check_uses_github_actions() can throw error", {
  create_local_package()
  withr::local_options(list(crayon.enabled = FALSE, cli.width = Inf))
  expect_snapshot(
    check_uses_github_actions(),
    error = TRUE,
    transform = scrub_testpkg
  )
})

test_that("use_github_action() accepts a name", {
  skip_if_no_git_user()
  skip_if_offline("github.com")
  local_interactive(FALSE)

  create_local_package()
  use_git()
  use_git_remote(name = "origin", url = "https://github.com/OWNER/REPO")
  use_readme_md()

  use_github_action("check-release")

  expect_proj_dir(".github")
  expect_proj_dir(".github/workflows")
  expect_proj_file(".github/workflows/R-CMD-check.yaml")

  readme_lines <- read_utf8(proj_path("README.md"))
  expect_match(readme_lines, "R-CMD-check", all = FALSE)

  # .github has been Rbuildignored
  expect_true(is_build_ignored("^\\.github$"))
})

test_that("use_tidy_github_actions() configures the full check and pr commands", {
  skip_if_no_git_user()
  skip_if_offline("github.com")
  local_interactive(FALSE)

  create_local_package()
  use_git()
  gert::git_add(".gitignore", repo = git_repo())
  gert::git_commit(
    "a commit, so we are not on an unborn branch",
    repo = git_repo()
  )
  use_git_remote(name = "origin", url = "https://github.com/OWNER/REPO")
  use_readme_md()
  use_tidy_github_actions()

  expect_proj_file(".github/workflows/R-CMD-check.yaml")

  yml <- yaml::yaml.load_file(proj_path(".github/workflows/R-CMD-check.yaml"))
  size_build_matrix <-
    length(yml[["jobs"]][["R-CMD-check"]][["strategy"]][["matrix"]][["config"]])
  expect_gte(size_build_matrix, 6) # release, r-devel, 4 previous versions

  expect_proj_file(".github/workflows/pkgdown.yaml")
  expect_proj_file(".github/workflows/test-coverage.yaml")
  expect_proj_file(".github/workflows/format-suggest.yaml")

  readme_lines <- read_utf8(proj_path("README.md"))
  expect_match(readme_lines, "R-CMD-check", all = FALSE)
  expect_match(readme_lines, "test coverage", all = FALSE)
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-github.R ---
test_that("has_github_links() uses the target_repo, if provided", {
  skip_if_no_git_user()
  create_local_package()
  local_interactive(FALSE)
  use_git()

  desc::desc_set_urls("https://github.com/OWNER/REPO")
  desc::desc_set("BugReports", "https://github.com/OWNER/REPO/issues")

  tr <- list(url = "git@github.com:OWNER/REPO.git")

  expect_true(has_github_links(tr))
})

test_that("use_github_links populates empty URL field", {
  skip_if_no_git_user()
  local_interactive(FALSE)
  create_local_package()
  use_git()

  local_mocked_bindings(
    github_url_from_git_remotes = function() "https://github.com/OWNER/REPO"
  )

  # when no URL field
  use_github_links()
  expect_equal(proj_desc()$get_urls(), "https://github.com/OWNER/REPO")
  expect_equal(
    proj_desc()$get_field("BugReports"),
    "https://github.com/OWNER/REPO/issues"
  )
})

test_that("use_github_links() aborts or appends URLs when it should", {
  skip_if_no_git_user()
  local_interactive(FALSE)
  create_local_package()
  use_git()

  local_mocked_bindings(
    github_url_from_git_remotes = function() "https://github.com/OWNER/REPO"
  )

  d <- proj_desc()
  d$set_urls(c("https://existing.url", "https://existing.url1"))
  d$write()

  expect_snapshot(use_github_links(overwrite = FALSE), error = TRUE)

  use_github_links(overwrite = TRUE)
  expect_equal(
    proj_desc()$get_urls(),
    c(
      "https://existing.url",
      "https://existing.url1",
      "https://github.com/OWNER/REPO"
    )
  )
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-github_token.R ---
test_that("code_hint_with_host() works", {
  expect_identical(code_hint_with_host("foo"), "foo()")
  expect_identical(code_hint_with_host("foo", arg_name = "arg"), "foo()")

  host_github <- "https://api.github.com"
  expect_identical(code_hint_with_host("foo", host = host_github), "foo()")
  expect_identical(
    code_hint_with_host("foo", host = host_github, arg_name = "arg"),
    "foo()"
  )

  host_ghe <- "https://github.acme.com"
  expect_identical(
    code_hint_with_host("foo", host = host_ghe),
    'foo("https://github.acme.com")'
  )
  expect_identical(
    code_hint_with_host("foo", host = host_ghe, arg_name = "arg"),
    'foo(arg = \"https://github.acme.com\")'
  )
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-helpers.R ---
test_that("valid_package_name() enforces valid package names", {
  # Contain only ASCII letters, numbers, and '.'
  # Have at least two characters
  # Start with a letter
  # Not end with '.'

  expect_true(valid_package_name("aa"))
  expect_true(valid_package_name("a7"))
  expect_true(valid_package_name("a.2"))

  expect_false(valid_package_name("a"))
  expect_false(valid_package_name("a-2"))
  expect_false(valid_package_name("2fa"))
  expect_false(valid_package_name(".fa"))
  expect_false(valid_package_name("aa\u00C0")) # \u00C0 is a-grave
  expect_false(valid_package_name("a3."))
})

test_that("valid_file_name() enforces valid file names", {
  # Contain only ASCII letters, numbers, '-', and '_'
  expect_true(valid_file_name("aa.R"))
  expect_true(valid_file_name("a7.R"))
  expect_true(valid_file_name("a-2.R"))
  expect_true(valid_file_name("a_2.R"))
  expect_false(valid_file_name("aa\u00C0.R")) # \u00C0 is a-grave
  expect_false(valid_file_name("a?3.R"))
})

# use_dependency ----------------------------------------------------------

test_that("we message for new type and are silent for same type", {
  create_local_package()
  withr::local_options(usethis.quiet = FALSE)

  expect_snapshot(
    use_dependency("crayon", "Imports")
  )
  expect_silent(use_dependency("crayon", "Imports"))
})

test_that("we message for version change and are silent for same version", {
  create_local_package()
  withr::local_options(usethis.quiet = FALSE)

  expect_snapshot(
    use_dependency("crayon", "Imports")
  )
  expect_snapshot(
    use_dependency("crayon", "Imports", min_version = "1.0.0")
  )
  expect_silent(use_dependency("crayon", "Imports", min_version = "1.0.0"))
  expect_snapshot(
    use_dependency("crayon", "Imports", min_version = "2.0.0")
  )
  expect_snapshot(
    use_dependency("crayon", "Imports", min_version = "1.0.0")
  )
})

## https://github.com/r-lib/usethis/issues/99
test_that("use_dependency() upgrades a dependency", {
  create_local_package()
  withr::local_options(usethis.quiet = FALSE)

  expect_snapshot(use_dependency("usethis", "Suggests"))
  expect_match(desc::desc_get("Suggests"), "usethis")

  expect_snapshot(use_dependency("usethis", "Imports"))
  expect_match(desc::desc_get("Imports"), "usethis")
  expect_no_match(desc::desc_get("Suggests"), "usethis")
})

## https://github.com/r-lib/usethis/issues/99
test_that("use_dependency() declines to downgrade a dependency", {
  create_local_package()
  withr::local_options(usethis.quiet = FALSE)

  expect_snapshot(use_dependency("usethis", "Imports"))
  expect_match(desc::desc_get("Imports"), "usethis")

  expect_snapshot(use_dependency("usethis", "Suggests"))
  expect_match(desc::desc_get("Imports"), "usethis")
  expect_no_match(desc::desc_get("Suggests"), "usethis")
})

test_that("can add LinkingTo dependency if other dependency already exists", {
  create_local_package()
  use_dependency("rlang", "Imports")

  withr::local_options(usethis.quiet = FALSE)
  expect_snapshot(
    use_dependency("rlang", "LinkingTo")
  )
  deps <- proj_deps()
  expect_setequal(deps$type, c("Imports", "LinkingTo"))
  expect_setequal(deps$package, "rlang")
})

test_that("use_dependency() does not fall over on 2nd LinkingTo request", {
  create_local_package()
  local_interactive(FALSE)

  use_dependency("rlang", "LinkingTo")

  withr::local_options(usethis.quiet = FALSE)

  expect_snapshot(use_dependency("rlang", "LinkingTo"))
})

# https://github.com/r-lib/usethis/issues/1649
test_that("use_dependency() can level up a LinkingTo dependency", {
  create_local_package()

  use_dependency("rlang", "LinkingTo")
  use_dependency("rlang", "Suggests")

  withr::local_options(usethis.quiet = FALSE)

  expect_snapshot(use_package("rlang"))
  deps <- proj_deps()
  expect_setequal(deps$type, c("Imports", "LinkingTo"))
  expect_setequal(deps$package, "rlang")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-ignore.R ---
test_that(". escaped around surround by anchors", {
  expect_equal(escape_path("."), "^\\.$")
})

test_that("strip trailing /", {
  expect_equal(escape_path("./"), "^\\.$")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-jenkins.R ---
test_that("use_jenkins() creates a Makefile AND a Jenkinsfile at project root", {
  pkg <- create_local_package()
  use_jenkins()
  expect_proj_file("Makefile")
  expect_proj_file("Jenkinsfile")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-latest-dependencies.R ---
test_that("sets version for imports & depends dependencies", {
  skip_if_offline()
  withr::local_options(list(repos = c(CRAN = "https://cloud.r-project.org")))

  create_local_package()
  use_package("usethis")
  use_package("desc", "Depends")
  use_latest_dependencies()

  deps <- proj_deps()
  expect_equal(
    deps$version[deps$package %in% c("usethis", "desc")] == "*",
    c(FALSE, FALSE)
  )
})

test_that("doesn't affect suggests", {
  skip_if_offline()
  withr::local_options(list(repos = c(CRAN = "https://cloud.r-project.org")))

  create_local_package()
  use_package("cli", "Suggests")
  use_latest_dependencies()

  deps <- proj_deps()
  expect_equal(deps$version[deps$package == "cli"], "*")
})

test_that("does nothing for a base package", {
  skip_if_offline()
  withr::local_options(list(repos = c(CRAN = "https://cloud.r-project.org")))

  create_local_package()
  use_package("tools")
  # if usethis ever depends on a recommended package, we could test that here too
  use_latest_dependencies()

  deps <- proj_deps()
  expect_equal(deps$version[deps$package == "tools"], "*")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-license.R ---
test_that("use_mit_license() works", {
  create_local_package()
  use_mit_license()

  expect_equal(desc::desc_get_field("License"), "MIT + file LICENSE")

  expect_proj_file("LICENSE.md")
  expect_true(is_build_ignored("^LICENSE\\.md$"))

  expect_proj_file("LICENSE")
  expect_false(is_build_ignored("^LICENSE$"))
})

test_that("use_proprietary_license() works", {
  create_local_package()
  use_proprietary_license("foo")

  expect_equal(desc::desc_get_field("License"), "file LICENSE")
  expect_proj_file("LICENSE")
  # TODO add snapshot test
})

test_that("other licenses work without error", {
  create_local_package()

  expect_no_error(use_agpl_license(3))
  expect_no_error(use_apache_license(2))
  expect_no_error(use_cc0_license())
  expect_no_error(use_ccby_license())
  expect_no_error(use_gpl_license(2))
  expect_no_error(use_gpl_license(3))
  expect_no_error(use_lgpl_license(2.1))
  expect_no_error(use_lgpl_license(3))

  # old fallbacks
  expect_no_error(use_agpl3_license())
  expect_no_error(use_gpl3_license())
  expect_no_error(use_apl2_license())
})

test_that("check license gives useful errors", {
  expect_usethis_error(check_license_version(1, 2), "must be 2")
  expect_usethis_error(check_license_version(1, 2:4), "must be 2, 3, or 4")
})

test_that("generate correct abbreviations", {
  expect_equal(license_abbr("GPL", 2, TRUE), "GPL (>= 2)")
  expect_equal(license_abbr("GPL", 2, FALSE), "GPL-2")
  expect_equal(
    license_abbr("Apache License", 2, FALSE),
    "Apache License (== 2)"
  )
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-lifecycle.R ---
test_that("use_lifecycle() imports badges", {
  create_local_package()
  use_package_doc()
  withr::local_options(usethis.quiet = FALSE, cli.width = Inf)

  expect_snapshot(
    use_lifecycle(),
    transform = scrub_testpkg
  )

  expect_proj_file("man", "figures", "lifecycle-stable.svg")
  expect_equal(roxygen_ns_show(), "#' @importFrom lifecycle deprecated")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-line-ending.R ---
test_that("can detect path from RStudio project file", {
  create_local_package()
  use_rstudio("posix")
  expect_equal(proj_line_ending(), "\n")

  file_delete(proj_path(paste(paste0(project_name(), ".Rproj"))))
  use_rstudio("windows")
  expect_equal(proj_line_ending(), "\r\n")
})

test_that("can detect path from DESCRIPTION or .R file", {
  create_local_project()

  write_utf8(proj_path("DESCRIPTION"), c("x", "y", "z"), line_ending = "\r\n")
  expect_equal(proj_line_ending(), "\r\n")
  file_delete(proj_path("DESCRIPTION"))

  dir_create(proj_path("R"))
  write_utf8(proj_path("R/test.R"), c("x", "y", "z"), line_ending = "\r\n")
  expect_equal(proj_line_ending(), "\r\n")
})

test_that("falls back to platform specific encoding", {
  create_local_project()
  expect_equal(proj_line_ending(), platform_line_ending())
})

test_that("correctly detect line encoding", {
  path <- file_temp()

  con <- file(path, open = "wb")
  writeLines(c("a", "b", "c"), con, sep = "\n")
  close(con)
  expect_equal(detect_line_ending(path), "\n")

  con <- file(path, open = "wb")
  writeLines(c("a", "b", "c"), con, sep = "\r\n")
  close(con)
  expect_equal(detect_line_ending(path), "\r\n")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-logo.R ---
test_that("use_logo() doesn't error with no README", {
  skip_if_not_installed("magick")
  skip_on_os("solaris")

  create_local_package()
  img <- magick::image_write(magick::image_read("logo:"), "logo.png")
  expect_no_error(use_logo("logo.png"))
})

test_that("use_logo() shows a clickable path with README", {
  skip_if_not_installed("magick")
  skip_on_os("solaris")

  create_local_package()
  use_readme_md()
  img <- magick::image_write(magick::image_read("logo:"), "logo.png")
  withr::local_options(usethis.quiet = FALSE)
  expect_snapshot(use_logo("logo.png"), transform = scrub_testpkg)
})

# https://github.com/r-lib/usethis/issues/1999
test_that("use_logo() writes a file in lowercase and it knows that", {
  skip_if_not_installed("magick")
  skip_on_os("solaris")

  create_local_package()
  img <- magick::image_write(magick::image_read("logo:"), "LoGo.PNG")

  withr::local_options(list(usethis.quiet = FALSE))
  expect_snapshot(use_logo("LoGo.PNG"), transform = scrub_testpkg)
  expect_proj_file("man", "figures", "logo.png")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-make.R ---
test_that("use_make() creates a Makefile at project root", {
  pkg <- create_local_package()
  use_make()
  expect_proj_file("Makefile")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-news.R ---
test_that("use_news_md() sets (development version)/'Initial submission' in new pkg", {
  create_local_package()
  local_cran_version(NULL)

  use_news_md()

  expect_snapshot(
    writeLines(read_utf8(proj_path("NEWS.md"))),
    transform = scrub_testpkg
  )
})

test_that("use_news_md() sets bullet to 'Added a NEWS.md file...' when on CRAN", {
  create_local_package()

  # on CRAN, local dev version
  proj_desc_field_update(key = "Version", value = "0.1.0.9000")
  local_cran_version("0.1.0")

  use_news_md()

  expect_snapshot(
    writeLines(read_utf8(proj_path("NEWS.md"))),
    transform = scrub_testpkg
  )
})

test_that("use_news_md() sets version number when 'production version'", {
  create_local_package()

  proj_desc_field_update(key = "Version", value = "0.2.0")
  local_cran_version(NULL)

  use_news_md()

  expect_snapshot(
    writeLines(read_utf8(proj_path("NEWS.md"))),
    transform = scrub_testpkg
  )
})

test_that("use_news_heading() tolerates blank lines at start", {
  create_local_package()

  header <- sprintf("# %s (development version)", project_name())
  writeLines(c("", header, "", "* Fixed the bugs."), con = "NEWS.md")

  use_news_heading(version = "1.0.0")
  contents <- read_utf8("NEWS.md")

  expected <- sprintf("# %s 1.0.0", project_name())
  expect_equal(contents[[2L]], expected)
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-package.R ---
test_that("use_package() won't facilitate dependency on tidyverse/tidymodels", {
  create_local_package()
  expect_usethis_error(use_package("tidyverse"), "rarely a good idea")
  expect_usethis_error(use_package("tidymodels"), "rarely a good idea")
})

test_that("use_package() guides new packages but not pre-existing ones", {
  create_local_package()
  withr::local_options(usethis.quiet = FALSE)
  expect_snapshot({
    use_package("withr")
    use_package("withr")
    use_package("withr", "Suggests")
  })
})

test_that("use_package() handles R versions with aplomb", {
  create_local_package()
  withr::local_options(usethis.quiet = FALSE)
  expect_snapshot(use_package("R"), error = TRUE)
  expect_snapshot(use_package("R", type = "Depends"), error = TRUE)
  expect_snapshot(use_package("R", type = "Depends", min_version = "3.6"))
  expect_equal(subset(proj_deps(), package == "R")$version, ">= 3.6")
  local_mocked_bindings(r_version = function() "4.1")

  expect_snapshot(use_package("R", type = "Depends", min_version = TRUE))
  expect_equal(subset(proj_deps(), package == "R")$version, ">= 4.1")
})

test_that("use_package(type = 'Suggests') guidance w/o and w/ rlang", {
  create_local_package()
  withr::local_options(usethis.quiet = FALSE)

  expect_snapshot(use_package("withr", "Suggests"))
  ui_silence(use_package("rlang"))
  expect_snapshot(use_package("purrr", "Suggests"))
})

# use_dev_package() -----------------------------------------------------------

test_that("use_dev_package() writes a remote", {
  create_local_package()
  local_ui_yep()

  use_dev_package("usethis")
  expect_equal(proj_desc()$get_remotes(), "r-lib/usethis")
})

test_that("use_dev_package() can override over default remote", {
  create_local_package()

  use_dev_package("usethis", remote = "github::r-lib/usethis")

  expect_equal(proj_desc()$get_remotes(), "github::r-lib/usethis")
})

test_that("package_remote() works for an installed package with github URL", {
  d <- desc::desc(
    text = c(
      "Package: test",
      "URL: https://github.com/OWNER/test"
    )
  )
  local_ui_yep()
  expect_equal(package_remote(d), "OWNER/test")
})

test_that("package_remote() works for package installed from github or gitlab", {
  d <- desc::desc(
    text = c(
      "Package: test",
      "RemoteUsername: OWNER",
      "RemoteRepo: test"
    )
  )

  d$set(RemoteType = "github")
  expect_equal(package_remote(d), "OWNER/test")

  d$set(RemoteType = "gitlab")
  expect_equal(package_remote(d), "gitlab::OWNER/test")
})

test_that("package_remote() errors if no remote and no github URL", {
  d <- desc::desc(text = c("Package: test"))
  expect_usethis_error(package_remote(d), "Cannot determine remote")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-pipe.R ---
test_that("use_pipe() requires a package", {
  create_local_project()
  expect_usethis_error(use_pipe(), "not an R package")
})

test_that("use_pipe(export = TRUE) adds promised file, Imports magrittr", {
  create_local_package()
  use_pipe(export = TRUE)
  expect_equal(desc::desc_get_field("Imports"), "magrittr")
  expect_proj_file("R", "utils-pipe.R")
})

test_that("use_pipe(export = FALSE) adds roxygen to package doc", {
  create_local_package()
  use_package_doc()
  use_pipe(export = FALSE)
  expect_equal(desc::desc_get_field("Imports"), "magrittr")

  expect_snapshot(roxygen_ns_show())
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-pkgdown.R ---
test_that("use_pkgdown() requires a package", {
  create_local_project()
  expect_usethis_error(use_pkgdown(), "not an R package")
})

test_that("use_pkgdown() creates and ignores the promised file/dir", {
  create_local_package()
  local_interactive(FALSE)
  local_check_installed()
  local_mocked_bindings(pkgdown_version = function() "1.9000")
  withr::local_options(usethis.quiet = FALSE)

  expect_snapshot(
    use_pkgdown()
  )

  expect_true(uses_pkgdown())
  expect_true(is_build_ignored("^_pkgdown\\.yml$"))
  expect_true(is_build_ignored("^docs$"))
})

# pkgdown helpers ----
test_that("pkgdown helpers behave in the absence of pkgdown", {
  create_local_package()
  expect_null(pkgdown_config_path())
  expect_false(uses_pkgdown())
  expect_equal(pkgdown_config_meta(), list())
  expect_null(pkgdown_url())
})

test_that("pkgdown_config_meta() returns a list", {
  create_local_package()
  local_interactive(FALSE)
  local_check_installed()
  local_mocked_bindings(pkgdown_version = function() "1.9000")

  use_pkgdown()
  expect_type(pkgdown_config_meta(), "list")

  writeLines(c("home:", "  strip_header: true"), pkgdown_config_path())
  expect_equal(
    pkgdown_config_meta(),
    list(home = list(strip_header = TRUE))
  )
})

test_that("pkgdown_url() returns correct data, warns if pedantic", {
  create_local_package()
  local_interactive(FALSE)
  local_check_installed()
  local_mocked_bindings(pkgdown_version = function() "1.9000")

  use_pkgdown()

  # empty config
  expect_null(pkgdown_url())
  expect_silent(pkgdown_url())
  withr::local_options(list(usethis.quiet = FALSE))
  expect_snapshot(
    pkgdown_url(pedantic = TRUE)
  )

  # nonempty config, but no url
  writeLines(c("home:", "  strip_header: true"), pkgdown_config_path())
  expect_null(pkgdown_url())
  expect_silent(pkgdown_url())
  expect_snapshot(
    pkgdown_url(pedantic = TRUE)
  )

  # config has url
  writeLines("url: https://usethis.r-lib.org", pkgdown_config_path())
  expect_equal(pkgdown_url(), "https://usethis.r-lib.org")

  # config has url with trailing slash
  writeLines(
    "url: https://malcolmbarrett.github.io/tidysmd/",
    pkgdown_config_path()
  )
  expect_equal(pkgdown_url(), "https://malcolmbarrett.github.io/tidysmd/")
})

test_that("tidyverse_url() leaves trailing slash alone, almost always", {
  url <- "https://malcolmbarrett.github.io/tidysmd/"
  out <- tidyverse_url(url, tr = list(repo_name = "REPO", repo_owner = "OWNER"))
  expect_equal(out, url)
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-proj-desc.R ---
test_that("proj_desc_field_update() only messages when adding", {
  create_local_package()
  withr::local_options(list(usethis.quiet = FALSE, crayon.enabled = FALSE))

  expect_snapshot({
    proj_desc_field_update("Config/Needs/foofy", "alfa", append = TRUE)
    proj_desc_field_update("Config/Needs/foofy", "alfa", append = TRUE)
    proj_desc_field_update("Config/Needs/foofy", "bravo", append = TRUE)
  })
  expect_equal(proj_desc()$get_list("Config/Needs/foofy"), c("alfa", "bravo"))
})

test_that("proj_desc_field_update() works with multiple values", {
  create_local_package()
  # Add something to begin with
  proj_desc_field_update("Config/Needs/foofy", "alfa", append = TRUE)
  withr::local_options(list(usethis.quiet = FALSE, crayon.enabled = FALSE))

  expect_snapshot({
    proj_desc_field_update(
      "Config/Needs/foofy",
      c("alfa", "bravo"),
      append = TRUE
    )
  })
  expect_equal(proj_desc()$get_list("Config/Needs/foofy"), c("alfa", "bravo"))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-proj.R ---
test_that("proj_set() errors on non-existent path", {
  expect_usethis_error(
    proj_set("abcedefgihklmnopqrstuv"),
    "does not exist"
  )
})

test_that("proj_set() errors if no criteria are fulfilled", {
  tmpdir <- withr::local_tempdir(pattern = "i-am-not-a-project")
  expect_usethis_error(
    proj_set(tmpdir),
    "does not appear to be inside a project or package"
  )
})

test_that("proj_set() can be forced, even if no criteria are fulfilled", {
  tmpdir <- withr::local_tempdir(pattern = "i-am-not-a-project")

  expect_no_error(old <- proj_set(tmpdir, force = TRUE))
  withr::defer(proj_set(old))
  expect_identical(proj_get(), proj_path_prep(tmpdir))
})

test_that("is_package() detects package-hood", {
  create_local_package()
  expect_true(is_package())

  create_local_project()
  expect_false(is_package())
})

test_that("check_is_package() errors for non-package", {
  create_local_project()
  expect_usethis_error(check_is_package(), "not an R package")
})

test_that("check_is_package() can reveal who's asking", {
  create_local_project()
  expect_snapshot(
    error = TRUE,
    check_is_package("foo()"),
    transform = scrub_testproj
  )
})

test_that("proj_path() appends to the project path", {
  create_local_project()
  expect_equal(
    proj_path("a", "b", "c"),
    path(proj_get(), "a/b/c")
  )
  expect_identical(proj_path("a", "b", "c"), proj_path("a/b/c"))
})

test_that("proj_path() errors with absolute paths", {
  create_local_project()
  expect_snapshot(proj_path(c("/a", "b", "/c")), error = TRUE)
  expect_snapshot(proj_path("/a", "b", "/c"), error = TRUE)
  expect_snapshot(proj_path("/a", c("b", "/c")), error = TRUE)
})

test_that("proj_path() with no inputs returns result of length 1, not 0", {
  create_local_project()
  expect_equal(proj_path(), proj_get())
})

test_that("proj_rel_path() returns path part below the project", {
  create_local_project()
  expect_equal(proj_rel_path(proj_path("a/b/c")), "a/b/c")
})

test_that("proj_rel_path() returns path 'as is' if not in project", {
  create_local_project()
  expect_identical(proj_rel_path(path_temp()), path_temp())
})

test_that("proj_set() enforces proj path preparation policy", {
  # specifically: check that proj_get() returns realized path
  t <- withr::local_tempdir("proj-set-path-prep")

  # a/b/d and a/b2/d identify same directory
  a <- path_real(dir_create(path(t, "a")))
  b <- dir_create(path(a, "b"))
  b2 <- link_create(b, path(a, "b2"))
  d <- dir_create(path(b, "d"))

  # input path includes symbolic link
  path_with_symlinks <- path(b2, "d")
  expect_equal(path_rel(path_with_symlinks, a), path("b2/d"))

  # force = TRUE
  local_project(path_with_symlinks, force = TRUE)
  expect_equal(path_rel(proj_get(), a), path("b/d"))

  # force = FALSE
  file_create(path(b, "d", ".here"))
  proj_set(path_with_symlinks, force = FALSE)
  expect_equal(path_rel(proj_get(), a), path("b/d"))
})

test_that("proj_path_prep() passes NULL through", {
  expect_null(proj_path_prep(NULL))
})

test_that("is_in_proj() detects whether files are (or would be) in project", {
  create_local_package()

  ## file does not exist but would be in project if created
  expect_true(is_in_proj(proj_path("fiction")))

  ## file exists in project
  expect_true(is_in_proj(proj_path("DESCRIPTION")))

  ## file does not exist and would not be in project if created
  expect_false(is_in_proj(file_temp()))

  ## file exists and is not in project
  expect_false(is_in_proj(path_temp()))
})

test_that("is_in_proj() does not activate a project", {
  pkg <- create_local_package()
  path <- proj_path("DESCRIPTION")
  expect_true(is_in_proj(path))
  local_project(NULL)
  expect_false(is_in_proj(path))
  expect_false(proj_active())
})

test_that("proj_sitrep() reports current working/project state", {
  pkg <- create_local_package()
  x <- proj_sitrep()
  expect_s3_class(x, "sitrep")
  expect_false(is.null(x[["working_directory"]]))
  expect_identical(
    fs::path_file(pkg),
    fs::path_file(x[["active_usethis_proj"]])
  )
})

test_that("with_project() runs code in temp proj, restores (lack of) proj", {
  old_project <- proj_get_()
  withr::defer(proj_set_(old_project))

  temp_proj <- create_project(
    file_temp(pattern = "TEMPPROJ"),
    rstudio = FALSE,
    open = FALSE
  )

  proj_set_(NULL)
  expect_null(proj_get_())

  res <- with_project(path = temp_proj, proj_get_())

  expect_identical(res, temp_proj)
  expect_null(proj_get_())
})

test_that("with_project() runs code in temp proj, restores original proj", {
  old_project <- proj_get_()
  withr::defer(proj_set_(old_project))

  host <- create_project(
    file_temp(pattern = "host"),
    rstudio = FALSE,
    open = FALSE
  )
  guest <- create_project(
    file_temp(pattern = "guest"),
    rstudio = FALSE,
    open = FALSE
  )

  proj_set(host)
  expect_identical(proj_get_(), host)

  res <- with_project(path = guest, proj_get_())

  expect_identical(res, guest)
  expect_identical(proj_get(), host)
})

test_that("with_project() works when temp proj == original proj", {
  old_project <- proj_get_()
  withr::defer(proj_set_(old_project))

  host <- create_project(
    file_temp(pattern = "host"),
    rstudio = FALSE,
    open = FALSE
  )

  proj_set(host)
  expect_identical(proj_get_(), host)

  res <- with_project(path = host, proj_get_())

  expect_identical(res, host)
  expect_identical(proj_get(), host)
})

test_that("local_project() activates proj til scope ends", {
  old_project <- proj_get_()
  withr::defer(proj_set_(old_project))

  new_proj <- file_temp(pattern = "localprojtest")
  create_project(new_proj, rstudio = FALSE, open = FALSE)
  proj_set_(NULL)

  foo <- function() {
    local_project(new_proj)
    proj_sitrep()
  }
  res <- foo()

  expect_identical(
    res[["active_usethis_proj"]],
    as.character(proj_path_prep(new_proj))
  )
  expect_null(proj_get_())
})

# https://github.com/r-lib/usethis/issues/954
test_that("proj_activate() works with relative path when RStudio is not detected", {
  sandbox <- path_real(dir_create(file_temp("sandbox")))
  withr::defer(dir_delete(sandbox))
  orig_proj <- proj_get_()
  withr::defer(proj_set(orig_proj, force = TRUE))
  withr::local_dir(sandbox)
  local_rstudio_available(FALSE)

  rel_path_proj <- path_file(file_temp(pattern = "mno"))
  out_path <- create_project(rel_path_proj, rstudio = FALSE, open = FALSE)
  expect_no_error(
    result <- proj_activate(rel_path_proj)
  )
  expect_true(result)
  expect_equal(path_wd(), out_path)
  expect_equal(proj_get(), out_path)
})

# https://github.com/r-lib/usethis/issues/1498
test_that("local_project()'s `quiet` argument works", {
  temp_proj <- create_project(
    file_temp(pattern = "TEMPPROJ"),
    rstudio = FALSE,
    open = FALSE
  )
  withr::defer(dir_delete(temp_proj))
  local_project(path = temp_proj, quiet = TRUE, force = TRUE, setwd = FALSE)
  expect_true(getOption("usethis.quiet"))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-r.R ---
test_that("use_r() creates a .R file below R/", {
  create_local_package()
  use_r("foo")
  expect_proj_file("R/foo.R")
})

test_that("use_test() creates a test file", {
  create_local_package()
  use_test("foo", open = FALSE)
  expect_proj_file("tests", "testthat", "test-foo.R")
})

test_that("use_test_helper() creates a helper file", {
  create_local_package()

  expect_snapshot(
    error = TRUE,
    use_test_helper(open = FALSE)
  )
  use_testthat()

  use_test_helper(open = FALSE)
  withr::local_options(list(usethis.quiet = FALSE))
  expect_snapshot(
    use_test_helper("foo", open = FALSE)
  )

  expect_proj_file("tests", "testthat", "helper.R")
  expect_proj_file("tests", "testthat", "helper-foo.R")
})

test_that("can use use_test() in a project", {
  create_local_project()
  expect_no_error(use_test("foofy"))
})

# helpers -----------------------------------------------------------------

test_that("compute_name() errors if no RStudio", {
  local_rstudio_available(FALSE)
  expect_snapshot(compute_name(), error = TRUE)
})

test_that("compute_name() sets extension if missing", {
  expect_equal(compute_name("foo"), "foo.R")
})

test_that("compute_name() validates its inputs", {
  expect_snapshot(error = TRUE, {
    compute_name("foo.c")
    compute_name("R/foo.c")
    compute_name(c("a", "b"))
    compute_name("")
    compute_name("****")
  })
})

test_that("compute_active_name() errors if no files open", {
  expect_snapshot(compute_active_name(NULL), error = TRUE)
})

test_that("compute_active_name() checks directory", {
  expect_snapshot(compute_active_name("foo/bar.R"), error = TRUE)
})

test_that("compute_active_name() standardises name", {
  dir <- create_local_project()

  expect_equal(
    compute_active_name(path(dir, "R/bar.R"), "c"),
    "bar.c"
  )
  expect_equal(
    compute_active_name(path(dir, "src/bar.cpp"), "R"),
    "bar.R"
  )
  expect_equal(
    compute_active_name(path(dir, "tests/testthat/test-bar.R"), "R"),
    "bar.R"
  )

  expect_equal(
    compute_active_name(path(dir, "tests/testthat/_snaps/bar.md"), "R"),
    "bar.R"
  )
  # https://github.com/r-lib/usethis/issues/1690
  expect_equal(
    compute_active_name(path(dir, "R/data.frame.R"), "R"),
    "data.frame.R"
  )
})

# https://github.com/r-lib/usethis/issues/1863
test_that("compute_name() accepts the declared extension", {
  expect_equal(compute_name("foo.cpp", ext = "cpp"), "foo.cpp")
})

test_that("as_test_helper_file() works", {
  expect_equal(as_test_helper_file(), "helper.R")
  expect_equal(as_test_helper_file("helper"), "helper.R")
  expect_equal(as_test_helper_file("helper.R"), "helper.R")
  expect_equal(as_test_helper_file("stuff"), "helper-stuff.R")
  expect_equal(as_test_helper_file("helper-stuff"), "helper-stuff.R")
  expect_equal(as_test_helper_file("stuff.R"), "helper-stuff.R")
  expect_equal(as_test_helper_file("helper-stuff.R"), "helper-stuff.R")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-rcpp.R ---
test_that("use_rcpp() requires a package", {
  create_local_project()
  expect_usethis_error(use_rcpp(), "not an R package")
})

test_that("use_rcpp() creates files/dirs, edits DESCRIPTION and .gitignore", {
  create_local_package()
  use_roxygen_md()

  # pretend Rcpp is installed
  local_check_installed()

  use_rcpp("test")
  expect_match(desc::desc_get("LinkingTo"), "Rcpp")
  expect_match(desc::desc_get("Imports"), "Rcpp")
  expect_proj_dir("src")
  expect_proj_file("src", "test.cpp")

  ignores <- read_utf8(proj_path("src", ".gitignore"))
  expect_true(all(c("*.o", "*.so", "*.dll") %in% ignores))
})

test_that("use_rcpp_armadillo() creates Makevars files and edits DESCRIPTION", {
  create_local_package()
  use_roxygen_md()

  local_interactive(FALSE)
  # pretend RcppArmadillo is installed
  local_check_installed()

  use_rcpp_armadillo("code")
  expect_proj_file("src", "code.cpp")
  expect_match(desc::desc_get("LinkingTo"), "RcppArmadillo")
  expect_proj_file("src", "Makevars")
  expect_proj_file("src", "Makevars.win")
})

test_that("use_rcpp_eigen() edits DESCRIPTION", {
  create_local_package()
  use_roxygen_md()

  # pretend RcppArmadillo is installed
  local_check_installed()
  use_rcpp_eigen("code")
  expect_proj_file("src", "code.cpp")
  expect_match(desc::desc_get("LinkingTo"), "RcppEigen")
})

test_that("use_src() doesn't message if not needed", {
  create_local_package()
  use_roxygen_md()
  use_package_doc()
  use_src()

  withr::local_options(list(usethis.quiet = FALSE))

  expect_silent(use_src())
})

test_that("use_makevars() respects pre-existing Makevars", {
  pkg <- create_local_package()

  dir_create(proj_path("src"))
  makevars_file <- proj_path("src", "Makevars")
  makevars_win_file <- proj_path("src", "Makevars.win")

  writeLines("USE_CXX = CXX11", makevars_file)
  file_copy(makevars_file, makevars_win_file)

  before_makevars_file <- read_utf8(makevars_file)
  before_makevars_win_file <- read_utf8(makevars_win_file)

  makevars_settings <- list(
    "PKG_CXXFLAGS" = "-Wno-reorder"
  )
  use_makevars(makevars_settings)

  expect_identical(before_makevars_file, read_utf8(makevars_file))
  expect_identical(before_makevars_win_file, read_utf8(makevars_win_file))
})

test_that("use_makevars() creates Makevars files with appropriate configuration", {
  pkg <- create_local_package()

  makevars_settings <- list(
    "CXX_STD" = "CXX11"
  )
  use_makevars(makevars_settings)

  makevars_content <- paste0(names(makevars_settings), " = ", makevars_settings)

  expect_identical(makevars_content, read_utf8(proj_path("src", "Makevars")))
  expect_identical(
    makevars_content,
    read_utf8(proj_path("src", "Makevars.win"))
  )
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-readme.R ---
test_that("use_readme_md() creates README.md", {
  create_local_package()
  use_readme_md()
  expect_proj_file("README.md")
})

test_that("use_readme_rmd() creates README.Rmd", {
  skip_if_not_installed("rmarkdown")

  create_local_package()
  use_readme_rmd()
  expect_proj_file("README.Rmd")
})

test_that("use_readme_rmd() sets up git pre-commit hook if pkg uses git", {
  skip_if_no_git_user()
  skip_if_not_installed("rmarkdown")

  create_local_package()
  use_git()
  use_readme_rmd(open = FALSE)
  expect_proj_file(".git", "hooks", "pre-commit")
})

test_that("use_readme_md() has expected form for a non-GitHub package", {
  skip_if_not_installed("rmarkdown")
  local_interactive(FALSE)

  create_local_package()
  use_readme_md()
  expect_snapshot(writeLines(read_utf8("README.md")), transform = scrub_testpkg)
})

test_that("use_readme_md() has expected form for a GitHub package", {
  skip_if_not_installed("rmarkdown")
  local_interactive(FALSE)
  local_target_repo_spec("OWNER/TESTPKG")

  create_local_package()
  use_readme_md()
  expect_snapshot(writeLines(read_utf8("README.md")), transform = scrub_testpkg)
})

test_that("use_readme_rmd() has expected form for a non-GitHub package", {
  skip_if_not_installed("rmarkdown")
  local_interactive(FALSE)

  create_local_package()
  use_readme_rmd()
  expect_snapshot(
    writeLines(read_utf8("README.Rmd")),
    transform = scrub_testpkg
  )
})

test_that("use_readme_rmd() has expected form for a GitHub package", {
  skip_if_not_installed("rmarkdown")
  local_interactive(FALSE)
  local_target_repo_spec("OWNER/TESTPKG")

  create_local_package()
  use_readme_rmd()
  expect_snapshot(
    writeLines(read_utf8("README.Rmd")),
    transform = scrub_testpkg
  )
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-release.R ---
# release bullets ---------------------------------------------------------

test_that("release bullets don't change accidentally", {
  withr::local_options(usethis.description = NULL)
  create_local_package()

  local_mocked_bindings(
    get_revdeps = function() "usethis"
  )

  # First release
  expect_snapshot(
    writeLines(release_checklist("0.1.0", on_cran = FALSE)),
    transform = scrub_testpkg
  )

  # Patch release
  expect_snapshot(
    writeLines(release_checklist("0.0.1", on_cran = TRUE)),
    transform = scrub_testpkg
  )

  # Major release
  expect_snapshot(
    writeLines(release_checklist("1.0.0", on_cran = TRUE)),
    transform = scrub_testpkg
  )
})

test_that("non-patch + lifecycle = advanced deprecation process", {
  withr::local_options(usethis.description = NULL)
  create_local_package()
  use_package("lifecycle")

  local_mocked_bindings(
    tidy_minimum_r_version = function() "3.6",
    get_revdeps = function() character(),
    has_github_links = function(...) FALSE,
    gh_milestone_number = function(...) NA
  )

  has_deprecation <- function(x) any(grepl("Advance deprecations", x))
  expect_true(has_deprecation(release_checklist("1.0.0", on_cran = TRUE)))
  expect_true(has_deprecation(release_checklist("1.1.0", on_cran = TRUE)))
  expect_false(has_deprecation(release_checklist("1.1.1", on_cran = TRUE)))
})

test_that("get extra news bullets if available", {
  env <- env(release_bullets = function() "Extra bullets")
  expect_equal(release_extra_bullets(env), "* [ ] Extra bullets")

  env <- env(release_questions = function() "Extra bullets")
  expect_equal(release_extra_bullets(env), "* [ ] Extra bullets")

  env <- env()
  expect_equal(release_extra_bullets(env), character())
})

test_that("construct correct revdep bullet", {
  create_local_package()
  env <- env(release_extra_revdeps = function() c("waldo", "testthat"))

  local_mocked_bindings(
    get_revdeps = function() "usethis"
  )

  expect_snapshot({
    release_revdepcheck(on_cran = FALSE)
    release_revdepcheck(on_cran = TRUE, is_posit_pkg = FALSE)
    release_revdepcheck(on_cran = TRUE, is_posit_pkg = TRUE)
    release_revdepcheck(on_cran = TRUE, is_posit_pkg = TRUE, env = env)
  })
})

test_that("RStudio-ness detection works", {
  withr::local_options(usethis.description = NULL)
  create_local_package()
  local_mocked_bindings(
    tidy_minimum_r_version = function() numeric_version("3.6"),
    get_revdeps = function() "usethis"
  )

  expect_false(is_posit_pkg())

  desc <- proj_desc()
  desc$add_author(given = "PoSiT, PbC", role = "fnd")
  desc$add_author(given = "someone", email = "someone@Rstudio.com")
  desc$add_urls("https://github.com/tidyverse/WHATEVER")
  desc$set_dep("R", "Depends", version = ">= 3.4")
  desc$write()

  expect_true(is_posit_pkg())
  expect_true(is_in_posit_org())
  expect_false(is_posit_person_canonical())
  expect_true(author_has_rstudio_email())

  expect_snapshot(
    writeLines(release_checklist("1.0.0", on_cran = TRUE)),
    transform = scrub_testpkg
  )
})

test_that("can find milestone numbers", {
  skip_if_offline("github.com")

  tr <- list(
    repo_owner = "r-lib",
    repo_name = "usethis",
    api_url = "https://api.github.com"
  )

  expect_equal(
    gh_milestone_number(tr, "2.1.6", state = "all"),
    8
  )
  expect_equal(
    gh_milestone_number(tr, "0.0.0", state = "all"),
    NA_integer_
  )
})

test_that("gh_milestone_number() returns NA when gh() errors", {
  local_mocked_bindings(
    gh_tr = function(tr) {
      function(endpoint, ...) {
        ui_abort("nope!")
      }
    }
  )
  tr <- list(
    repo_owner = "r-lib",
    repo_name = "usethis",
    api_url = "https://api.github.com"
  )
  expect_true(is.na(gh_milestone_number(tr, "1.1.1")))
})

# news --------------------------------------------------------------------

test_that("must have at least one heading", {
  expect_usethis_error(
    news_latest(""),
    regexp = "No top-level headings"
  )
})

test_that("trims blank lines when extracting bullets", {
  lines <- c(
    "# Heading",
    "",
    "Contents",
    ""
  )
  expect_equal(news_latest(lines), "Contents\n")

  lines <- c(
    "# Heading",
    "",
    "Contents 1",
    "",
    "# Heading",
    "",
    "Contents 2"
  )
  expect_equal(news_latest(lines), "Contents 1\n")
})

test_that("returns empty string if no bullets", {
  lines <- c(
    "# Heading",
    "",
    "# Heading"
  )
  expect_equal(news_latest(lines), "")
})

# draft release ----------------------------------------------------------------
test_that("get_release_data() works if no file found", {
  skip_if_no_git_user()

  local_interactive(FALSE)
  create_local_package()
  use_git()
  gert::git_add(".gitignore")
  gert::git_commit("we need at least one commit")

  res <- get_release_data()
  expect_equal(res$Version, "0.0.0.9000")
  expect_match(res$SHA, "[[:xdigit:]]{40}")
})

test_that("get_release_data() works for old-style CRAN-RELEASE", {
  skip_if_no_git_user()

  local_interactive(FALSE)
  create_local_package()
  use_git()
  gert::git_add(".gitignore")
  gert::git_commit("we need at least one commit")
  HEAD <- gert::git_info(repo = git_repo())$commit

  write_utf8(
    proj_path("CRAN-RELEASE"),
    glue(
      "
      This package was submitted to CRAN on YYYY-MM-DD.
      Once it is accepted, delete this file and tag the release (commit {HEAD})."
    )
  )

  res <- get_release_data(tr = list(repo_spec = "OWNER/REPO"))
  expect_equal(res$Version, "0.0.0.9000")
  expect_equal(res$SHA, HEAD)
  expect_equal(path_file(res$file), "CRAN-RELEASE")
})

test_that("get_release_data() works for new-style CRAN-RELEASE", {
  skip_if_no_git_user()

  local_interactive(FALSE)
  create_local_package()
  use_git()
  gert::git_add(".gitignore")
  gert::git_commit("we need at least one commit")
  HEAD <- gert::git_info(repo = git_repo())$commit

  write_utf8(
    proj_path("CRAN-SUBMISSION"),
    glue(
      "
      Version: 1.2.3
      Date: 2021-10-14 23:57:41 UTC
      SHA: {HEAD}"
    )
  )

  res <- get_release_data(tr = list(repo_spec = "OWNER/REPO"))
  expect_equal(res$Version, "1.2.3")
  expect_equal(res$SHA, HEAD)
  expect_equal(path_file(res$file), "CRAN-SUBMISSION")
})

test_that("cran_version() returns package version if package found", {
  local_mocked_bindings(available.packages = function(...) {
    # simulate minimal available.packages entry
    as.matrix(data.frame(Package = c(usethis = "usethis"), Version = "1.0.0"))
  })

  expect_null(cran_version("doesntexist"))
  expect_equal(cran_version("usethis"), package_version("1.0.0"))
})

test_that("cran_version() returns NULL if no available packages", {
  local_mocked_bindings(available.packages = function(...) NULL)
  expect_null(cran_version("doesntexist"))
})

test_that("default_cran_mirror() is respects set value but falls back to cloud", {
  withr::local_options(repos = c(CRAN = "https://example.com"))
  expect_equal(default_cran_mirror(), c(CRAN = "https://example.com"))

  withr::local_options(repos = c(CRAN = "@CRAN@"))
  expect_equal(default_cran_mirror(), c(CRAN = "https://cloud.r-project.org"))

  withr::local_options(repos = c())
  expect_equal(default_cran_mirror(), c(CRAN = "https://cloud.r-project.org"))
})

test_that("no revdep release bullets when there are no revdeps", {
  withr::local_options(usethis.description = NULL)
  create_local_package()

  local_mocked_bindings(
    get_revdeps = function() NULL
  )

  expect_snapshot(
    writeLines(release_checklist("1.0.0", on_cran = TRUE)),
    transform = scrub_testpkg
  )
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-rename-files.R ---
test_that("checks uncommitted files", {
  create_local_package()
  expect_usethis_error(rename_files("foo", "bar"))

  git_init()
  use_r("foo", open = FALSE)
  expect_usethis_error(
    rename_files("foo", "bar"),
    "uncommitted changes"
  )
})

test_that("renames R and test and snapshot files", {
  create_local_package()
  local_mocked_bindings(
    challenge_uncommitted_changes = function(...) invisible()
  )
  git_init()

  use_r("foo", open = FALSE)
  rename_files("foo", "bar")
  expect_proj_file("R/bar.R")

  use_test("foo", open = FALSE)
  rename_files("foo", "bar")
  expect_proj_file("tests/testthat/test-bar.R")

  dir_create(proj_path("tests", "testthat", "_snaps"))
  write_utf8(proj_path("tests", "testthat", "_snaps", "foo.md"), "abc")
  rename_files("foo", "bar")
  expect_proj_file("tests/testthat/_snaps/bar.md")
})

test_that("renames src/ files", {
  create_local_package()
  local_mocked_bindings(
    challenge_uncommitted_changes = function(...) invisible()
  )
  git_init()

  use_src()
  file_create(proj_path("src/foo.c"))
  file_create(proj_path("src/foo.h"))

  withr::local_options(list(usethis.quiet = FALSE))
  expect_snapshot({
    rename_files("foo", "bar")
  })

  expect_proj_file("src/bar.c")
  expect_proj_file("src/bar.h")
})

test_that("strips context from test file", {
  create_local_package()
  local_mocked_bindings(
    challenge_uncommitted_changes = function(...) invisible()
  )
  git_init()

  use_testthat()
  write_utf8(
    proj_path("tests", "testthat", "test-foo.R"),
    c(
      "context('bar')",
      "",
      "a <- 1"
    )
  )

  rename_files("foo", "bar")
  lines <- read_utf8(proj_path("tests", "testthat", "test-bar.R"))
  expect_equal(lines, "a <- 1")
})

test_that("rename paths in test file", {
  create_local_package()
  local_mocked_bindings(
    challenge_uncommitted_changes = function(...) invisible()
  )
  git_init()

  use_testthat()
  write_utf8(proj_path("tests", "testthat", "test-foo.txt"), "10")
  write_utf8(proj_path("tests", "testthat", "test-foo.R"), "test-foo.txt")

  rename_files("foo", "bar")
  expect_proj_file("tests/testthat/test-bar.txt")
  lines <- read_utf8(proj_path("tests", "testthat", "test-bar.R"))
  expect_equal(lines, "test-bar.txt")
})

test_that("does not remove non-R dots in filename", {
  create_local_package()
  local_mocked_bindings(
    challenge_uncommitted_changes = function(...) invisible()
  )
  git_init()

  file_create(proj_path("R/foo.bar.R"))
  rename_files("foo.bar", "baz.qux")
  expect_proj_file("R/baz.qux.R")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-revdep.R ---
test_that("use_revdep() requires a package", {
  create_local_project()
  expect_usethis_error(use_revdep(), "not an R package")
})

test_that("use_revdep() creates and ignores files/dirs", {
  create_local_package()
  use_revdep()
  expect_proj_file("revdep", ".gitignore")
  expect_true(is_build_ignored("^revdep$"))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-rmarkdown.R ---
test_that("use_rmarkdown_template() creates everything as promised, defaults", {
  create_local_package()
  use_rmarkdown_template()
  path <- path("inst", "rmarkdown", "templates", "template-name")
  yml <- read_utf8(proj_path(path, "template.yaml"))
  expect_true(
    all(
      c(
        "name: Template Name",
        "description: >",
        "   A description of the template",
        "create_dir: FALSE"
      ) %in%
        yml
    )
  )
  expect_proj_file(path, "skeleton", "skeleton.Rmd")
})

test_that("use_rmarkdown_template() creates everything as promised, args", {
  create_local_package()
  use_rmarkdown_template(
    template_name = "aaa",
    template_dir = "bbb",
    template_description = "ccc",
    template_create_dir = TRUE
  )
  path <- path("inst", "rmarkdown", "templates", "bbb")
  yml <- read_utf8(proj_path(path, "template.yaml"))
  expect_true(
    all(
      c("name: aaa", "description: >", "   ccc", "create_dir: TRUE") %in% yml
    )
  )
  expect_proj_file(path, "skeleton", "skeleton.Rmd")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-roxygen.R ---
test_that("use_package_doc() compatible with roxygen_ns_append()", {
  create_local_package()
  withr::local_options(list(usethis.quiet = FALSE, crayon.enabled = FALSE))

  expect_snapshot(use_package_doc(), transform = scrub_testpkg)
  expect_snapshot(roxygen_ns_append("test"), transform = scrub_testpkg)
  expect_silent(roxygen_ns_append("test"))
})

test_that("use_roxygen_md() adds DESCRIPTION fields to naive package", {
  skip_if_not_installed("roxygen2")

  pkg <- create_local_package()
  use_roxygen_md()

  desc <- proj_desc()
  expect_equal(desc$get("Roxygen"), c(Roxygen = "list(markdown = TRUE)"))
  expect_true(desc$has_fields("RoxygenNote"))
  expect_true(uses_roxygen_md())
})

test_that("use_roxygen_md() finds 'markdown = TRUE' in presence of other stuff", {
  skip_if_not_installed("roxygen2")

  pkg <- create_local_package()
  desc::desc_set(
    Roxygen = 'list(markdown = TRUE, r6 = FALSE, load = "source", roclets = c("collate", "namespace", "rd", "roxyglobals::global_roclet"))'
  )

  local_check_installed()
  expect_no_error(use_roxygen_md())
  expect_true(uses_roxygen_md())
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-rstudio.R ---
test_that("use_rstudio() creates .Rproj file, named after directory", {
  dir <- create_local_package(rstudio = FALSE)
  use_rstudio()
  rproj <- path_file(dir_ls(proj_get(), regexp = "[.]Rproj$"))
  expect_identical(path_ext_remove(rproj), path_file(dir))

  # Always uses POSIX line endings
  expect_equal(proj_line_ending(), "\n")
})

test_that("use_rstudio() can opt-out of reformatting", {
  create_local_project(rstudio = FALSE)
  use_rstudio(reformat = FALSE)
  out <- readLines(rproj_path())
  expect_true(is.na(match("AutoAppendNewline", out)))
  expect_true(is.na(match("StripTrailingWhitespace", out)))
  expect_true(is.na(match("LineEndingConversion", out)))
})

test_that("use_rstudio() omits package-related config for a project", {
  create_local_project(rstudio = FALSE)
  use_rstudio()
  out <- readLines(rproj_path())
  expect_true(is.na(match("BuildType: Package", out)))
})

test_that("an RStudio project is recognized", {
  create_local_package(rstudio = TRUE)
  expect_true(is_rstudio_project())
  expect_match(rproj_path(), "\\.Rproj$")
})

test_that("we error if there isn't exactly one Rproj files", {
  dir <- withr::local_tempdir()
  path <- dir_create(path(dir, "test"))

  expect_snapshot(rproj_path(path), error = TRUE)

  file_touch(path(path, "a.Rproj"))
  file_touch(path(path, "b.Rproj"))
  expect_snapshot(rproj_path(path), error = TRUE)
})

test_that("a non-RStudio project is not recognized", {
  create_local_package(rstudio = FALSE)
  expect_false(is_rstudio_project())
  expect_snapshot(rproj_path(), error = TRUE, transform = scrub_testpkg)
})


test_that("Rproj is parsed (actually, only colon-containing lines)", {
  tmp <- withr::local_tempfile()
  writeLines(c("a: a", "", "b: b", "I have no colon"), tmp)
  expect_identical(
    parse_rproj(tmp),
    list(a = "a", "", b = "b", "I have no colon")
  )
})

test_that("Existing field(s) in Rproj can be modified", {
  tmp <- withr::local_tempfile()
  writeLines(
    c(
      "Version: 1.0",
      "",
      "RestoreWorkspace: Default",
      "SaveWorkspace: Yes",
      "AlwaysSaveHistory: Default"
    ),
    tmp
  )
  before <- parse_rproj(tmp)
  delta <- list(RestoreWorkspace = "No", SaveWorkspace = "No")
  after <- modify_rproj(tmp, delta)
  expect_identical(before[c(1, 2, 5)], after[c(1, 2, 5)])
  expect_identical(after[3:4], delta)
})

test_that("we can roundtrip an Rproj file", {
  create_local_package(rstudio = TRUE)
  rproj_file <- rproj_path()
  before <- read_utf8(rproj_file)
  rproj <- modify_rproj(rproj_file, list())
  writeLines(serialize_rproj(rproj), rproj_file)
  after <- read_utf8(rproj_file)
  expect_identical(before, after)
})

test_that("use_blank_state('project') modifies Rproj", {
  create_local_package(rstudio = TRUE)
  use_blank_slate("project")
  rproj <- parse_rproj(rproj_path())
  expect_equal(rproj$RestoreWorkspace, "No")
  expect_equal(rproj$SaveWorkspace, "No")
})

test_that("use_blank_state() modifies user-level RStudio prefs", {
  path <- withr::local_tempdir()
  withr::local_envvar(c("XDG_CONFIG_HOME" = path))

  use_blank_slate()

  prefs <- rstudio_prefs_read()
  expect_equal(prefs[["save_workspace"]], "never")
  expect_false(prefs[["load_workspace"]])
})

test_that("use_rstudio_preferences", {
  path <- withr::local_tempdir()
  withr::local_envvar(c("XDG_CONFIG_HOME" = path))

  use_rstudio_preferences(x = 1, y = "a")

  prefs <- rstudio_prefs_read()
  expect_equal(prefs$x, 1)
  expect_equal(prefs$y, "a")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-template.R ---
test_that("can leave existing file unchanged, without an error", {
  create_local_package()
  desc_lines_before <- read_utf8(proj_path("DESCRIPTION"))
  expect_no_error(
    use_template("NEWS.md", "DESCRIPTION")
  )
  desc_lines_after <- read_utf8(proj_path("DESCRIPTION"))
  expect_identical(desc_lines_before, desc_lines_after)
})

# helpers -----------------------------------------------------------------

test_that("find_template errors if template missing", {
  expect_usethis_error(find_template("xxx"), "Could not find template")
})

test_that("find_template can find templates for tricky Rbuildignored files", {
  expect_match(find_template("codecov.yml"), "codecov\\.yml$")
  expect_match(find_template("cran-comments.md"), "cran-comments\\.md$")
  expect_match(find_template("template.Rproj"), "template\\.Rproj$")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-test.R ---
test_that("check_edition() validates inputs", {
  local_mocked_bindings(testthat_version = function() numeric_version("3.2.0"))

  expect_snapshot(check_edition(20), error = TRUE)
  expect_snapshot(check_edition("x"), error = TRUE)
  expect_equal(check_edition(1.5), 1)
  expect_equal(check_edition(), 3)
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-testthat.R ---
test_that("use_testhat() sets up infrastructure", {
  pkg <- create_local_package()
  use_testthat()
  expect_match(proj_desc()$get("Suggests"), "testthat")
  expect_proj_dir("tests", "testthat")
  expect_proj_file("tests", "testthat.R")
  expect_true(uses_testthat())
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-tibble.R ---
test_that("use_tibble() requires a package", {
  create_local_project()
  expect_usethis_error(use_tibble(), "not an R package")
})

test_that("use_tibble() Imports tibble and imports tibble::tibble()", {
  create_local_package()

  withr::local_options(list(usethis.quiet = FALSE))
  local_roxygen_update_ns()
  local_check_installed()
  ui_silence(use_package_doc())
  local_check_fun_exists()

  expect_snapshot(
    use_tibble(),
    transform = scrub_testpkg
  )

  expect_match(proj_desc()$get("Imports"), "tibble")

  pkg_doc <- readLines(package_doc_path())
  expect_match(pkg_doc, "#' @importFrom tibble tibble", all = FALSE)
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-tidyverse.R ---
test_that("use_tidy_description() alphabetises dependencies and remotes", {
  pkg <- create_local_package()
  use_package("usethis")
  use_package("desc")
  use_package("withr", "Suggests")
  use_package("gh", "Suggests")
  desc::desc_set_remotes(c("r-lib/styler", "jimhester/lintr"))
  use_tidy_description()
  desc <- read_utf8(proj_path("DESCRIPTION"))
  expect_gt(grep("usethis", desc), grep("desc", desc))
  expect_gt(grep("withr", desc), grep("gh", desc))
  expect_gt(grep("r\\-lib\\/styler", desc), grep("jimhester\\/lintr", desc))
})

test_that("use_tidy_dependencies() isn't overly informative", {
  skip_if_offline("github.com")

  create_local_package()
  use_package_doc(open = FALSE)
  withr::local_options(usethis.quiet = FALSE, cli.width = Inf)

  expect_snapshot(
    use_tidy_dependencies(),
    transform = scrub_testpkg
  )
})

test_that("use_tidy_GITHUB-STUFF() adds and Rbuildignores files", {
  local_interactive(FALSE)
  local_target_repo_spec("OWNER/REPO")

  create_local_package()
  use_git()
  use_tidy_contributing()
  use_tidy_support()
  use_tidy_issue_template()
  use_tidy_coc()
  expect_proj_file(".github/CONTRIBUTING.md")
  expect_proj_file(".github/ISSUE_TEMPLATE/issue_template.md")
  expect_proj_file(".github/SUPPORT.md")
  expect_proj_file(".github/CODE_OF_CONDUCT.md")
  expect_true(is_build_ignored("^\\.github$"))
})

test_that("use_tidy_github() adds and Rbuildignores files", {
  local_interactive(FALSE)
  local_target_repo_spec("OWNER/REPO")

  create_local_package()
  use_git()
  use_tidy_github()
  expect_proj_file(".github/CONTRIBUTING.md")
  expect_proj_file(".github/ISSUE_TEMPLATE/issue_template.md")
  expect_proj_file(".github/SUPPORT.md")
  expect_proj_file(".github/CODE_OF_CONDUCT.md")
  expect_true(is_build_ignored("^\\.github$"))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-tutorial.R ---
test_that("use_tutorial() checks its inputs", {
  skip_if_not_installed("rmarkdown")

  create_local_package()
  expect_snapshot(use_tutorial(), error = TRUE)
  expect_snapshot(use_tutorial(name = "tutorial-file"), error = TRUE)
})

test_that("use_tutorial() creates a tutorial", {
  skip_if_not_installed("rmarkdown")

  create_local_package()
  local_check_installed()

  use_tutorial(name = "aaa", title = "bbb")

  tute_file <- path("inst", "tutorials", "aaa", "aaa", ext = "Rmd")
  expect_proj_file(tute_file)
  expect_equal(rmarkdown::yaml_front_matter(tute_file)$title, "bbb")
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-ui-legacy.R ---
test_that("basic legacy UI actions behave as expected", {
  # suppress test silencing
  withr::local_options(list(usethis.quiet = FALSE))

  expect_snapshot({
    ui_line("line")
    ui_todo("to do")
    ui_done("done")
    ui_oops("oops")
    ui_info("info")
    ui_code_block(c("x <- 1", "y <- 2"))
    ui_warn("a warning")
  })
})

test_that("legacy UI actions respect usethis.quiet = TRUE", {
  withr::local_options(list(usethis.quiet = TRUE))

  expect_no_message({
    ui_line("line")
    ui_todo("to do")
    ui_done("done")
    ui_oops("oops")
    ui_info("info")
    ui_code_block(c("x <- 1", "y <- 2"))
  })
})

test_that("ui_stop() works", {
  expect_usethis_error(ui_stop("an error"), "an error")
})

test_that("ui_silence() suppresses output", {
  # suppress test silencing
  withr::local_options(list(usethis.quiet = FALSE))

  expect_output(ui_silence(ui_line()), NA)
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-upkeep.R ---
test_that("tidy upkeep bullets don't change accidentally", {
  create_local_package()
  use_mit_license()
  expect_equal(last_upkeep_year(), 2000L)

  local_mocked_bindings(
    Sys.Date = function() as.Date("2025-01-01"),
    usethis_version = function() "1.1.0",
    author_has_rstudio_email = function() TRUE,
    is_posit_pkg = function() TRUE,
    is_posit_person_canonical = function() FALSE
  )

  expect_snapshot(writeLines(tidy_upkeep_checklist()))
})

test_that("tidy upkeep omits bullets present in last_upkeep", {
  create_local_package()
  use_mit_license()
  expect_equal(last_upkeep_year(), 2000L)
  record_upkeep_date(as.Date("2023-04-04"))
  expect_equal(last_upkeep_year(), 2023L)

  local_mocked_bindings(
    Sys.Date = function() as.Date("2025-01-01"),
    usethis_version = function() "1.1.0",
    author_has_rstudio_email = function() TRUE,
    is_posit_pkg = function() TRUE,
    is_posit_person_canonical = function() FALSE
  )

  expect_snapshot(writeLines(tidy_upkeep_checklist()))
})

test_that("upkeep bullets don't change accidentally", {
  skip_if_no_git_user()

  create_local_package()

  local_mocked_bindings(
    Sys.Date = function() as.Date("2023-01-01"),
    usethis_version = function() "1.1.0",
    git_default_branch = function() "main"
  )

  expect_snapshot(writeLines(upkeep_checklist()))

  # Test some conditional TODOs
  use_code_of_conduct("jane.doe@foofymail.com")
  writeLines("# test environment\n", "cran-comments.md")
  local_mocked_bindings(git_default_branch = function() "master")

  # Look like a package that hasn't switched to testthat 3e yet
  use_testthat()
  desc::desc_del("Config/testthat/edition")
  desc::desc_del("Suggests")
  use_package("testthat", "Suggests")

  # previously (withr 2.5.0) we could put local_edition(2L) inside {..} inside
  # the expect_snapshot() call
  # that is no longer true with withr 3.0.0, but this hacktastic approach works
  local({
    local_edition(2L)
    checklist <<- upkeep_checklist()
  })

  expect_snapshot(writeLines(checklist))
})

test_that("get extra upkeep bullets works", {
  e <- new.env(parent = empty_env())
  expect_equal(upkeep_extra_bullets(e), "")

  e$upkeep_bullets <- function() c("extra", "upkeep bullets")
  expect_equal(
    upkeep_extra_bullets(e),
    c("* [ ] extra", "* [ ] upkeep bullets", "")
  )
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-use_github_file.R ---
test_that("parse_file_url() works when it should", {
  expected <- list(
    parsed = TRUE,
    repo_spec = "OWNER/REPO",
    path = "path/to/some/file",
    ref = "REF",
    host = "https://github.com"
  )
  expect_equal(
    parse_file_url("https://github.com/OWNER/REPO/blob/REF/path/to/some/file"),
    expected
  )
  expect_equal(
    parse_file_url(
      "https://raw.githubusercontent.com/OWNER/REPO/REF/path/to/some/file"
    ),
    expected
  )

  expected$path <- "file"
  expect_equal(
    parse_file_url("https://github.com/OWNER/REPO/blob/REF/file"),
    expected
  )
  expect_equal(
    parse_file_url("https://github.com/OWNER/REPO/blob/REF/file"),
    parse_file_url("https://raw.githubusercontent.com/OWNER/REPO/REF/file")
  )

  expected$host <- "https://github.acme.com"
  expect_equal(
    parse_file_url("https://github.acme.com/OWNER/REPO/blob/REF/file"),
    expected
  )
  expect_equal(
    parse_file_url("https://raw.github.acme.com/OWNER/REPO/REF/file"),
    expected
  )
})

test_that("parse_file_url() gives up when it should", {
  out <- parse_file_url("OWNER/REPO")
  expect_false(out$parsed)
})

test_that("parse_file_url() errors when it should", {
  expect_usethis_error(parse_file_url("https://github.com/OWNER/REPO"))
  expect_usethis_error(parse_file_url("https://github.com/OWNER/REPO.git"))
  expect_usethis_error(parse_file_url(
    "https://github.com/OWNER/REPO/commit/abcdefg"
  ))
  expect_usethis_error(parse_file_url(
    "https://github.com/OWNER/REPO/releases/tag/vx.y.z"
  ))
  expect_usethis_error(parse_file_url(
    "https://github.com/OWNER/REPO/tree/BRANCH"
  ))
  expect_usethis_error(parse_file_url(
    "https://gitlab.com/OWNER/REPO/path/to/file"
  ))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-use_import_from.R ---
test_that("use_import_from() imports the related package & adds line to package doc", {
  create_local_package()
  use_package_doc()
  use_import_from("lifecycle", "deprecated")

  expect_equal(proj_desc()$get_field("Imports"), "lifecycle")
  expect_equal(roxygen_ns_show(), "#' @importFrom lifecycle deprecated")
})

test_that("use_import_from() adds one line for each function", {
  create_local_package()
  use_package_doc()
  use_import_from("lifecycle", c("deprecate_warn", "deprecate_stop"))

  expect_snapshot(roxygen_ns_show())
})

test_that("use_import_from() generates helpful errors", {
  create_local_package()
  use_package_doc()

  expect_snapshot(error = TRUE, {
    use_import_from(1)
    use_import_from(c("desc", "rlang"))

    use_import_from("desc", "pool_noodle")
  })
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-use_standalone.R ---
test_that("standalone_header() works with various inputs", {
  expect_snapshot(
    standalone_header("OWNER/REPO", "R/standalone-foo.R")
  )
  expect_snapshot(
    standalone_header("OWNER/REPO", "R/standalone-foo.R", ref = "blah")
  )
  expect_snapshot(
    standalone_header(
      "OWNER/REPO",
      "R/standalone-foo.R",
      host = "https://github.com"
    )
  )
  expect_snapshot(
    standalone_header(
      "OWNER/REPO",
      "R/standalone-foo.R",
      host = "https://github.acme.com"
    )
  )
  expect_snapshot(
    standalone_header(
      "OWNER/REPO",
      "R/standalone-foo.R",
      ref = "blah",
      host = "https://github.com"
    )
  )
  expect_snapshot(
    standalone_header(
      "OWNER/REPO",
      "R/standalone-foo.R",
      ref = "blah",
      host = "https://github.acme.com"
    )
  )
})

test_that("can import standalone file with dependencies", {
  skip_if_offline("github.com")
  create_local_package()

  # NOTE: Check ref after r-lib/rlang@standalone-dep has been merged
  use_standalone("r-lib/rlang", "types-check", ref = "73182fe94")
  expect_setequal(
    as.character(path_rel(dir_ls(proj_path("R"))), proj_path()),
    c("R/import-standalone-types-check.R", "R/import-standalone-obj-type.R")
  )

  desc <- proj_desc()
  imports <- proj_desc()$get_field("Imports")
  expect_length(imports, 1)
  expect_match(imports, "rlang")
})

test_that("can use full github url", {
  skip_if_offline("github.com")
  create_local_package()

  use_standalone(
    "https://github.com/r-lib/rlang",
    file = "sizes",
    ref = "4670cb233ecc8d11"
  )
  expect_equal(
    as.character(path_rel(dir_ls(proj_path("R"))), proj_path()),
    "R/import-standalone-sizes.R"
  )
})


test_that("can offer choices", {
  skip_if_offline("github.com")

  expect_snapshot(error = TRUE, {
    standalone_choose("tidyverse/forcats", ref = "v1.0.0")
    standalone_choose("r-lib/rlang", ref = "4670cb233ecc8d11")
  })
})

test_that("can extract dependencies", {
  extract_deps <- function(deps) {
    out <- standalone_dependencies(c("# ---", deps, "# ---"), "test.R")
    out$deps
  }

  expect_equal(extract_deps(NULL), character())
  expect_equal(extract_deps("# dependencies: a"), "a")
  expect_equal(extract_deps("# dependencies: [a, b]"), c("a", "b"))
})

test_that("can extract imports", {
  extract_imports <- function(imports) {
    out <- standalone_dependencies(
      c("# ---", imports, "# ---"),
      "test.R",
      error_call = current_env()
    )
    out$imports
  }

  expect_equal(
    extract_imports(NULL),
    version_info_df()
  )

  expect_equal(
    extract_imports("# imports: rlang"),
    version_info_df("rlang", NA, NA)
  )

  expect_equal(
    extract_imports("# imports: rlang (>= 1.0.0)"),
    version_info_df("rlang", ">=", "1.0.0")
  )

  expect_equal(
    extract_imports("# imports: [rlang (>= 1.0.0), purrr]"),
    version_info_df(c("rlang", "purrr"), c(">=", NA), c("1.0.0", NA))
  )

  expect_snapshot(error = TRUE, {
    extract_imports("# imports: rlang (== 1.0.0)")
    extract_imports("# imports: rlang (>= 1.0.0), purrr")
    extract_imports("# imports: foo (>=0.0.0)")
  })
})

test_that("errors on malformed dependencies", {
  expect_snapshot(error = TRUE, {
    standalone_dependencies(c(), "test.R")
    standalone_dependencies(c("# ---", "# dependencies: 1", "# ---"), "test.R")
  })
})

test_that("standalone file is normalised", {
  expect_equal(as_standalone_file("foo"), "standalone-foo.R")
  expect_equal(as_standalone_file("standalone-foo"), "standalone-foo.R")
  expect_equal(as_standalone_file("standalone-foo.R"), "standalone-foo.R")
  expect_equal(as_standalone_file("aaa-standalone-foo"), "aaa-standalone-foo.R")
  expect_equal(
    as_standalone_file("aaa-standalone-foo.R"),
    "aaa-standalone-foo.R"
  )
})

test_that("standalone destination file is normalised", {
  expect_equal(
    as_standalone_dest_file("standalone-foo.R"),
    "import-standalone-foo.R"
  )
  expect_equal(
    as_standalone_dest_file("aaa-standalone-foo.R"),
    "aaa-import-standalone-foo.R"
  )
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-usethis-deprecated.R ---
test_that("use_tidy_style() is deprecated", {
  expect_snapshot(use_tidy_style())
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-utils-gh.R ---
test_that("is_github_enterprise() identifies GitHub Enterprise URLs (or not)", {
  # https://github.com/r-lib/usethis/pull/2098
  expect_true(is_github_enterprise("https://my-cool-org.ghe.com"))

  # not handled yet: self-hosted GHE server
  # https://github.com/r-lib/usethis/pull/2098
  expect_false(is_github_enterprise(
    "https://ghe-gsk-prod.metworx.com/account/reponame"
  ))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-utils-git.R ---
# Branch ------------------------------------------------------------------
test_that("git_branch() works", {
  skip_if_no_git_user()
  create_local_project()

  expect_usethis_error(git_branch(), "Cannot detect")

  git_init()
  expect_usethis_error(git_branch(), "unborn branch")

  writeLines("blah", proj_path("blah.txt"))
  gert::git_add("blah.txt", repo = git_repo())
  gert::git_commit("Make one commit", repo = git_repo())
  # branch name can depend on user's config, e.g. could be 'master' or 'main'
  expect_no_error(
    b <- git_branch()
  )
  expect_true(nzchar(b))
})

# Protocol ------------------------------------------------------------------
test_that("git_protocol() catches bad input from usethis.protocol option", {
  withr::with_options(
    list(usethis.protocol = "nope"),
    {
      expect_usethis_error(git_protocol(), "must be either")
      expect_null(getOption("usethis.protocol"))
    }
  )
  withr::with_options(
    list(usethis.protocol = c("ssh", "https")),
    {
      expect_usethis_error(git_protocol(), "must be either")
      expect_null(getOption("usethis.protocol"))
    }
  )
})

test_that("use_git_protocol() errors for bad input", {
  expect_usethis_error(use_git_protocol("nope"), "must be either")
})

test_that("git_protocol() defaults to 'https'", {
  withr::with_options(
    list(usethis.protocol = NULL),
    expect_identical(git_protocol(), "https")
  )
})

test_that("git_protocol() honors, vets, and lowercases the option", {
  withr::with_options(
    list(usethis.protocol = "ssh"),
    expect_identical(git_protocol(), "ssh")
  )
  withr::with_options(
    list(usethis.protocol = "SSH"),
    expect_identical(git_protocol(), "ssh")
  )
  withr::with_options(
    list(usethis.protocol = "https"),
    expect_identical(git_protocol(), "https")
  )
  withr::with_options(
    list(usethis.protocol = "nope"),
    expect_usethis_error(git_protocol(), "must be either")
  )
})

test_that("use_git_protocol() prioritizes and lowercases direct input", {
  withr::with_options(
    list(usethis.protocol = "ssh"),
    {
      expect_identical(use_git_protocol("HTTPS"), "https")
      expect_identical(git_protocol(), "https")
    }
  )
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-utils-github.R ---
test_that("parse_github_remotes() works, on named list or named character", {
  urls <- list(
    https = "https://github.com/OWNER/REPO.git",
    ghe = "https://github.acme.com/OWNER/REPO.git",
    browser = "https://github.com/OWNER/REPO",
    ssh1 = "git@github.com:OWNER/REPO.git",
    ssh2 = "ssh://git@github.com/OWNER/REPO.git",
    gitlab1 = "https://gitlab.com/OWNER/REPO.git",
    gitlab2 = "git@gitlab.com:OWNER/REPO.git",
    bitbucket1 = "https://bitbucket.org/OWNER/REPO.git",
    bitbucket2 = "git@bitbucket.org:OWNER/REPO.git"
  )
  parsed <- parse_github_remotes(urls)
  expect_equal(parsed$name, names(urls))
  expect_equal(unique(parsed$repo_owner), "OWNER")
  expect_equal(
    parsed$host,
    c(
      "github.com",
      "github.acme.com",
      "github.com",
      "github.com",
      "github.com",
      "gitlab.com",
      "gitlab.com",
      "bitbucket.org",
      "bitbucket.org"
    )
  )
  expect_equal(unique(parsed$repo_name), "REPO")
  expect_equal(
    parsed$protocol,
    c("https", "https", "https", "ssh", "ssh", "https", "ssh", "https", "ssh")
  )

  parsed2 <- parse_github_remotes(unlist(urls))
  expect_equal(parsed, parsed2)
})

test_that("parse_github_remotes() works on edge cases", {
  parsed <- parse_github_remotes("https://github.com/HenrikBengtsson/R.rsp")
  expect_equal(parsed$repo_owner, "HenrikBengtsson")
  expect_equal(parsed$repo_name, "R.rsp")
})

test_that("parse_github_remotes() works for length zero input", {
  expect_no_error(
    parsed <- parse_github_remotes(character())
  )
  expect_equal(nrow(parsed), 0)
  expect_setequal(
    names(parsed),
    c("name", "url", "host", "repo_owner", "repo_name", "protocol")
  )
})

test_that("parse_repo_url() passes a naked repo spec through", {
  out <- parse_repo_url("OWNER/REPO")
  expect_equal(
    out,
    list(repo_spec = "OWNER/REPO", host = NULL)
  )
})

test_that("parse_repo_url() handles GitHub remote URLs", {
  urls <- list(
    https = "https://github.com/OWNER/REPO.git",
    ghe = "https://github.acme.com/OWNER/REPO.git",
    browser = "https://github.com/OWNER/REPO",
    ssh = "git@github.com:OWNER/REPO.git"
  )
  out <- map(urls, parse_repo_url)
  expect_match(map_chr(out, "repo_spec"), "OWNER/REPO", fixed = TRUE)
  out_host <- map_chr(out, "host")
  expect_match(
    out_host[c("https", "browser", "ssh")],
    "https://github.com",
    fixed = TRUE
  )
  expect_equal(out_host[["ghe"]], "https://github.acme.com")
})

test_that("parse_repo_url() errors for non-GitHub remote URLs", {
  urls <- list(
    gitlab1 = "https://gitlab.com/OWNER/REPO.git",
    gitlab2 = "git@gitlab.com:OWNER/REPO.git",
    bitbucket1 = "https://bitbucket.org/OWNER/REPO.git",
    bitbucket2 = "git@bitbucket.org:OWNER/REPO.git"
  )
  safely_parse_repo_url <- purrr::safely(parse_repo_url)
  out <- map(urls, safely_parse_repo_url)
  out_result <- map(out, "result")
  expect_true(all(map_lgl(out_result, is.null)))
})

test_that("github_remote_list() works", {
  local_interactive(FALSE)
  create_local_project()
  use_git()
  use_git_remote("origin", "https://github.com/OWNER/REPO.git")
  use_git_remote("upstream", "https://github.com/THEM/REPO.git")
  use_git_remote("foofy", "https://github.com/OTHERS/REPO.git")
  use_git_remote("gitlab", "https://gitlab.com/OTHERS/REPO.git")
  use_git_remote("bitbucket", "git@bitbucket.org:OWNER/REPO.git")

  grl <- github_remote_list()
  expect_setequal(grl$remote, c("origin", "upstream"))
  expect_setequal(grl$repo_spec, c("OWNER/REPO", "THEM/REPO"))

  grl <- github_remote_list(c("upstream", "foofy"))
  expect_setequal(grl$remote, c("upstream", "foofy"))
  nms <- names(grl)

  grl <- github_remote_list(c("gitlab", "bitbucket"))
  expect_equal(nrow(grl), 0)
  expect_named(grl, nms)
})

test_that("github_remotes(), github_remote_list() accept explicit 0-row input", {
  x <- data.frame(
    name = character(),
    url = character(),
    stringsAsFactors = FALSE
  )
  grl <- github_remote_list(x = x)
  expect_equal(nrow(grl), 0)
  expect_true(all(map_lgl(grl, is.character)))

  gr <- github_remotes(x = x)
  expect_equal(nrow(grl), 0)
})

test_that("github_remotes() works", {
  skip_if_offline("github.com")
  skip_if_no_git_user()

  create_local_project()
  use_git()

  # no git remotes = 0-row edge case
  expect_no_error(
    grl <- github_remotes()
  )

  # a public remote = no token necessary to get github info
  use_git_remote("origin", "https://github.com/r-lib/usethis.git")
  expect_no_error(
    grl <- github_remotes()
  )
  expect_false(grl$is_fork)
  expect_true(is.na(grl$parent_repo_owner))

  # no git remote by this name = 0-row edge case
  expect_no_error(
    grl <- github_remotes("foofy")
  )

  # gh::gh() call should fail, so we should get no info from github
  use_git_remote(
    "origin",
    "https://github.com/r-lib/DOESNOTEXIST.git",
    overwrite = TRUE
  )
  expect_no_error(
    grl <- github_remotes()
  )
  expect_true(is.na(grl$is_fork))
})

test_that("github_url_from_git_remotes() is idempotent", {
  url <- "https://github.com/r-lib/usethis.git"
  out <- github_url_from_git_remotes(url)
  expect_equal(out, github_url_from_git_remotes(out))
})

# GitHub remote configuration --------------------------------------------------

test_that("we understand the list of all possible configs", {
  expect_snapshot(all_configs())
})

test_that("'no_github' is reported correctly", {
  expect_snapshot(new_no_github())
})

test_that("'ours' is reported correctly", {
  expect_snapshot(new_ours())
})

test_that("'theirs' is reported correctly", {
  expect_snapshot(new_theirs())
})

test_that("'fork' is reported correctly", {
  expect_snapshot(new_fork())
})

test_that("'maybe_ours_or_theirs' is reported correctly", {
  expect_snapshot(new_maybe_ours_or_theirs())
})

test_that("'maybe_fork' is reported correctly", {
  expect_snapshot(new_maybe_fork())
})

test_that("'fork_cannot_push_origin' is reported correctly", {
  expect_snapshot(new_fork_cannot_push_origin())
})

test_that("'fork_upstream_is_not_origin_parent' is reported correctly", {
  expect_snapshot(new_fork_upstream_is_not_origin_parent())
})

test_that("'upstream_but_origin_is_not_fork' is reported correctly", {
  expect_snapshot(new_upstream_but_origin_is_not_fork())
})

test_that("'fork_upstream_is_not_origin_parent' is detected correctly", {
  # inspired by something that actually happened:
  # 1. r-pkgs/gh is created
  # 2. user forks and clones: origin = USER/gh, upstream = r-pkgs/gh
  # 3. parent repo becomes r-lib/gh, due to transfer or ownership or owner
  #    name change
  # Now upstream looks like it does not point to fork parent.
  local_interactive(FALSE)
  create_local_project()
  use_git()
  use_git_remote("origin", "https://github.com/jennybc/gh.git")
  use_git_remote("upstream", "https://github.com/r-pkgs/gh.git")
  gr <- github_remotes(github_get = FALSE)
  gr$github_got <- TRUE
  gr$is_fork <- c(TRUE, FALSE)
  gr$can_push <- TRUE
  gr$perm_known <- TRUE
  gr$parent_repo_owner <- c("r-lib", NA)
  gr$parent_repo_name <- c("gh", NA)
  gr$parent_repo_spec <- c("r-lib/gh", NA)
  local_mocked_bindings(github_remotes = function(...) gr)
  cfg <- github_remote_config()
  expect_equal(cfg$type, "fork_upstream_is_not_origin_parent")
  expect_snapshot(error = TRUE, stop_bad_github_remote_config(cfg))
})

test_that("bad github config error", {
  expect_snapshot(
    error = TRUE,
    stop_bad_github_remote_config(new_fork_upstream_is_not_origin_parent())
  )
})

test_that("maybe bad github config error", {
  expect_snapshot(
    error = TRUE,
    stop_maybe_github_remote_config(new_maybe_fork())
  )
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-utils-glue.R ---
test_that("glue_chr() returns plain character, evals in correct env", {
  x <- letters[1:2]
  y <- LETTERS[25:26]
  f <- toupper
  expect_identical(glue_chr("{f(x)}-{y}"), c("A-Y", "B-Z"))
})

test_that("glue_data_chr() returns plain character, evals in correct env", {
  z <- list(x = letters[1:2], y = LETTERS[25:26])
  f <- tolower
  x <- 1
  y <- 2
  expect_identical(glue_data_chr(z, "{x}-{f(y)}"), c("a-y", "b-z"))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-utils-ui.R ---
cli::test_that_cli("ui_bullets() look as expected", {
  # suppress test silencing
  withr::local_options(list(usethis.quiet = FALSE))

  expect_snapshot(
    ui_bullets(c(
      # relate to legacy functions
      "_" = "todo", # ui_todo()
      "v" = "done", # ui_done()
      "x" = "oops", # ui_oops()
      "i" = "info", # ui_info()
      "noindent", # ui_line()

      # other cli bullets that have no special connection to usethis history
      " " = "indent",
      "*" = "bullet",
      ">" = "arrow",
      "!" = "warning"
    ))
  )
})

test_that("ui_bullets() respect usethis.quiet = TRUE", {
  withr::local_options(list(usethis.quiet = TRUE))

  expect_no_message(
    ui_bullets(c(
      # relate to legacy functions
      "_" = "todo", # ui_todo()
      "v" = "done", # ui_done()
      "x" = "oops", # ui_oops()
      "i" = "info", # ui_info()
      "noindent", # ui_line()

      # other cli bullets that have no special connection to usethis history
      " " = "indent",
      "*" = "bullet",
      ">" = "arrow",
      "!" = "warning"
    ))
  )
})

cli::test_that_cli("ui_bullets() does glue interpolation and inline markup", {
  # suppress test silencing
  withr::local_options(list(usethis.quiet = FALSE))

  x <- "world"

  expect_snapshot(
    ui_bullets(c(
      "i" = "Hello, {x}!",
      "v" = "Updated the {.field BugReports} field",
      "x" = "Scary {.code code} or {.fun function}"
    ))
  )
})

test_that("trailing slash behaviour of ui_path_impl()", {
  # target doesn't exist so no empirical evidence that it's a directory
  expect_match(ui_path_impl("abc"), "abc$")

  # path suggests it's a directory
  expect_match(ui_path_impl("abc/"), "abc/$")
  expect_match(ui_path_impl("abc//"), "abc/$")

  # path is known to be a directory
  tmpdir <- withr::local_tempdir(pattern = "ui_path_impl")

  expect_match(ui_path_impl(tmpdir), "/$")
  expect_match(ui_path_impl(paste0(tmpdir, "/")), "[^/]/$")
  expect_match(ui_path_impl(paste0(tmpdir, "//")), "[^/]/$")
})

test_that("ui_abort() works", {
  expect_usethis_error(ui_abort("spatula"), "spatula")

  # usethis.quiet should have no effect on this
  withr::local_options(list(usethis.quiet = TRUE))
  expect_usethis_error(ui_abort("whisk"), "whisk")
})

test_that("ui_abort() defaults to 'x' for first bullet", {
  expect_snapshot(error = TRUE, ui_abort("no explicit bullet"))
})

test_that("ui_abort() can take explicit first bullet", {
  expect_snapshot(error = TRUE, ui_abort(c("v" = "success bullet")))
})

test_that("ui_abort() defaults to 'i' for non-first bullet", {
  expect_snapshot(
    error = TRUE,
    ui_abort(c(
      "oops",
      " " = "space bullet",
      "info bullet",
      "v" = "success bullet"
    ))
  )
})

cli::test_that_cli(
  "ui_code_snippet() with scalar input",
  {
    withr::local_options(list(usethis.quiet = FALSE))

    expect_snapshot(
      ui_code_snippet(
        "
      options(
        warnPartialMatchArgs = TRUE,
        warnPartialMatchDollar = TRUE,
        warnPartialMatchAttr = TRUE
      )"
      )
    )
  },
  configs = c("plain", "ansi")
)

cli::test_that_cli(
  "ui_code_snippet() with vector input",
  {
    withr::local_options(list(usethis.quiet = FALSE))

    expect_snapshot(
      ui_code_snippet(c(
        "options(",
        "  warnPartialMatchArgs = TRUE,",
        "  warnPartialMatchDollar = TRUE,",
        "  warnPartialMatchAttr = TRUE",
        ")"
      ))
    )
  },
  configs = c("plain", "ansi")
)

cli::test_that_cli(
  "ui_code_snippet() when language is not R",
  {
    withr::local_options(list(usethis.quiet = FALSE))
    h <- "blah.h"
    expect_snapshot(
      ui_code_snippet("#include <{h}>", language = "")
    )
  },
  configs = c("plain", "ansi")
)

cli::test_that_cli(
  "ui_code_snippet() can interpolate",
  {
    withr::local_options(list(usethis.quiet = FALSE))

    true_val <- "TRUE"
    false_val <- "'FALSE'"

    expect_snapshot(
      ui_code_snippet("if (1) {true_val} else {false_val}")
    )
  },
  configs = c("plain", "ansi")
)

cli::test_that_cli(
  "ui_code_snippet() can NOT interpolate",
  {
    withr::local_options(list(usethis.quiet = FALSE))
    expect_snapshot({
      ui_code_snippet(
        "foo <- function(x){x}",
        interpolate = FALSE
      )
      ui_code_snippet(
        "foo <- function(x){{x}}",
        interpolate = TRUE
      )
    })
  },
  configs = c("plain", "ansi")
)

test_that("bulletize() works", {
  withr::local_options(list(usethis.quiet = FALSE))
  expect_snapshot(ui_bullets(bulletize(letters)))
  expect_snapshot(ui_bullets(bulletize(letters, bullet = "x")))
  expect_snapshot(ui_bullets(bulletize(letters, n_show = 2)))
  expect_snapshot(ui_bullets(bulletize(letters[1:6])))
  expect_snapshot(ui_bullets(bulletize(letters[1:7])))
  expect_snapshot(ui_bullets(bulletize(letters[1:8])))
  expect_snapshot(ui_bullets(bulletize(letters[1:6], n_fudge = 0)))
  expect_snapshot(ui_bullets(bulletize(letters[1:8], n_fudge = 3)))
})

test_that("usethis_map_cli() works", {
  x <- c("aaa", "bbb", "ccc")
  expect_equal(
    usethis_map_cli(x, template = "{.file <<x>>}"),
    c("{.file aaa}", "{.file bbb}", "{.file ccc}")
  )
})

cli::test_that_cli(
  "ui_special() works",
  {
    expect_snapshot(cli::cli_text(ui_special()))
    expect_snapshot(cli::cli_text(ui_special("whatever")))
  },
  configs = c("plain", "ansi")
)

cli::test_that_cli(
  "kv_line() looks as expected in basic use",
  {
    withr::local_options(list(usethis.quiet = FALSE))

    expect_snapshot({
      kv_line("CHARACTER", "VALUE")
      kv_line("NUMBER", 1)
      kv_line("LOGICAL", TRUE)
    })
  },
  configs = c("plain", "fancy")
)

cli::test_that_cli(
  "kv_line() can interpolate and style inline in key",
  {
    withr::local_options(list(usethis.quiet = FALSE))

    field <- "SOME_FIELD"
    expect_snapshot(
      kv_line("Let's reveal {.field {field}}", "whatever")
    )
  },
  configs = c("plain", "fancy")
)

cli::test_that_cli(
  "kv_line() can treat value in different ways",
  {
    withr::local_options(list(usethis.quiet = FALSE))

    value <- "some value"
    adjective <- "great"
    url <- "https://usethis.r-lib.org/"

    expect_snapshot({
      # evaluation in .envir
      kv_line("Key", value)

      # NULL is special
      kv_line("Something we don't have", NULL)
      # explicit special
      kv_line("Key", ui_special("discovered"))

      # value taken at face value
      kv_line("Key", "something {.emph important}")

      # I() indicates value has markup
      kv_line("Key", I("something {.emph important}"))
      kv_line("Key", I("something {.emph {adjective}}"))
      kv_line("Interesting file", I("{.url {url}}"))
    })
  },
  configs = c("plain", "fancy")
)

test_that("ui_escape_glue() doubles curly braces", {
  expect_equal(ui_escape_glue("no braces"), "no braces")
  expect_equal(ui_escape_glue("one { brace"), "one {{ brace")
  expect_equal(ui_escape_glue("one } brace"), "one }} brace")
  expect_equal(
    ui_escape_glue("A {brace_set} in text"),
    "A {{brace_set}} in text"
  )
  expect_equal(
    ui_escape_glue("{multiple} {brace} {sets}"),
    "{{multiple}} {{brace}} {{sets}}"
  )
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-utils.R ---
test_that("check_is_named_list() works", {
  l <- list(a = "a", b = 2, c = letters)
  expect_identical(l, check_is_named_list(l))

  user_facing_function <- function(somevar) {
    check_is_named_list(somevar)
  }

  expect_snapshot(error = TRUE, user_facing_function(NULL))
  expect_snapshot(error = TRUE, user_facing_function(c(a = "a", b = "b")))
  expect_snapshot(error = TRUE, user_facing_function(list("a", b = 2)))
})

test_that("asciify() substitutes non-ASCII but respects case", {
  expect_identical(asciify("aB!d$F+_h"), "aB-d-F-_h")
})

test_that("path_first_existing() works", {
  create_local_project()

  all_3_files <- proj_path(c("alfa", "bravo", "charlie"))

  expect_null(path_first_existing(all_3_files))

  write_utf8(proj_path("charlie"), "charlie")
  expect_equal(path_first_existing(all_3_files), proj_path("charlie"))

  write_utf8(proj_path("bravo"), "bravo")
  expect_equal(path_first_existing(all_3_files), proj_path("bravo"))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-version.R ---
test_that("bump_version() presents all possible incremented versions", {
  expect_identical(
    bump_version("1.1.1.9000"),
    c(major = "2.0.0", minor = "1.2.0", patch = "1.1.2", dev = "1.1.1.9001")
  )
})

test_that("use_version() and use_dev_version() require a package", {
  create_local_project()
  expect_usethis_error(use_version("major"), "not an R package")
  expect_usethis_error(use_dev_version(), "not an R package")
})

test_that("use_version() errors for invalid `which`", {
  create_local_package()
  expect_snapshot(error = TRUE, use_version("1.2.3"))
})

test_that("use_version() increments version in DESCRIPTION, edits NEWS", {
  create_local_package()
  proj_desc_field_update(
    key = "Version",
    value = "1.1.1.9000",
    overwrite = TRUE
  )
  local_cran_version("1.1.1")
  use_news_md()

  use_version("major")
  expect_identical(proj_version(), "2.0.0")

  expect_snapshot(
    writeLines(read_utf8(proj_path("NEWS.md"))),
    transform = scrub_testpkg
  )
})

test_that("use_dev_version() appends .9000 to Version, exactly once", {
  create_local_package()
  proj_desc_field_update(key = "Version", value = "0.0.1", overwrite = TRUE)
  use_dev_version()
  expect_identical(proj_version(), "0.0.1.9000")
  use_dev_version()
  expect_identical(proj_version(), "0.0.1.9000")
})

test_that("use_version() updates (development version) directly", {
  create_local_package()
  proj_desc_field_update(key = "Version", value = "0.0.1", overwrite = TRUE)
  local_cran_version("0.0.1")
  use_news_md()

  # bump to dev to set (development version)
  use_dev_version()

  # directly overwrite development header
  use_version("patch")

  expect_snapshot(
    writeLines(read_utf8(proj_path("NEWS.md"))),
    transform = scrub_testpkg
  )
})

test_that("use_version() updates version.c", {
  create_local_package()
  proj_desc_field_update(key = "Version", value = "1.0.0", overwrite = TRUE)

  name <- project_name()
  src_path <- proj_path("src")
  ver_path <- path(src_path, "version.c")
  dir_create(src_path)

  write_utf8(
    ver_path,
    glue(
      '
    foo;
    const char {name}_version = "1.0.0";
    bar;'
    )
  )

  use_dev_version()

  lines <- read_utf8(ver_path)
  expect_snapshot(writeLines(lines), transform = scrub_testpkg)
})

test_that("is_dev_version() detects dev version directly and with DESCRIPTION", {
  expect_true(is_dev_version("0.0.1.9000"))
  expect_false(is_dev_version("0.0.1"))

  create_local_package()
  proj_desc_field_update(key = "Version", value = "1.0.0", overwrite = TRUE)
  expect_false(is_dev_version())
  use_dev_version()
  expect_true(is_dev_version())
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-vignette.R ---
# use_vignette ------------------------------------------------------------

test_that("use_vignette() requires a package", {
  create_local_project()

  expect_usethis_error(use_vignette(), "not an R package")
})

test_that("use_vignette() gives useful errors", {
  create_local_package()

  expect_snapshot(error = TRUE, {
    use_vignette()
    use_vignette("bad name")
  })
})

test_that("use_vignette() does the promised setup, Rmd", {
  create_local_package()

  use_vignette("name", "title")
  expect_proj_file("vignettes/name.Rmd")

  ignores <- read_utf8(proj_path(".gitignore"))
  expect_true("inst/doc" %in% ignores)

  deps <- proj_deps()
  expect_true(
    all(c("knitr", "rmarkdown") %in% deps$package[deps$type == "Suggests"])
  )

  expect_identical(proj_desc()$get_field("VignetteBuilder"), "knitr")
})

test_that("use_vignette() does the promised setup, qmd", {
  create_local_package()
  local_check_installed()

  use_vignette("name.qmd", "title")
  expect_proj_file("vignettes/name.qmd")

  ignores <- read_utf8(proj_path(".gitignore"))
  expect_true("inst/doc" %in% ignores)

  deps <- proj_deps()
  expect_true(
    all(c("knitr", "quarto") %in% deps$package[deps$type == "Suggests"])
  )

  expect_identical(proj_desc()$get_field("VignetteBuilder"), "quarto")
})

test_that("use_vignette() does the promised setup, mix of Rmd and qmd", {
  create_local_package()
  local_check_installed()

  use_vignette("older-vignette", "older Rmd vignette")
  use_vignette("newer-vignette.qmd", "newer qmd vignette")
  expect_proj_file("vignettes/older-vignette.Rmd")
  expect_proj_file("vignettes/newer-vignette.qmd")

  deps <- proj_deps()
  expect_true(
    all(
      c("knitr", "quarto", "rmarkdown") %in%
        deps$package[deps$type == "Suggests"]
    )
  )

  vignette_builder <- proj_desc()$get_field("VignetteBuilder")
  expect_match(vignette_builder, "knitr", fixed = TRUE)
  expect_match(vignette_builder, "quarto", fixed = TRUE)
})

# use_article -------------------------------------------------------------
test_that("use_article() does the promised setup, Rmd", {
  create_local_package()
  local_interactive(FALSE)

  # Let's have another package already in Config/Needs/website
  proj_desc_field_update("Config/Needs/website", "somepackage")
  use_article("name", "title")

  expect_proj_file("vignettes/articles/name.Rmd")

  expect_setequal(
    proj_desc()$get_list("Config/Needs/website"),
    c("rmarkdown", "somepackage")
  )
})

# Note that qmd articles seem to cause problems for build_site() rn
# https://github.com/r-lib/pkgdown/issues/2821
test_that("use_article() does the promised setup, qmd", {
  create_local_package()
  local_check_installed()
  local_interactive(FALSE)

  # Let's have another package already in Config/Needs/website
  proj_desc_field_update("Config/Needs/website", "somepackage")
  use_article("name.qmd", "title")

  expect_proj_file("vignettes/articles/name.qmd")

  expect_setequal(
    proj_desc()$get_list("Config/Needs/website"),
    c("quarto", "somepackage")
  )
})

# helpers -----------------------------------------------------------------

test_that("valid_vignette_name() works", {
  expect_true(valid_vignette_name("perfectly-valid-name"))
  expect_false(valid_vignette_name("01-test"))
  expect_false(valid_vignette_name("test.1"))
})

test_that("we error informatively for bad vignette extension", {
  expect_snapshot(
    error = TRUE,
    check_vignette_extension("Rnw")
  )
})


# --- FILE: https://raw.githubusercontent.com/r-lib/usethis/main/tests/testthat/test-write.R ---
# test that write_utf8() does not alter active project and
# does not consult active project for line ending
test_that("write_utf8(): no active project, write path outside project", {
  local_project(NULL)
  expect_false(proj_active())
  dir <- withr::local_tempdir(pattern = "write-utf8-nonproject")
  expect_false(possibly_in_proj(dir))

  write_utf8(path(dir, "letters_LF"), letters[1:2], line_ending = "\n")
  expect_equal(
    readBin(path(dir, "letters_LF"), what = "raw", n = 3),
    charToRaw("a\nb")
  )
  write_utf8(path(dir, "letters_CRLF"), letters[1:2], line_ending = "\r\n")
  expect_equal(
    readBin(path(dir, "letters_CRLF"), what = "raw", n = 3),
    charToRaw("a\r\n")
  )

  expect_false(proj_active())
})

test_that("write_utf8(): no active project, write to path inside a project", {
  local_project(NULL)
  expect_false(proj_active())
  dir <- withr::local_tempdir(pattern = "write-utf8-in-a-project")
  file_create(path(dir, ".here"))
  expect_true(possibly_in_proj(dir))

  with_project(dir, use_rstudio(line_ending = "posix"))
  write_utf8(path(dir, "letters"), letters[1:2])
  expect_equal(
    readBin(path(dir, "letters"), what = "raw", n = 3),
    charToRaw("a\nb")
  )
  file_delete(path(dir, paste0(path_file(dir), ".Rproj")))

  with_project(dir, use_rstudio(line_ending = "windows"))
  write_utf8(path(dir, "letters"), letters[1:2])
  expect_equal(
    readBin(path(dir, "letters"), what = "raw", n = 3),
    charToRaw("a\r\n")
  )

  expect_false(proj_active())
})

test_that("write_utf8(): in an active project, write path outside project", {
  proj <- create_local_project(rstudio = TRUE)
  expect_true(proj_active())
  dir <- withr::local_tempdir(pattern = "write-utf8-nonproject")
  expect_false(possibly_in_proj(dir))

  write_utf8(path(dir, "letters_LF"), letters[1:2], line_ending = "\n")
  expect_equal(
    readBin(path(dir, "letters_LF"), what = "raw", n = 3),
    charToRaw("a\nb")
  )
  write_utf8(path(dir, "letters_CRLF"), letters[1:2], line_ending = "\r\n")
  expect_equal(
    readBin(path(dir, "letters_CRLF"), what = "raw", n = 3),
    charToRaw("a\r\n")
  )

  expect_equal(proj_get(), proj)
})

test_that("write_utf8(): in an active project, write path in other project", {
  proj <- create_local_project(rstudio = TRUE)
  expect_true(proj_active())
  dir <- withr::local_tempdir(pattern = "write-utf8-in-a-project")
  file_create(path(dir, ".here"))
  expect_true(possibly_in_proj(dir))

  with_project(dir, use_rstudio(line_ending = "posix"))
  write_utf8(path(dir, "letters"), letters[1:2])
  expect_equal(
    readBin(path(dir, "letters"), what = "raw", n = 3),
    charToRaw("a\nb")
  )
  file_delete(path(dir, paste0(path_file(dir), ".Rproj")))

  with_project(dir, use_rstudio(line_ending = "windows"))
  write_utf8(path(dir, "letters"), letters[1:2])
  expect_equal(
    readBin(path(dir, "letters"), what = "raw", n = 3),
    charToRaw("a\r\n")
  )

  expect_equal(proj_get(), proj)
})

test_that("write_utf8() can append text when requested", {
  path <- file_temp()
  write_utf8(path, "x", line_ending = "\n")
  write_utf8(path, "x", line_ending = "\n", append = TRUE)

  expect_equal(readChar(path, 4), "x\nx\n")
})

test_that("write_utf8() respects line ending", {
  path <- file_temp()

  write_utf8(path, "x", line_ending = "\n")
  expect_equal(detect_line_ending(path), "\n")

  write_utf8(path, "x", line_ending = "\r\n")
  expect_equal(detect_line_ending(path), "\r\n")
})

# TODO: explore more edge cases re: active project on both sides
test_that("write_utf8() can operate outside of a project", {
  dir <- withr::local_tempdir(pattern = "write-utf8-test")
  withr::local_dir(dir)
  local_project(NULL)

  expect_false(proj_active())
  expect_no_error(write_utf8(path = "foo", letters[1:3]))
})

# https://github.com/r-lib/usethis/issues/514
test_that("write_utf8() always produces a trailing newline", {
  path <- file_temp()
  write_utf8(path, "x", line_ending = "\n")
  expect_equal(readChar(path, 2), "x\n")
})

test_that("write_union() writes a de novo file", {
  tmp <- file_temp()
  expect_false(file_exists(tmp))
  write_union(tmp, letters[1:3], quiet = TRUE)
  expect_identical(read_utf8(tmp), letters[1:3])
})

test_that("write_union() leaves file 'as is'", {
  tmp <- file_temp()
  writeLines(letters[1:3], tmp)
  before <- read_utf8(tmp)
  write_union(tmp, "b", quiet = TRUE)
  expect_identical(before, read_utf8(tmp))
})

test_that("write_union() adds lines", {
  tmp <- file_temp()
  writeLines(letters[1:3], tmp)
  write_union(tmp, letters[4:5], quiet = TRUE)
  expect_setequal(read_utf8(tmp), letters[1:5])
})

# https://github.com/r-lib/usethis/issues/526
test_that("write_union() doesn't remove duplicated lines in the input", {
  tmp <- file_temp()
  before <- rep(letters[1:2], 3)
  add_me <- c("z", "a", "c", "a", "b")
  writeLines(before, tmp)
  expect_identical(before, read_utf8(tmp))
  write_union(tmp, add_me, quiet = TRUE)
  expect_identical(read_utf8(tmp), c(before, c("z", "c")))
})

test_that("same_contents() detects if contents are / are not same", {
  tmp <- file_temp()
  x <- letters[1:3]
  writeLines(x, con = tmp, sep = "\n")
  expect_true(same_contents(tmp, x))
  expect_false(same_contents(tmp, letters[4:6]))
})

test_that("write_over() leaves file 'as is' (outside of a project)", {
  local_interactive(FALSE)
  tmp <- withr::local_file(file_temp())

  writeLines(letters[1:3], tmp)

  before <- read_utf8(tmp)
  write_over(tmp, letters[4:6], quiet = TRUE)
  expect_identical(read_utf8(tmp), before)

  # usethis.overwrite shouldn't matter for a file outside of a project
  withr::with_options(
    list(usethis.overwrite = TRUE),
    {
      write_over(tmp, letters[4:6], quiet = TRUE)
      expect_identical(read_utf8(tmp), before)
    }
  )
})

test_that("write_over() works in active project", {
  local_interactive(FALSE)
  create_local_project()

  tmp <- proj_path("foo.txt")
  writeLines(letters[1:3], tmp)

  before <- read_utf8(tmp)
  write_over(tmp, letters[4:6], quiet = TRUE)
  expect_identical(read_utf8(tmp), before)

  use_git()
  withr::with_options(
    list(usethis.overwrite = TRUE),
    {
      write_over(tmp, letters[4:6], quiet = TRUE)
      expect_identical(read_utf8(tmp), letters[4:6])
    }
  )
})

test_that("write_over() works for a file in a project that is not active", {
  local_interactive(FALSE)
  owd <- getwd()
  proj <- create_local_project()
  use_git()

  tmp <- proj_path("foo.txt")
  writeLines(letters[1:3], tmp)

  withr::local_dir(owd)
  local_project(NULL)
  expect_false(proj_active())

  tmp <- path(proj, "foo.txt")
  before <- read_utf8(tmp)

  withr::with_options(
    list(usethis.overwrite = FALSE),
    {
      write_over(tmp, letters[4:6], quiet = TRUE)
      expect_identical(read_utf8(tmp), before)
    }
  )

  withr::with_options(
    list(usethis.overwrite = TRUE),
    {
      write_over(tmp, letters[4:6], quiet = TRUE)
      expect_identical(read_utf8(tmp), letters[4:6])
    }
  )
  expect_false(proj_active())
})

test_that("write_union() messaging is correct with weird working directory", {
  create_local_project()
  use_directory("aaa/bbb")
  setwd("aaa/bbb")

  withr::local_options(usethis.quiet = FALSE)
  expect_snapshot(
    write_union(proj_path("somefile"), letters[4:6])
  )
})
