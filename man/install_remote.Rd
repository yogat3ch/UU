% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_setup.R
\name{install_remote}
\alias{install_remote}
\title{Install a package}
\usage{
install_remote(pkg, remote, ..., to_desc = TRUE, snapshot = TRUE)
}
\arguments{
\item{pkg}{\code{chr} package names \strong{Required}}

\item{remote}{\code{chr} github remote}

\item{...}{
  Arguments passed on to \code{\link[remotes:install_github]{remotes::install_github}}
  \describe{
    \item{\code{repo}}{Repository address in the format
\verb{username/repo[/subdir][@ref|#pull|@*release]}. Alternatively, you can
specify \code{subdir} and/or \code{ref} using the respective parameters
(see below); if both are specified, the values in \code{repo} take
precedence.}
    \item{\code{ref}}{Desired git reference. Could be a commit, tag, or branch
name, or a call to \code{\link[remotes:github_pull]{github_pull()}} or \code{\link[remotes:github_release]{github_release()}}. Defaults to
\code{"HEAD"}, which means the default branch on GitHub and for git remotes.
See \href{https://help.github.com/en/github/administering-a-repository/setting-the-default-branch}{setting-the-default-branch}
for more details.}
    \item{\code{subdir}}{Subdirectory within repo that contains the R package.}
    \item{\code{auth_token}}{To install from a private repo, generate a personal
access token (PAT) with at least repo scope in
\url{https://github.com/settings/tokens} and
supply to this argument. This is safer than using a password because
you can easily delete a PAT without affecting any others. Defaults to
the \code{GITHUB_PAT} environment variable.}
    \item{\code{host}}{GitHub API host to use. Override with your GitHub enterprise
hostname, for example, \code{"github.hostname.com/api/v3"}.}
    \item{\code{dependencies}}{Which dependencies do you want to check?
Can be a character vector (selecting from "Depends", "Imports",
"LinkingTo", "Suggests", or "Enhances"), or a logical vector.

\code{TRUE} is shorthand for "Depends", "Imports", "LinkingTo" and
"Suggests". \code{NA} is shorthand for "Depends", "Imports" and "LinkingTo"
and is the default. \code{FALSE} is shorthand for no dependencies (i.e.
just check this package, not its dependencies).

The value "soft" means the same as \code{TRUE}, "hard" means the same as \code{NA}.

You can also specify dependencies from one or more additional fields,
common ones include:
\itemize{
\item Config/Needs/website - for dependencies used in building the pkgdown site.
\item Config/Needs/coverage for dependencies used in calculating test coverage.
}}
    \item{\code{upgrade}}{Should package dependencies be upgraded? One of "default", "ask", "always", or "never". "default"
respects the value of the \code{R_REMOTES_UPGRADE} environment variable if set,
and falls back to "ask" if unset. "ask" prompts the user for which out of
date packages to upgrade. For non-interactive sessions "ask" is equivalent
to "always". \code{TRUE} and \code{FALSE} are also accepted and correspond to
"always" and "never" respectively.}
    \item{\code{force}}{Force installation, even if the remote state has not changed
since the previous install.}
    \item{\code{quiet}}{If \code{TRUE}, suppress output.}
    \item{\code{build}}{If \code{TRUE} build the package before installing.}
    \item{\code{build_opts}}{Options to pass to \verb{R CMD build}, only used when \code{build} is \code{TRUE}.}
    \item{\code{build_manual}}{If \code{FALSE}, don't build PDF manual ('--no-manual').}
    \item{\code{build_vignettes}}{If \code{FALSE}, don't build package vignettes ('--no-build-vignettes').}
    \item{\code{repos}}{A character vector giving repositories to use.}
    \item{\code{type}}{Type of package to \code{update}.}
  }}

\item{to_desc}{\code{lgl} Add the package dependency to the \emph{DESCRIPTION} file?}

\item{snapshot}{\code{lgl} Run \code{renv::\link[renv]{snapshot}}}
}
\value{
Installs the package, add the version to the \emph{DESCRIPTION} file, and \link[renv]{snapshot}s the package to the \emph{renv.lock} file if present.
}
\description{
Install a package
}
\seealso{
Other project setup: 
\code{\link{creds_to_renviron}()},
\code{\link{fun_docs_table}()},
\code{\link{ignore_files}()},
\code{\link{key_pairs_duplicated}()},
\code{\link{key_pairs_text}()},
\code{\link{use_UU_reimports}()},
\code{\link{use_reimport}()},
\code{\link{write_to_rprofile}()}
}
\concept{project setup}
