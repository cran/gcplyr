% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{import_blockdesigns}
\alias{import_blockdesigns}
\title{Import blockdesigns}
\usage{
import_blockdesigns(
  files,
  block_names = NULL,
  block_name_header = "block_name",
  join_as_cols = TRUE,
  sep = NULL,
  values_colname = "Designs",
  into = NULL,
  keep_blocknames = !join_as_cols,
  warn_joinrows_nointo = TRUE,
  join_designs = NULL,
  ...
)
}
\arguments{
\item{files}{A vector of filepaths relative to the current working directory
where each filepath is a single plate read to be read by
\link{read_blocks}.}

\item{block_names}{Vector of names corresponding to each design element (each
block). Inferred from filenames, if not specified.

When \code{keep_blocknames = TRUE}, the output will have
a column containing these values, with the column name
specified by \code{block_name_header}.

When \code{join_as_cols = TRUE}, the \code{block_names} are
also used as the output column names for each separated
design column.}

\item{block_name_header}{When \code{keep_blocknames = TRUE}, the column name of the
column containing the \code{block_names}.}

\item{join_as_cols}{logical indicating whether blocks (if there are multiple)
should be joined as columns (i.e. describe the same plate) in 
the tidy output. If \code{FALSE}, blocks are joined as rows
(i.e. describe different plates) in the tidy output.}

\item{sep}{If designs have been pasted together, this specifies the
string they should be split apart by via \link{separate_tidy}.}

\item{values_colname}{When \code{join_as_cols = FALSE} and \code{sep} is
not specified, all the design values will be in a column
named by \code{values_colname}. For other cases, see the
\strong{Value} section.}

\item{into}{When \code{sep} is specified, \code{into} sets the names
of the columns after splitting (see \strong{Value} section
for behavior when \code{into} is not set).}

\item{keep_blocknames}{logical indicating whether the column containing
\code{block_names} (or those inferred from file names) should
be retained in the output. By default, blocknames are retained
only if \code{join_as_cols = FALSE}.}

\item{warn_joinrows_nointo}{logical indicating whether warning should
be raised when multiple blocks are joined as rows 
(\code{join_as_cols = FALSE}) and \code{sep} is specified,
but \code{into} is not specified.}

\item{join_designs}{Deprecated, use \code{join_as_cols} instead}

\item{...}{Other arguments to pass to \link{read_blocks}, 
\link{paste_blocks}, \link{trans_block_to_wide},
\link{trans_wide_to_tidy}, or \link{separate_tidy}.

See Details for more information}
}
\value{
A tidy-shaped \code{data.frame} containing the design information
        from \code{files}. This always includes a "Well" column. 
        
        If \code{keep_blocknames = TRUE}, this includes a column with the
        column name specified by \code{block_name_header} and containing
        \code{block_names} (or block names inferred from file names).
        
        The layout of the design values varies depending on the inputs:
        
        If \code{join_as_cols = TRUE}, each block was joined as a column,
        with the columns named according to \code{block_names} (or block
        names inferred from file names). In this case, if \code{sep} was 
        specified, each column was split by \code{sep} into columns named by 
        splitting the corresponding block name by \code{sep} (post-split 
        column names can alternatively be specified directly via \code{into}).
        
        Otherwise, when \code{join_as_cols = FALSE}, each block was joined
        as rows, with the column containing all design values named by
        \code{values_colname}. In this case, if \code{sep} was specified,
        that single design column was split by \code{sep} into columns
        named by splitting \code{values_colname} (post-split column names
        can alternatively be specified directly via \code{into}).
}
\description{
Function to import block-shaped designs from files and return tidy designs.
This function acts as a wrapper that calls \link{read_blocks}, 
\link{paste_blocks}, \link{trans_block_to_wide}, \link{trans_wide_to_tidy}, 
and \link{separate_tidy}
}
\details{
Other common arguments that you may want to provide via 
             \code{...} include:

             \code{startrow}, \code{endrow}, \code{startcol}, \code{endcol}, 
             \code{sheet} - specifying the location of design information 
             inside \code{files} to \link{read_blocks}.
             
             \code{wellnames_sep} - specifying what character (or "" for 
             none) should be used when pasting together the rownames and
             column names. Note that this should be chosen to match
             the well names in your measures.
             
             \code{into} - specifying the column names resulting from
             using \link{separate_tidy} on the values_colname column.
             
             Note that \code{import_blockdesigns} cannot currently handle
             metadata specified via the \code{metadata} argument of
             \link{read_blocks}.
             
             If you find yourself needing more control, you can run the 
             steps manually, first reading with \link{read_blocks},
             pasting as needed with \link{paste_blocks}, 
             transforming to tidy with \link{trans_block_to_wide} and
             \link{trans_wide_to_tidy}, and separating as needed with
             \link{separate_tidy}.
}
