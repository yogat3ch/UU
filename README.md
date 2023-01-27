
<!-- README.md is generated from README.Rmd. Please edit that file -->

# UU

<!-- badges: start -->
<!-- badges: end -->

A collection of *U*niversally *U*seful functions!

## Installation

You can install the development version of UU from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yogat3ch/UU")
```

All the fun(s)!

| Name                                    | Title                                                                                            | Description                                                                                                                                                             |
|:----------------------------------------|:-------------------------------------------------------------------------------------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| all_in                                  | Are all items in x in y?                                                                         | Are all items in x in y?                                                                                                                                                |
| as_js                                   | Preserve a string as JS/HTML (prevent translation of characters)                                 | Preserve a string as JS/HTML (prevent translation of characters)                                                                                                        |
| assign_in_ns                            | Assign a variable into a namespace                                                               | Unlocks and relocks namespaces and bindings as needed                                                                                                                   |
| col_types                               | Converts input to a specified type output                                                        | Given various inputs, provide a col_type specification in the format indicated by outtype                                                                               |
| color_cycle                             | Makes a cyclic color palette of a specified length using the specified transformation each cycle | Makes a cyclic color palette of a specified length using the specified transformation each cycle                                                                        |
| color_distance                          | Compute color distance                                                                           | Compute color distance                                                                                                                                                  |
| color_luminance                         | Find the luminance of a particular color, scaled 0-1                                             | Find the luminance of a particular color, scaled 0-1                                                                                                                    |
| color_match                             | Match colors by visual distance                                                                  | Helpful for pairing colors across light/dark palettes                                                                                                                   |
| color_rgb_table                         | Convert vector of colors to named tbl                                                            | Convert vector of colors to named tbl                                                                                                                                   |
| color_separate                          | Separate a vector of colors based on their distance                                              | Separate a vector of colors based on their distance                                                                                                                     |
| colors2css                              | Convert a list of colors to SCSS/Sass variables or classes                                       | Convert a list of colors to SCSS/Sass variables or classes                                                                                                              |
| common_names                            | Find the names in common                                                                         | Given named objects, find the names in common                                                                                                                           |
| creds_to_renviron                       | Write named keypairs to an .Renviron / .Rprofile file                                            | Writes key pairs to .Renviron / .Rprofile and adds .Renviron to .gitignore if not already there.                                                                        |
| css_col2vec                             | Convert a CSS representation of a color to an r,g,b numeric                                      | Convert a CSS representation of a color to an r,g,b numeric                                                                                                             |
| dep_read                                | Read a dependency from file                                                                      | Read a dependency from file                                                                                                                                             |
| dep_write                               | Write a dependency to file                                                                       | Write a dependency to file                                                                                                                                              |
| dir_fn                                  | Create a directory path pointing function                                                        | Create a directory path pointing function                                                                                                                               |
| dirs                                    | Path functions for commonly used directories                                                     | Path functions for commonly used directories                                                                                                                            |
| .file                                   | All the files that can be navigated to with file if they exist                                   | All the files that can be navigated to with file if they exist                                                                                                          |
| duration_print                          | Translate a duration into the human-legible estimation as a character                            | Translate a duration into the human-legible estimation as a character                                                                                                   |
| excel_date                              | Convert Excel character date representation to a Date                                            | Convert Excel character date representation to a Date                                                                                                                   |
| expr_pipe                               | Return a list of expressions all piped together as a single expression                           | Return a list of expressions all piped together as a single expression                                                                                                  |
| ext                                     | Extract the file extensions from a filepath                                                      | Given a path, extract the file extension                                                                                                                                |
| file_fn                                 | Return the appropriate function for reading the specified path/extension                         | Return the appropriate function for reading the specified path/extension                                                                                                |
| file                                    | Go to a specified file in the Virga Labs golem project                                           | A List object with convenience functions that open the named file in RStudio. An R named sublist of all files in the R folder if such a folder exists                   |
| filter_to                               | Change or apply filters to output type                                                           | Useful in concert with axis brushing                                                                                                                                    |
| find_by_class                           | Find an object by it’s class                                                                     | Find an object by it’s class                                                                                                                                            |
| fn_name                                 | Retrieve the function name                                                                       | Sometimes a function is passed down the call stack and it’s name is unknown. This function finds the name without having to pass it down the call stack as an argument. |
| folder                                  | Go to a specified folder in the Virga Labs golem project                                         | Go to a specified folder in the Virga Labs golem project                                                                                                                |
| fun_docs_table                          | Create a table of functions and their uses                                                       | Create a table of functions and their uses                                                                                                                              |
| gbort                                   | Custom error message                                                                             | Throw abort with format_error                                                                                                                                           |
| get_from_ns                             | Get an object from a namespace                                                                   | Get an object from a namespace                                                                                                                                          |
| get_global                              | Get an object from the global environment                                                        | Get an object from the global environment                                                                                                                               |
| get_package_fns                         | Get the names of all exported functions in a package                                             | Get the names of all exported functions in a package                                                                                                                    |
| glue_js                                 | Create a JS string with glue insertions                                                          |                                                                                                                                                                         |
| glue .open = !@ & .close = @#           | Create a JS string with glue insertions                                                          |                                                                                                                                                                         |
| glue .open = !@ & .close = @#           |                                                                                                  |                                                                                                                                                                         |
| gmsg                                    | Custom message                                                                                   |                                                                                                                                                                         |
| Message using format_message & cat_line | Custom message                                                                                   |                                                                                                                                                                         |
| Message using format_message & cat_line |                                                                                                  |                                                                                                                                                                         |
| %nin%                                   | Are lhs values absent from set on rhs?                                                           | Are lhs values absent from set on rhs?                                                                                                                                  |
| %\|0\|%                                 | Replace a 0 length value                                                                         | If the lhs is length 0, replace with rhs                                                                                                                                |
| %\|legit\|%                             | If legit lhs, else rhs                                                                           | If legit lhs, else rhs                                                                                                                                                  |
| %\|zchar\|%                             | Replace zero-length character strings with right hand side                                       | Replace zero-length character strings with right hand side                                                                                                              |
| gwarn                                   | Custom warning message                                                                           | Throw cli_alert_warning with format_warning                                                                                                                             |
| if_debug                                | Run expressions only when option use_debug = TRUE                                                | Run expressions only when option use_debug = TRUE                                                                                                                       |
| ignore_files                            | Add lines to .gitignore                                                                          | Add lines to .gitignore                                                                                                                                                 |
| inequality_key                          | Inequality conversion key                                                                        | Inequality conversion key                                                                                                                                               |
| install_remote                          | Install a package                                                                                | Install a package                                                                                                                                                       |
| is_error                                | Is object an error                                                                               | Is object an error                                                                                                                                                      |
| is_filepath                             | Is path a file path                                                                              | Given a path, is it a filepath?                                                                                                                                         |
| is_legit                                | Is object legit?                                                                                 | Is object non-null, non-empty, non-NA, and not a try-error?                                                                                                             |
| is_package_dev                          | Is package in development or installed                                                           | Is package in development or installed                                                                                                                                  |
| is_package                              | Is working directory a package?                                                                  | Is working directory a package?                                                                                                                                         |
| join_check                              | Detect possible duplicates of rows or columns after a join                                       | Detect possible duplicates of rows or columns after a join                                                                                                              |
| key_out                                 | Handle different output type requests for match_df                                               | Handle different output type requests for match_df                                                                                                                      |
| key_pairs_duplicated                    | Find duplicates in key pairs                                                                     | Find duplicates in key pairs                                                                                                                                            |
| key_pairs_text                          | Make key-pairs from a named character vector                                                     | Make key-pairs from a named character vector                                                                                                                            |
| last_updated                            | Gather last updated times for on-disk files                                                      | Check the last modified time files or paths                                                                                                                             |
| len_unique                              | The length of unique values in a vector                                                          | The length of unique values in a vector                                                                                                                                 |
| list.files2                             | List full file paths with the file name as the name                                              | List full file paths with the file name as the name                                                                                                                     |
| luminance_filter                        | Filter colors based on a luminance threshold                                                     | Filter colors based on a luminance threshold                                                                                                                            |
| magnitude_order                         | Compute the order of magnitude                                                                   | Uses the floor to round                                                                                                                                                 |
| magnitude_triplet                       | Compute the order of magnitude triplet ie thousand, million, trillion                            | Compute the order of magnitude triplet ie thousand, million, trillion                                                                                                   |
| make_names                              | Make a file path name with underscores                                                           | Make a file path name with underscores                                                                                                                                  |
| map_class                               | Match the classes of one object to that of another object                                        | Match the classes of one object to that of another object                                                                                                               |
| match_df                                | Extract matching rows of a data frame.                                                           | Match works in the same way as join, but instead of return the combined                                                                                                 |

dataset, it only returns the matching rows from the first dataset. This
is particularly useful when you’ve summarised the data in some way and
want to subset the original data by a characteristic of the subset. \|
\|match_letters \|Match the first n letters to supplied arguments \|Case
insensitive matching of argument to possibilities provided in ellipsis.
\| \|max2 \|An alternative to max that preserves names \|An alternative
to max that preserves names \| \|missing_args \|Get the missing
arguments from the function as character \|Get the missing arguments
from the function as character \| \|mkpath \|Construct a path \|Given a
path, construct it if it does not exist. \| \|most \|Are most values
TRUE \|IF more than half the values are TRUE, returns TRUE \|
\|move_js_to_folder \|Move all js files to js folder \|Move all js files
to js folder \| \|need_pkg \|Get a function from a package, abort if
package not installed. \|Get a function from a package, abort if package
not installed. \| \|needs_update \|Check if files need to be updated
\|Check if files need to be updated \| \|nonull \|Is value non-null?
\|Is value non-null? \| \|not_na \|Is value non-NA? \|Is value non-NA?
\| \|num_chr_suffi \|Abbreviations of numeric magnitude \|Abbreviations
of numeric magnitude \| \|num2str_vec \|Convert number to string
Vectorized version \|Convert number to string Vectorized version \|
\|num2str \|Convert numeric value to a string abbreviation with K, M, B
for Thousand, Million & Billion \|Convert numeric value to a string
abbreviation with K, M, B for Thousand, Million & Billion \|
\|object_ext \|Provide the appropriate file extension for a given object
\|Provide the appropriate file extension for a given object \|
\|object_fn \|Return the appropriate function for writing the supplied
object to disk \|Return the appropriate function for writing the
supplied object to disk \| \|object_write \|Provide the appropriate file
read/write function \|Write an object to disk \| \|opts \|Check option
value. Use interactively only! \|Check option value. Use interactively
only! \| \|pkg_ns \|Return the current package namespace \|Return the
current package namespace \| \|profile_script \|profile_script \|This
function will add profiling code to a script wherever the following
flags are found in the first non-spacing characters on the line:

\#\<p Opening comment flag where profile_open will be inserted. \#\>p
Closing comment flag where profile_close will be inserted. \| \|read_js
\|Read Javascript file \|Read Javascript file \| \|reexports \|Objects
exported from other packages \|These objects are imported from other
packages. Follow the links below to see their documentation.

plyrround_any

rlang%\|%, %\|\|% \| \|regex_op \|Create a compound regex grouped
statement \|Create a compound regex grouped statement \| \|regex_or
\|Create a compound regex grouped OR statement \|Create a compound regex
grouped OR statement \| \|rgb2hex \|Convert r,g,b,a values as string or
numeric to hex \|Convert r,g,b,a values as string or numeric to hex \|
\|rle_df \|rle_df - create a run-length-encoding data.frame \|Given an
rle this function will return a data.frame of starts, ends, and indexes
thereof of the run lengths. Credit:
<https://stackoverflow.com/questions/43875716/find-start-and-end-positions-indices-of-runs-consecutive-values>
\| \|rle_seq \|Create a sequence from the start to the end for a given
value from an rle_df for indexing \|Create a sequence from the start to
the end for a given value from an rle_df for indexing \| \|round_to
\|Find the global minima/maxima of input vectors \|If accuracy is
omitted, number will be rounded to the nearest order of magnitude IE
145, if fn = min, will round to 100 \| \|shiny_error_recover \|Toggle
recover on error when obtuse shiny errors are encountered \|Toggle
recover on error when obtuse shiny errors are encountered \| \|size
\|Digital storage size conversion See object.size \|Digital storage size
conversion See object.size \| \|smode \|Statistical mode \|Return the
most frequenctly occuring item in a dataset \| \|startup \|Load project
& user-level .Renviron & .Rprofile \|Load project & user-level .Renviron
& .Rprofile \| \|str_break_every \|Break word every x characters \|Break
word every x characters \| \|str_inequality \|Convert inequality
statements between character, mathematic, symbol and function
representations \|Convert inequality statements between character,
mathematic, symbol and function representations \| \|`%&#124;try&#124;%`
\|Try an expression \|Calls the expression (LHS) & if it fails return
RHS \| \|time_elapsed \|Return a logical on an interval \|Return a
logical on an interval \| \|toggle \|Toggle an option \|Meant for
interactive use only. \| \|unit_conversion \|Abbreviations of numeric
magnitude for various units \|Abbreviations of numeric magnitude for
various units \| \|unit_find \|Find the row corresponding to a value in
unit_conversion \|Find the row corresponding to a value in
unit_conversion \| \|unit_modify_vec \|Modify unit abbreviation,
vectorized version \|Modify unit abbreviation, vectorized version \|
\|unit_modify \|Modify unit abbreviations \|Modify unit abbreviations \|
\|unit_string \|Extract the units from a string \|It is assumed that
units are encased in parentheses at the end of the string \|
\|unload_namespaces \|Unload namespaces prior to package install
\|Unload namespaces prior to package install \| \|use_reimport \|Add a
function to reimports \|Add a function to reimports \|
\|use_UU_reimports \|Write R/aaa_reimports.R file \|Write
R/aaa_reimports.R file \| \|vlookup_from_ref \|Vlookup replace using a
lookup column and reference table \|Vlookup replace using a lookup
column and reference table \| \|week_factor \|Get numeric day of the
week \|Get numeric day of the week \| \|write_dir_fn \|Write dir helper
function that are robust to dev vs deployed package states \|Write dir
helper function that are robust to dev vs deployed package states \|
\|write_opts \|Write all the option checking functions to a file \|Write
all the option checking functions to a file \| \|write_to_rprofile
\|Write expressions to the .Rprofile \|Write expressions to the
.Rprofile \| \|xpath_sibling_between \|Generate xpath to find sibling
nodes between two elements The function produces a compounding xpath
with each subsequent argument provided. Thus the final argument
specified will be the node that is selected by the resulting xpath with
the exception of nested_tag_contains which helps to identify a nested
tag by its contents \|Generate xpath to find sibling nodes between two
elements The function produces a compounding xpath with each subsequent
argument provided. Thus the final argument specified will be the node
that is selected by the resulting xpath with the exception of
nested_tag_contains which helps to identify a nested tag by its contents
\| \|zchar_remove \|Remove zero length strings (or string with all
spaces) \|Remove zero length strings (or string with all spaces) \|
\|zchar \|Is zero-length character? \|Is zero-length character? \|
