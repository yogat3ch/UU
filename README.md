
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

Check out the [pkgdown site](https://yogat3ch.github.io/UU/) for more
details!

All the fun(s)!

<table class="table">
<thead>
<tr>
<th>Name</th>
<th>Concept</th>
<th>Title</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td>character_codes</td>
<td></td>
<td>Replace HTML Character codes with their character equivalent</td>
<td>See ?.character_codes for conversion table. Note that this will not translate Ampersand if converting from Namedcode to character because it will translate the Namedcodes themselves.</td>
</tr>
<tr>
<td>col_type_hash</td>
<td></td>
<td>Index of column type conversions</td>
<td></td>
</tr>
<tr>
<td>color_interpolate</td>
<td></td>
<td>Interpolate between two colors</td>
<td></td>
</tr>
<tr>
<td>common_names</td>
<td></td>
<td>Find the common names between two objects</td>
<td>Given named objects, find the names in common</td>
</tr>
<tr>
<td>concat_rows</td>
<td></td>
<td>Concatenate row values in a poorly scraped table</td>
<td></td>
</tr>
<tr>
<td>create_simple_get_function</td>
<td></td>
<td>Create a function that creates an object and assigns it to a namespace the first time it's called and subsequently retrieves it from the namespace thereafter.</td>
<td>Useful when the object depends on a long running task such as a database query.</td>
</tr>
<tr>
<td>css_col2vec_</td>
<td></td>
<td>Vectorized version of css_col2vec</td>
<td></td>
</tr>
<tr>
<td>.character_codes</td>
<td></td>
<td>Character code conversion table</td>
<td></td>
</tr>
<tr>
<td>.time_factor</td>
<td></td>
<td>Timespans as factor</td>
<td></td>
</tr>
<tr>
<td>expr_pipe</td>
<td></td>
<td>Return a list of expressions all piped together as a single expression</td>
<td>Useful when making complex compound statements that require dynamic substitution via tidy eval for dynamically created variables derived from the context.</td>
</tr>
<tr>
<td>file_timestamp</td>
<td></td>
<td>Make a file path compliant ISO8601 timestamp</td>
<td></td>
</tr>
<tr>
<td>filter_to</td>
<td></td>
<td>Change or apply filters to output type</td>
<td>Useful in concert with axis brushing</td>
</tr>
<tr>
<td>find_by_class</td>
<td></td>
<td>Find an object by it's class</td>
<td></td>
</tr>
<tr>
<td>fml_list</td>
<td></td>
<td>Print function formals as a list</td>
<td></td>
</tr>
<tr>
<td>fn_name</td>
<td></td>
<td>Retrieve the function name</td>
<td>Sometimes a function is passed down the call stack and it's name is unknown. This function finds the name without having to pass it down the call stack as an argument.</td>
</tr>
<tr>
<td>formula_make</td>
<td></td>
<td>Create a formula given predictors and a label (response variable)</td>
<td></td>
</tr>
<tr>
<td>increment</td>
<td></td>
<td>Increment an in-place counter</td>
<td></td>
</tr>
<tr>
<td>join_check</td>
<td></td>
<td>Detect possible duplicates of rows or columns after a join</td>
<td></td>
</tr>
<tr>
<td>json_validate</td>
<td></td>
<td>Is JSON file or text valid?</td>
<td></td>
</tr>
<tr>
<td>key_out</td>
<td></td>
<td>Handle different output type requests for match_df</td>
<td></td>
</tr>
<tr>
<td>list_rename</td>
<td></td>
<td>Rename a list</td>
<td>From https://stackoverflow.com/users/6646912/krassowskikrassowski on SO https://stackoverflow.com/a/73621060/2675597link</td>
</tr>
<tr>
<td>map_class</td>
<td></td>
<td>Match the classes of one object to that of another object</td>
<td></td>
</tr>
<tr>
<td>match_df</td>
<td></td>
<td>Extract matching rows of a data frame.</td>
<td>Match works in the same way as join, but instead of return the combined
dataset, it only returns the matching rows from the first dataset. This is
particularly useful when you've summarised the data in some way
and want to subset the original data by a characteristic of the subset.</td>
</tr>
<tr>
<td>match_letters</td>
<td></td>
<td>Match the first n letters to supplied arguments</td>
<td>Case insensitive matching of argument to possibilities provided in ellipsis.</td>
</tr>
<tr>
<td>meeting_timer</td>
<td></td>
<td>Partition a meeting into evenly distributed sections based on how much time is left between when intros end and the end of the meeting</td>
<td></td>
</tr>
<tr>
<td>missing_args</td>
<td></td>
<td>Get the missing arguments from the function as character</td>
<td></td>
</tr>
<tr>
<td>nonull</td>
<td></td>
<td>Is value non-null?</td>
<td></td>
</tr>
<tr>
<td>not_na</td>
<td></td>
<td>Is value non-NA?</td>
<td></td>
</tr>
<tr>
<td>object_ext</td>
<td></td>
<td>Provide the appropriate file extension for a given object</td>
<td></td>
</tr>
<tr>
<td>reexports</td>
<td></td>
<td>Objects exported from other packages</td>
<td>These objects are imported from other packages. Follow the links
below to see their documentation.
&#10;
  plyrround_any
&#10;  rlang%||%</td>
</tr>
<tr>
<td>regex_op</td>
<td></td>
<td>Create a compound regex grouped statement</td>
<td></td>
</tr>
<tr>
<td>regex_or</td>
<td></td>
<td>Create a compound regex grouped OR statement</td>
<td></td>
</tr>
<tr>
<td>startup</td>
<td></td>
<td>Load project &amp; user-level .Renviron &amp; .Rprofile</td>
<td></td>
</tr>
<tr>
<td>unit_conversion</td>
<td></td>
<td>Abbreviations of numeric magnitude for various units</td>
<td></td>
</tr>
<tr>
<td>unit_shorthand</td>
<td></td>
<td>Easily translate long form unit names to shorthand</td>
<td>Useful for condensed displays like axis titles</td>
</tr>
<tr>
<td>was_updated</td>
<td></td>
<td>Was a file updated in the last hour?</td>
<td></td>
</tr>
<tr>
<td>week_bins_per_year</td>
<td></td>
<td>Create a tbl with each day of the year, it's week number, and the interval containing that week.</td>
<td></td>
</tr>
<tr>
<td>which_cols</td>
<td></td>
<td>Translate DT Column names to numeric indices using regex matching</td>
<td></td>
</tr>
<tr>
<td>color_cycle</td>
<td>color</td>
<td>Makes a cyclic color palette of a specified length using the specified transformation each cycle</td>
<td></td>
</tr>
<tr>
<td>color_distance</td>
<td>color</td>
<td>Compute color distance</td>
<td></td>
</tr>
<tr>
<td>color_luminance</td>
<td>color</td>
<td>Find the luminance of a particular color, scaled 0-1</td>
<td></td>
</tr>
<tr>
<td>color_match</td>
<td>color</td>
<td>Match colors by visual distance</td>
<td>Helpful for pairing colors across light/dark palettes</td>
</tr>
<tr>
<td>color_rgb_table</td>
<td>color</td>
<td>Convert vector of colors to named tbl</td>
<td></td>
</tr>
<tr>
<td>color_separate</td>
<td>color</td>
<td>Separate a vector of colors based on their distance</td>
<td></td>
</tr>
<tr>
<td>color_text_by_luminance</td>
<td>color</td>
<td>Set text color based on luminance</td>
<td>Useful for applying one or another of text colors based on the luminance of a background</td>
</tr>
<tr>
<td>colors2css</td>
<td>color</td>
<td>Convert a list of colors to SCSS/Sass variables or classes</td>
<td></td>
</tr>
<tr>
<td>css_col2vec</td>
<td>color</td>
<td>Convert a CSS representation of a color to an r,g,b numeric</td>
<td></td>
</tr>
<tr>
<td>luminance_filter</td>
<td>color</td>
<td>Filter colors based on a luminance threshold</td>
<td></td>
</tr>
<tr>
<td>rgb2hex</td>
<td>color</td>
<td>Convert r,g,b,a values as string or numeric to hex</td>
<td></td>
</tr>
<tr>
<td>gbort</td>
<td>condition signaling</td>
<td>Custom error message</td>
<td>Throw abort with format_error</td>
</tr>
<tr>
<td>ginfo</td>
<td>condition signaling</td>
<td>Custom info message</td>
<td>Provide info with inform</td>
</tr>
<tr>
<td>gmsg</td>
<td>condition signaling</td>
<td>Custom message
Message using format_message &amp; cat_line</td>
<td></td>
</tr>
<tr>
<td>gwarn</td>
<td>condition signaling</td>
<td>Custom warning message</td>
<td>Throw cli_alert_warning with format_warning</td>
</tr>
<tr>
<td>trace_back_json</td>
<td>condition signaling</td>
<td>Writes a trace back as a json for error logging
Useful for remote error logging on deployed shiny apps, such as via Sentry</td>
<td></td>
</tr>
<tr>
<td>is_error</td>
<td>conditionals</td>
<td>Is object an error</td>
<td></td>
</tr>
<tr>
<td>is_legit</td>
<td>conditionals</td>
<td>Is object legit?</td>
<td>Is object non-null, non-empty, non-NA, and not a try-error?</td>
</tr>
<tr>
<td>is_project</td>
<td>conditionals</td>
<td>Is Session in a Project?</td>
<td></td>
</tr>
<tr>
<td>larger</td>
<td>conditionals</td>
<td>Which is larger</td>
<td></td>
</tr>
<tr>
<td>most</td>
<td>conditionals</td>
<td>Are most values TRUE</td>
<td>IF more than half the values are TRUE, returns TRUE</td>
</tr>
<tr>
<td>same</td>
<td>conditionals</td>
<td>Are the values in each object the same?</td>
<td>The primary difference from identical &amp; all.equal is that objects are sorted by name so order doesn't matter. Set sort_by_names = FALSE to sort by values.</td>
</tr>
<tr>
<td>zchar</td>
<td>conditionals</td>
<td>Is zero-length character?</td>
<td></td>
</tr>
<tr>
<td>col_types</td>
<td>file IO</td>
<td>Converts input to a specified type output</td>
<td>Given various inputs, provide a col_type specification in the format indicated by outtype</td>
</tr>
<tr>
<td>dep_read</td>
<td>file IO</td>
<td>Read a dependency from file</td>
<td></td>
</tr>
<tr>
<td>dep_write</td>
<td>file IO</td>
<td>Write a dependency to file</td>
<td></td>
</tr>
<tr>
<td>dir_fn</td>
<td>file IO</td>
<td>Create a directory path pointing function</td>
<td></td>
</tr>
<tr>
<td>dirs</td>
<td>file IO</td>
<td>Path functions for commonly used directories</td>
<td></td>
</tr>
<tr>
<td>ext</td>
<td>file IO</td>
<td>Extract the file extensions from a filepath</td>
<td>Given a path, extract the file extension</td>
</tr>
<tr>
<td>file_fn</td>
<td>file IO</td>
<td>Return the appropriate function for reading the specified path/extension</td>
<td></td>
</tr>
<tr>
<td>is_filepath</td>
<td>file IO</td>
<td>Is path a file path</td>
<td>Given a path, is it a filepath?</td>
</tr>
<tr>
<td>last_updated</td>
<td>file IO</td>
<td>Gather last updated times for on-disk files</td>
<td>Check the last modified time files or paths</td>
</tr>
<tr>
<td>list.files2</td>
<td>file IO</td>
<td>List full file paths with the file name as the name</td>
<td></td>
</tr>
<tr>
<td>load_obj</td>
<td>file IO</td>
<td>Load an R object from a file</td>
<td>This function loads an R object from a file into the global environment or a new environment.</td>
</tr>
<tr>
<td>make_names</td>
<td>file IO</td>
<td>Make a file path name with underscores</td>
<td></td>
</tr>
<tr>
<td>mkpath</td>
<td>file IO</td>
<td>Construct a path</td>
<td>Given a path, construct it if it does not exist.</td>
</tr>
<tr>
<td>move_files_to_folder</td>
<td>file IO</td>
<td>Move all files to a folder</td>
<td></td>
</tr>
<tr>
<td>needs_update</td>
<td>file IO</td>
<td>Check if files need to be updated</td>
<td></td>
</tr>
<tr>
<td>object_fn</td>
<td>file IO</td>
<td>Return the appropriate function for writing the supplied object to disk</td>
<td></td>
</tr>
<tr>
<td>object_write</td>
<td>file IO</td>
<td>Provide the appropriate file read/write function</td>
<td>Write an object to disk</td>
</tr>
<tr>
<td>package_size</td>
<td>file IO</td>
<td>Return the size of a package, or all packages in a folder</td>
<td></td>
</tr>
<tr>
<td>write_dir_fn</td>
<td>file IO</td>
<td>Write dir helper function that are robust to dev vs deployed package states</td>
<td></td>
</tr>
<tr>
<td>write_lines</td>
<td>file IO</td>
<td>Write lines at a specific location in a file</td>
<td></td>
</tr>
<tr>
<td>.file</td>
<td>file navigation</td>
<td>All the files that can be navigated to with file if they exist</td>
<td></td>
</tr>
<tr>
<td>file</td>
<td>file navigation</td>
<td>Go to a specified file</td>
<td>A List object with convenience functions that open the named file in RStudio. An R named sublist of all files in the R folder if such a folder exists</td>
</tr>
<tr>
<td>folder</td>
<td>file navigation</td>
<td>Go to a specified folder</td>
<td></td>
</tr>
<tr>
<td>%allin%</td>
<td>infixes</td>
<td>Are all lhs values in rhs?</td>
<td></td>
</tr>
<tr>
<td>%nin%</td>
<td>infixes</td>
<td>Are lhs values absent from set on rhs?</td>
<td></td>
</tr>
<tr>
<td>%|0|%</td>
<td>infixes</td>
<td>Replace a 0 length value</td>
<td>If the lhs is length 0, replace with rhs</td>
</tr>
<tr>
<td>%|%</td>
<td>infixes</td>
<td>Replace NA values in LHS with RHS</td>
<td>Does not strictly enforce class typing like op-na-default</td>
</tr>
<tr>
<td>%|legit|%</td>
<td>infixes</td>
<td>If legit lhs, else rhs</td>
<td></td>
</tr>
<tr>
<td>%|try|%</td>
<td>infixes</td>
<td>Try an expression</td>
<td>Calls the expression (LHS) &amp; if it fails return RHS</td>
</tr>
<tr>
<td>%|zchar|%</td>
<td>infixes</td>
<td>Replace zero-length character strings with right hand side</td>
<td></td>
</tr>
<tr>
<td>comparison_inverse_key</td>
<td>math</td>
<td>Math comparison comparator inverse key</td>
<td></td>
</tr>
<tr>
<td>comparison_inverse</td>
<td>math</td>
<td>Convert a math comparator to it's inverse</td>
<td></td>
</tr>
<tr>
<td>comparison_key</td>
<td>math</td>
<td>Math comparison comparator to plain english key</td>
<td></td>
</tr>
<tr>
<td>evens</td>
<td>math</td>
<td>Get even numbers</td>
<td></td>
</tr>
<tr>
<td>interpolate</td>
<td>math</td>
<td>Simple interpolate between two numbers</td>
<td></td>
</tr>
<tr>
<td>odds</td>
<td>math</td>
<td>Get odd numbers</td>
<td></td>
</tr>
<tr>
<td>str_comparison</td>
<td>math</td>
<td>Convert inequality statements between character, mathematic, symbol and function representations</td>
<td></td>
</tr>
<tr>
<td>assign_global</td>
<td>namespaces</td>
<td>Assign an object to the global environment</td>
<td></td>
</tr>
<tr>
<td>assign_in_ns</td>
<td>namespaces</td>
<td>Assign a variable into a namespace</td>
<td>Unlocks and relocks namespaces and bindings as needed</td>
</tr>
<tr>
<td>get_from_ns</td>
<td>namespaces</td>
<td>Get an object from a namespace</td>
<td></td>
</tr>
<tr>
<td>get_global</td>
<td>namespaces</td>
<td>Get an object from the global environment</td>
<td></td>
</tr>
<tr>
<td>pkg_ns</td>
<td>namespaces</td>
<td>Return the current package namespace</td>
<td></td>
</tr>
<tr>
<td>unload_namespaces</td>
<td>namespaces</td>
<td>Unload namespaces prior to package install</td>
<td></td>
</tr>
<tr>
<td>if_debug</td>
<td>options</td>
<td>Run expressions only when option use_debug = TRUE</td>
<td></td>
</tr>
<tr>
<td>opts</td>
<td>options</td>
<td>Check option value.</td>
<td>This is a list that will populate dynamically with the options in the project local .Rprofile allowing them to be read by calling the method. This population of methods happens at the beginning of a session. It can be accessed with UU::opts. If you wish to check option values in non-interactive sessions, see the write_opts function which will write a file with an opts object based on the current state of .Rprofile that can be used during non-interactive (deployed apps) sessions.</td>
</tr>
<tr>
<td>toggle</td>
<td>options</td>
<td>Toggle or change an option listed in a local .Rprofile for the session</td>
<td>Any options in the project local .Rprofile will populate this object as named methods. These named methods, when called, will toggle the options on or off. Alternatively, if an option is not logical, it can be changed using the set argument. This loads at the start of an R session if startup has been called in the user-level .Rprofile. These methods are meant for interactive use only.</td>
</tr>
<tr>
<td>write_opts</td>
<td>options</td>
<td>Write all the option checking functions to a file</td>
<td></td>
</tr>
<tr>
<td>get_package_fns</td>
<td>package dev</td>
<td>Get the names of all exported functions in a package</td>
<td></td>
</tr>
<tr>
<td>is_package_dev</td>
<td>package dev</td>
<td>Is package in development or installed</td>
<td></td>
</tr>
<tr>
<td>is_package</td>
<td>package dev</td>
<td>Is working directory a package?</td>
<td></td>
</tr>
<tr>
<td>need_pkg</td>
<td>package dev</td>
<td>Get a function from a package, abort if package not installed.</td>
<td></td>
</tr>
<tr>
<td>pkg_chr_split_comma</td>
<td>package dev</td>
<td>Split a list of packages separated by commas</td>
<td></td>
</tr>
<tr>
<td>profile_script</td>
<td>profiling</td>
<td>profile_script</td>
<td>This function will add profiling code to a script wherever the following flags are found in the first non-spacing characters on the line:
&#10;#&lt;p Opening comment flag where profile_open will be inserted.
#&gt;p Closing comment flag where profile_close will be inserted.</td>
</tr>
<tr>
<td>creds_to_renviron</td>
<td>project setup</td>
<td>Write named keypairs to an .Renviron / .Rprofile file</td>
<td>Writes key pairs to .Renviron / .Rprofile and adds .Renviron to .gitignore if not already there.</td>
</tr>
<tr>
<td>fun_docs_table</td>
<td>project setup</td>
<td>Create a table of functions and their uses</td>
<td></td>
</tr>
<tr>
<td>ignore_files</td>
<td>project setup</td>
<td>Add lines to .gitignore</td>
<td></td>
</tr>
<tr>
<td>install_remote</td>
<td>project setup</td>
<td>Install a package</td>
<td></td>
</tr>
<tr>
<td>key_pairs_duplicated</td>
<td>project setup</td>
<td>Find duplicates in key pairs</td>
<td></td>
</tr>
<tr>
<td>key_pairs_text</td>
<td>project setup</td>
<td>Make key-pairs from a named character vector</td>
<td></td>
</tr>
<tr>
<td>use_reimport</td>
<td>project setup</td>
<td>Add a function to reimports</td>
<td></td>
</tr>
<tr>
<td>use_UU_reimports</td>
<td>project setup</td>
<td>Write R/aaa_reimports.R file with all current infix operators</td>
<td>All infix operators available: %allin%, %nin%, %|%, %|0|%, %|legit|%, %|try|%, %|zchar|%, and %||%</td>
</tr>
<tr>
<td>write_to_rprofile</td>
<td>project setup</td>
<td>Write expressions to the .Rprofile</td>
<td></td>
</tr>
<tr>
<td>magnitude_order</td>
<td>rounding</td>
<td>Compute the order of magnitude</td>
<td>Uses the floor to round</td>
</tr>
<tr>
<td>magnitude_triplet</td>
<td>rounding</td>
<td>Compute the order of magnitude triplet ie thousand, million, trillion</td>
<td></td>
</tr>
<tr>
<td>num_chr_suffi</td>
<td>rounding</td>
<td>Abbreviations of numeric magnitude</td>
<td></td>
</tr>
<tr>
<td>num2str_vec</td>
<td>rounding</td>
<td>Convert number to string Vectorized version</td>
<td></td>
</tr>
<tr>
<td>num2str</td>
<td>rounding</td>
<td>Convert numeric value to a string abbreviation with K, M, B for Thousand, Million &amp; Billion</td>
<td></td>
</tr>
<tr>
<td>round_to</td>
<td>rounding</td>
<td>Find convenient limits of input vectors</td>
<td>If accuracy is omitted, number will be rounded to the nearest order of magnitude IE 145, if fn = min, will round to 100</td>
</tr>
<tr>
<td>size</td>
<td>rounding</td>
<td>Digital storage size conversion
See object.size</td>
<td>Digital storage size conversion
See object.size
&#10;Vectorized version of size</td>
</tr>
<tr>
<td>unit_find</td>
<td>rounding</td>
<td>Find the row corresponding to a value in unit_conversion</td>
<td></td>
</tr>
<tr>
<td>unit_modify_vec</td>
<td>rounding</td>
<td>Modify unit abbreviation, vectorized version</td>
<td></td>
</tr>
<tr>
<td>unit_modify</td>
<td>rounding</td>
<td>Modify unit abbreviations</td>
<td></td>
</tr>
<tr>
<td>unit_string</td>
<td>rounding</td>
<td>Extract the units from a string</td>
<td>It is assumed that units are encased in parentheses at the end of the string</td>
</tr>
<tr>
<td>as_js</td>
<td>shiny</td>
<td>Preserve a string as JS/HTML (prevent translation of characters)</td>
<td></td>
</tr>
<tr>
<td>glue_js</td>
<td>shiny</td>
<td>Create a JS string with glue insertions
glue .open = !@ &amp; .close = @#</td>
<td></td>
</tr>
<tr>
<td>nm_to_id</td>
<td>shiny</td>
<td>Make a randomly formatted name into snakecase id</td>
<td></td>
</tr>
<tr>
<td>path_strip_shiny</td>
<td>shiny</td>
<td>Strip a file path to everything after resourcepath if shiny is running</td>
<td>Useful for linking to internal files, such as with image source attributes &lt;img src="[path]"&gt;</td>
</tr>
<tr>
<td>path_strip_to</td>
<td>shiny</td>
<td>Strip a file path to everything after resourcepath</td>
<td>Useful for linking to internal files, such as with image source attributes &lt;img src="[path]"&gt;</td>
</tr>
<tr>
<td>read_js</td>
<td>shiny</td>
<td>Read Javascript file</td>
<td></td>
</tr>
<tr>
<td>shiny_error_recover</td>
<td>shiny</td>
<td>Toggle recover on error when obtuse shiny errors are encountered</td>
<td></td>
</tr>
<tr>
<td>strip_html</td>
<td>shiny</td>
<td>Remove all HTML tags from a character vector</td>
<td></td>
</tr>
<tr>
<td>max2</td>
<td>statistics</td>
<td>An alternative to max that preserves names</td>
<td></td>
</tr>
<tr>
<td>smode</td>
<td>statistics</td>
<td>Statistical mode</td>
<td>Return the most frequenctly occuring item in a dataset</td>
</tr>
<tr>
<td>duration_print</td>
<td>time</td>
<td>Translate a duration into the human-legible estimation as a character</td>
<td></td>
</tr>
<tr>
<td>excel_date</td>
<td>time</td>
<td>Convert Excel character date representation to a Date</td>
<td></td>
</tr>
<tr>
<td>month_factor</td>
<td>time</td>
<td>Month as factor/numeric</td>
<td></td>
</tr>
<tr>
<td>season_factor</td>
<td>time</td>
<td>Season as factor/numeric</td>
<td></td>
</tr>
<tr>
<td>time_aggregates</td>
<td>time</td>
<td>Timespans as character</td>
<td></td>
</tr>
<tr>
<td>time_difftimes</td>
<td>time</td>
<td>Timespans as durations</td>
<td></td>
</tr>
<tr>
<td>time_elapsed</td>
<td>time</td>
<td>Return a logical on an interval</td>
<td></td>
</tr>
<tr>
<td>time_factor</td>
<td>time</td>
<td>Turn timespans into an ordered factor</td>
<td></td>
</tr>
<tr>
<td>timespan</td>
<td>time</td>
<td>Create a timespan duration</td>
<td></td>
</tr>
<tr>
<td>week_factor</td>
<td>time</td>
<td>Day of the week as factor/numeric</td>
<td></td>
</tr>
<tr>
<td>len_unique</td>
<td>vectors</td>
<td>The length of unique values in a vector</td>
<td></td>
</tr>
<tr>
<td>names_values_switch</td>
<td>vectors</td>
<td>Switch the names and the values of a vector</td>
<td></td>
</tr>
<tr>
<td>rle_df</td>
<td>vectors</td>
<td>rle_df - create a run-length-encoding data.frame</td>
<td>Given an rle this function will return a data.frame of starts, ends, and indexes thereof of the run lengths.
Credit: https://stackoverflow.com/questions/43875716/find-start-and-end-positions-indices-of-runs-consecutive-values</td>
</tr>
<tr>
<td>rle_groups</td>
<td>vectors</td>
<td>Create an RLE Grouping from a logical vector</td>
<td></td>
</tr>
<tr>
<td>rle_seq</td>
<td>vectors</td>
<td>Create a sequence from the start to the end for a given value from an rle_df for indexing</td>
<td></td>
</tr>
<tr>
<td>sort_by_names</td>
<td>vectors</td>
<td>Sort a vector or list by it's name (or self if no names)</td>
<td></td>
</tr>
<tr>
<td>true_names</td>
<td>vectors</td>
<td>Return the names of all TRUE items in a logical vector</td>
<td></td>
</tr>
<tr>
<td>unify_vec_preserve_order</td>
<td>vectors</td>
<td>Unify two vectors preserving the order of x</td>
<td></td>
</tr>
<tr>
<td>unique_with_names</td>
<td>vectors</td>
<td>Unique a vector, preserving the names of the first original entries</td>
<td></td>
</tr>
<tr>
<td>vlookup_from_ref</td>
<td>vectors</td>
<td>Vlookup replace using a lookup column and reference table</td>
<td></td>
</tr>
<tr>
<td>zchar_remove</td>
<td>vectors</td>
<td>Remove zero length strings (or string with all spaces)</td>
<td></td>
</tr>
<tr>
<td>xpath_sibling_between</td>
<td>webscraping</td>
<td>Generate xpath to find sibling nodes between two elements
The function produces a compounding xpath with each subsequent argument provided. Thus the final argument specified will be the node that is selected by the resulting xpath with the exception of nested_tag_contains which helps to identify a nested tag by its contents</td>
<td></td>
</tr>
</tbody>
</table>
