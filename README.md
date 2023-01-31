
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

<table class="table">
<thead>
<tr>
<th>Name</th>
<th>Family</th>
<th>Title</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td>all_in</td>
<td></td>
<td>Are all items in x in y?</td>
<td>
Are all items in x in y?
</td>
</tr>
<tr>
<td>as_js</td>
<td></td>
<td>Preserve a string as JS/HTML (prevent translation of characters)</td>
<td>
Preserve a string as JS/HTML (prevent translation of characters)
</td>
</tr>
<tr>
<td>assign_in_ns</td>
<td></td>
<td>Assign a variable into a namespace</td>
<td>
Unlocks and relocks namespaces and bindings as needed
</td>
</tr>
<tr>
<td>col_types</td>
<td></td>
<td>Converts input to a specified type output</td>
<td>
Given various inputs, provide a col_type specification in the format indicated by outtype
</td>
</tr>
<tr>
<td>color_cycle</td>
<td></td>
<td>Makes a cyclic color palette of a specified length using the specified transformation each cycle</td>
<td>
Makes a cyclic color palette of a specified length using the specified transformation each cycle
</td>
</tr>
<tr>
<td>color_distance</td>
<td></td>
<td>Compute color distance</td>
<td>
Compute color distance
</td>
</tr>
<tr>
<td>color_luminance</td>
<td></td>
<td>Find the luminance of a particular color, scaled 0-1</td>
<td>
Find the luminance of a particular color, scaled 0-1
</td>
</tr>
<tr>
<td>color_match</td>
<td></td>
<td>Match colors by visual distance</td>
<td>
Helpful for pairing colors across light/dark palettes
</td>
</tr>
<tr>
<td>color_rgb_table</td>
<td></td>
<td>Convert vector of colors to named tbl</td>
<td>
Convert vector of colors to named tbl
</td>
</tr>
<tr>
<td>color_separate</td>
<td></td>
<td>Separate a vector of colors based on their distance</td>
<td>
Separate a vector of colors based on their distance
</td>
</tr>
<tr>
<td>colors2css</td>
<td></td>
<td>Convert a list of colors to SCSS/Sass variables or classes</td>
<td>
Convert a list of colors to SCSS/Sass variables or classes
</td>
</tr>
<tr>
<td>common_names</td>
<td></td>
<td>Find the names in common</td>
<td>
Given named objects, find the names in common
</td>
</tr>
<tr>
<td>creds_to_renviron</td>
<td></td>
<td>Write named keypairs to an .Renviron / .Rprofile file</td>
<td>
Writes key pairs to .Renviron / .Rprofile and adds .Renviron to .gitignore if not already there.
</td>
</tr>
<tr>
<td>css_col2vec</td>
<td></td>
<td>Convert a CSS representation of a color to an r,g,b numeric</td>
<td>
Convert a CSS representation of a color to an r,g,b numeric
</td>
</tr>
<tr>
<td>dep_read</td>
<td></td>
<td>Read a dependency from file</td>
<td>
Read a dependency from file
</td>
</tr>
<tr>
<td>dep_write</td>
<td></td>
<td>Write a dependency to file</td>
<td>
Write a dependency to file
</td>
</tr>
<tr>
<td>dir_fn</td>
<td></td>
<td>Create a directory path pointing function</td>
<td>
Create a directory path pointing function
</td>
</tr>
<tr>
<td>dirs</td>
<td></td>
<td>Path functions for commonly used directories</td>
<td>
Path functions for commonly used directories
</td>
</tr>
<tr>
<td>.file</td>
<td></td>
<td>All the files that can be navigated to with file if they exist</td>
<td>
All the files that can be navigated to with file if they exist
</td>
</tr>
<tr>
<td>duration_print</td>
<td></td>
<td>Translate a duration into the human-legible estimation as a character</td>
<td>
Translate a duration into the human-legible estimation as a character
</td>
</tr>
<tr>
<td>excel_date</td>
<td></td>
<td>Convert Excel character date representation to a Date</td>
<td>
Convert Excel character date representation to a Date
</td>
</tr>
<tr>
<td>expr_pipe</td>
<td></td>
<td>Return a list of expressions all piped together as a single expression</td>
<td>
Return a list of expressions all piped together as a single expression
</td>
</tr>
<tr>
<td>ext</td>
<td></td>
<td>Extract the file extensions from a filepath</td>
<td>
Given a path, extract the file extension
</td>
</tr>
<tr>
<td>file_fn</td>
<td></td>
<td>Return the appropriate function for reading the specified path/extension</td>
<td>
Return the appropriate function for reading the specified path/extension
</td>
</tr>
<tr>
<td>file</td>
<td></td>
<td>Go to a specified file in the Virga Labs golem project</td>
<td>
A List object with convenience functions that open the named file in RStudio. An R named sublist of all files in the R folder if such a folder exists
</td>
</tr>
<tr>
<td>filter_to</td>
<td></td>
<td>Change or apply filters to output type</td>
<td>
Useful in concert with axis brushing
</td>
</tr>
<tr>
<td>find_by_class</td>
<td></td>
<td>Find an object by it's class</td>
<td>
Find an object by it's class
</td>
</tr>
<tr>
<td>fn_name</td>
<td></td>
<td>Retrieve the function name</td>
<td>
Sometimes a function is passed down the call stack and it's name is unknown. This function finds the name without having to pass it down the call stack as an argument.
</td>
</tr>
<tr>
<td>folder</td>
<td></td>
<td>Go to a specified folder in the Virga Labs golem project</td>
<td>
Go to a specified folder in the Virga Labs golem project
</td>
</tr>
<tr>
<td>fun_docs_table</td>
<td></td>
<td>Create a table of functions and their uses</td>
<td>
Create a table of functions and their uses
</td>
</tr>
<tr>
<td>gbort</td>
<td></td>
<td>Custom error message</td>
<td>
Throw abort with format_error
</td>
</tr>
<tr>
<td>get_from_ns</td>
<td></td>
<td>Get an object from a namespace</td>
<td>
Get an object from a namespace
</td>
</tr>
<tr>
<td>get_global</td>
<td></td>
<td>Get an object from the global environment</td>
<td>
Get an object from the global environment
</td>
</tr>
<tr>
<td>get_package_fns</td>
<td></td>
<td>Get the names of all exported functions in a package</td>
<td>
Get the names of all exported functions in a package
</td>
</tr>
<tr>
<td>glue_js</td>
<td></td>
<td>Create a JS string with glue insertions
glue .open = !@ &amp; .close = @#</td>
<td>
Create a JS string with glue insertions
glue .open = !@ &amp; .close = @#
</td>
</tr>
<tr>
<td>gmsg</td>
<td></td>
<td>Custom message
Message using format_message &amp; cat_line</td>
<td>
Custom message
Message using format_message &amp; cat_line
</td>
</tr>
<tr>
<td>%nin%</td>
<td></td>
<td>Are lhs values absent from set on rhs?</td>
<td>
Are lhs values absent from set on rhs?
</td>
</tr>
<tr>
<td>%|0|%</td>
<td></td>
<td>Replace a 0 length value</td>
<td>
If the lhs is length 0, replace with rhs
</td>
</tr>
<tr>
<td>%|legit|%</td>
<td></td>
<td>If legit lhs, else rhs</td>
<td>
If legit lhs, else rhs
</td>
</tr>
<tr>
<td>%|zchar|%</td>
<td></td>
<td>Replace zero-length character strings with right hand side</td>
<td>
Replace zero-length character strings with right hand side
</td>
</tr>
<tr>
<td>gwarn</td>
<td></td>
<td>Custom warning message</td>
<td>
Throw cli_alert_warning with format_warning
</td>
</tr>
<tr>
<td>if_debug</td>
<td></td>
<td>Run expressions only when option use_debug = TRUE</td>
<td>
Run expressions only when option use_debug = TRUE
</td>
</tr>
<tr>
<td>ignore_files</td>
<td></td>
<td>Add lines to .gitignore</td>
<td>
Add lines to .gitignore
</td>
</tr>
<tr>
<td>inequality_key</td>
<td></td>
<td>Inequality conversion key</td>
<td>
Inequality conversion key
</td>
</tr>
<tr>
<td>install_remote</td>
<td></td>
<td>Install a package</td>
<td>
Install a package
</td>
</tr>
<tr>
<td>is_error</td>
<td></td>
<td>Is object an error</td>
<td>
Is object an error
</td>
</tr>
<tr>
<td>is_filepath</td>
<td></td>
<td>Is path a file path</td>
<td>
Given a path, is it a filepath?
</td>
</tr>
<tr>
<td>is_legit</td>
<td></td>
<td>Is object legit?</td>
<td>
Is object non-null, non-empty, non-NA, and not a try-error?
</td>
</tr>
<tr>
<td>is_package_dev</td>
<td></td>
<td>Is package in development or installed</td>
<td>
Is package in development or installed
</td>
</tr>
<tr>
<td>is_package</td>
<td></td>
<td>Is working directory a package?</td>
<td>
Is working directory a package?
</td>
</tr>
<tr>
<td>join_check</td>
<td></td>
<td>Detect possible duplicates of rows or columns after a join</td>
<td>
Detect possible duplicates of rows or columns after a join
</td>
</tr>
<tr>
<td>key_out</td>
<td></td>
<td>Handle different output type requests for match_df</td>
<td>
Handle different output type requests for match_df
</td>
</tr>
<tr>
<td>key_pairs_duplicated</td>
<td></td>
<td>Find duplicates in key pairs</td>
<td>
Find duplicates in key pairs
</td>
</tr>
<tr>
<td>key_pairs_text</td>
<td></td>
<td>Make key-pairs from a named character vector</td>
<td>
Make key-pairs from a named character vector
</td>
</tr>
<tr>
<td>last_updated</td>
<td></td>
<td>Gather last updated times for on-disk files</td>
<td>
Check the last modified time files or paths
</td>
</tr>
<tr>
<td>len_unique</td>
<td></td>
<td>The length of unique values in a vector</td>
<td>
The length of unique values in a vector
</td>
</tr>
<tr>
<td>list.files2</td>
<td></td>
<td>List full file paths with the file name as the name</td>
<td>
List full file paths with the file name as the name
</td>
</tr>
<tr>
<td>luminance_filter</td>
<td></td>
<td>Filter colors based on a luminance threshold</td>
<td>
Filter colors based on a luminance threshold
</td>
</tr>
<tr>
<td>magnitude_order</td>
<td></td>
<td>Compute the order of magnitude</td>
<td>
Uses the floor to round
</td>
</tr>
<tr>
<td>magnitude_triplet</td>
<td></td>
<td>Compute the order of magnitude triplet ie thousand, million, trillion</td>
<td>
Compute the order of magnitude triplet ie thousand, million, trillion
</td>
</tr>
<tr>
<td>make_names</td>
<td></td>
<td>Make a file path name with underscores</td>
<td>
Make a file path name with underscores
</td>
</tr>
<tr>
<td>map_class</td>
<td></td>
<td>Match the classes of one object to that of another object</td>
<td>
Match the classes of one object to that of another object
</td>
</tr>
<tr>
<td>match_df</td>
<td></td>
<td>Extract matching rows of a data frame.</td>
<td>
Match works in the same way as join, but instead of return the combined
dataset, it only returns the matching rows from the first dataset. This is
particularly useful when you've summarised the data in some way
and want to subset the original data by a characteristic of the subset.
</td>
</tr>
<tr>
<td>match_letters</td>
<td></td>
<td>Match the first n letters to supplied arguments</td>
<td>
Case insensitive matching of argument to possibilities provided in ellipsis.
</td>
</tr>
<tr>
<td>max2</td>
<td></td>
<td>An alternative to max that preserves names</td>
<td>
An alternative to max that preserves names
</td>
</tr>
<tr>
<td>missing_args</td>
<td></td>
<td>Get the missing arguments from the function as character</td>
<td>
Get the missing arguments from the function as character
</td>
</tr>
<tr>
<td>mkpath</td>
<td></td>
<td>Construct a path</td>
<td>
Given a path, construct it if it does not exist.
</td>
</tr>
<tr>
<td>most</td>
<td></td>
<td>Are most values TRUE</td>
<td>
IF more than half the values are TRUE, returns TRUE
</td>
</tr>
<tr>
<td>move_js_to_folder</td>
<td></td>
<td>Move all js files to js folder</td>
<td>
Move all js files to js folder
</td>
</tr>
<tr>
<td>need_pkg</td>
<td></td>
<td>Get a function from a package, abort if package not installed.</td>
<td>
Get a function from a package, abort if package not installed.
</td>
</tr>
<tr>
<td>needs_update</td>
<td></td>
<td>Check if files need to be updated</td>
<td>
Check if files need to be updated
</td>
</tr>
<tr>
<td>nonull</td>
<td></td>
<td>Is value non-null?</td>
<td>
Is value non-null?
</td>
</tr>
<tr>
<td>not_na</td>
<td></td>
<td>Is value non-NA?</td>
<td>
Is value non-NA?
</td>
</tr>
<tr>
<td>num_chr_suffi</td>
<td></td>
<td>Abbreviations of numeric magnitude</td>
<td>
Abbreviations of numeric magnitude
</td>
</tr>
<tr>
<td>num2str_vec</td>
<td></td>
<td>Convert number to string Vectorized version</td>
<td>
Convert number to string Vectorized version
</td>
</tr>
<tr>
<td>num2str</td>
<td></td>
<td>Convert numeric value to a string abbreviation with K, M, B for Thousand, Million &amp; Billion</td>
<td>
Convert numeric value to a string abbreviation with K, M, B for Thousand, Million &amp; Billion
</td>
</tr>
<tr>
<td>object_ext</td>
<td></td>
<td>Provide the appropriate file extension for a given object</td>
<td>
Provide the appropriate file extension for a given object
</td>
</tr>
<tr>
<td>object_fn</td>
<td></td>
<td>Return the appropriate function for writing the supplied object to disk</td>
<td>
Return the appropriate function for writing the supplied object to disk
</td>
</tr>
<tr>
<td>object_write</td>
<td></td>
<td>Provide the appropriate file read/write function</td>
<td>
Write an object to disk
</td>
</tr>
<tr>
<td>opts</td>
<td></td>
<td>Check option value. Use interactively only!</td>
<td>
Check option value. Use interactively only!
</td>
</tr>
<tr>
<td>pkg_ns</td>
<td></td>
<td>Return the current package namespace</td>
<td>
Return the current package namespace
</td>
</tr>
<tr>
<td>profile_script</td>
<td></td>
<td>profile_script</td>
<td>
This function will add profiling code to a script wherever the following flags are found in the first non-spacing characters on the line:

#&lt;p Opening comment flag where profile_open will be inserted.
#&gt;p Closing comment flag where profile_close will be inserted.

</td>
</tr>
<tr>
<td>read_js</td>
<td></td>
<td>Read Javascript file</td>
<td>
Read Javascript file
</td>
</tr>
<tr>
<td>reexports</td>
<td></td>
<td>Objects exported from other packages</td>
<td>
These objects are imported from other packages. Follow the links
below to see their documentation.


  plyrround_any

  rlang%|%, %||%
</td>
</tr>
<tr>
<td>regex_op</td>
<td></td>
<td>Create a compound regex grouped statement</td>
<td>
Create a compound regex grouped statement
</td>
</tr>
<tr>
<td>regex_or</td>
<td></td>
<td>Create a compound regex grouped OR statement</td>
<td>
Create a compound regex grouped OR statement
</td>
</tr>
<tr>
<td>rgb2hex</td>
<td></td>
<td>Convert r,g,b,a values as string or numeric to hex</td>
<td>
Convert r,g,b,a values as string or numeric to hex
</td>
</tr>
<tr>
<td>rle_df</td>
<td></td>
<td>rle_df - create a run-length-encoding data.frame</td>
<td>
Given an rle this function will return a data.frame of starts, ends, and indexes thereof of the run lengths.
Credit: https://stackoverflow.com/questions/43875716/find-start-and-end-positions-indices-of-runs-consecutive-values
</td>
</tr>
<tr>
<td>rle_seq</td>
<td></td>
<td>Create a sequence from the start to the end for a given value from an rle_df for indexing</td>
<td>
Create a sequence from the start to the end for a given value from an rle_df for indexing
</td>
</tr>
<tr>
<td>round_to</td>
<td></td>
<td>Find the global minima/maxima of input vectors</td>
<td>
If accuracy is omitted, number will be rounded to the nearest order of magnitude IE 145, if fn = min, will round to 100
</td>
</tr>
<tr>
<td>shiny_error_recover</td>
<td></td>
<td>Toggle recover on error when obtuse shiny errors are encountered</td>
<td>
Toggle recover on error when obtuse shiny errors are encountered
</td>
</tr>
<tr>
<td>size</td>
<td></td>
<td>Digital storage size conversion
See object.size</td>
<td>
Digital storage size conversion
See object.size
</td>
</tr>
<tr>
<td>smode</td>
<td></td>
<td>Statistical mode</td>
<td>
Return the most frequenctly occuring item in a dataset
</td>
</tr>
<tr>
<td>startup</td>
<td></td>
<td>Load project &amp; user-level .Renviron &amp; .Rprofile</td>
<td>
Load project &amp; user-level .Renviron &amp; .Rprofile
</td>
</tr>
<tr>
<td>str_break_every</td>
<td></td>
<td>Break word every x characters</td>
<td>
Break word every x characters
</td>
</tr>
<tr>
<td>str_inequality</td>
<td></td>
<td>Convert inequality statements between character, mathematic, symbol and function representations</td>
<td>
Convert inequality statements between character, mathematic, symbol and function representations
</td>
</tr>
<tr>
<td>`%|try|%`</td>
<td></td>
<td>Try an expression</td>
<td>
Calls the expression (LHS) &amp; if it fails return RHS
</td>
</tr>
<tr>
<td>time_elapsed</td>
<td></td>
<td>Return a logical on an interval</td>
<td>
Return a logical on an interval
</td>
</tr>
<tr>
<td>toggle</td>
<td></td>
<td>Toggle an option</td>
<td>
Meant for interactive use only.
</td>
</tr>
<tr>
<td>unit_conversion</td>
<td></td>
<td>Abbreviations of numeric magnitude for various units</td>
<td>
Abbreviations of numeric magnitude for various units
</td>
</tr>
<tr>
<td>unit_find</td>
<td></td>
<td>Find the row corresponding to a value in unit_conversion</td>
<td>
Find the row corresponding to a value in unit_conversion
</td>
</tr>
<tr>
<td>unit_modify_vec</td>
<td></td>
<td>Modify unit abbreviation, vectorized version</td>
<td>
Modify unit abbreviation, vectorized version
</td>
</tr>
<tr>
<td>unit_modify</td>
<td></td>
<td>Modify unit abbreviations</td>
<td>
Modify unit abbreviations
</td>
</tr>
<tr>
<td>unit_string</td>
<td></td>
<td>Extract the units from a string</td>
<td>
It is assumed that units are encased in parentheses at the end of the string
</td>
</tr>
<tr>
<td>unload_namespaces</td>
<td></td>
<td>Unload namespaces prior to package install</td>
<td>
Unload namespaces prior to package install
</td>
</tr>
<tr>
<td>use_reimport</td>
<td></td>
<td>Add a function to reimports</td>
<td>
Add a function to reimports
</td>
</tr>
<tr>
<td>use_UU_reimports</td>
<td></td>
<td>Write R/aaa_reimports.R file</td>
<td>
Write R/aaa_reimports.R file
</td>
</tr>
<tr>
<td>vlookup_from_ref</td>
<td></td>
<td>Vlookup replace using a lookup column and reference table</td>
<td>
Vlookup replace using a lookup column and reference table
</td>
</tr>
<tr>
<td>week_factor</td>
<td></td>
<td>Get numeric day of the week</td>
<td>
Get numeric day of the week
</td>
</tr>
<tr>
<td>write_dir_fn</td>
<td></td>
<td>Write dir helper function that are robust to dev vs deployed package states</td>
<td>
Write dir helper function that are robust to dev vs deployed package states
</td>
</tr>
<tr>
<td>write_opts</td>
<td></td>
<td>Write all the option checking functions to a file</td>
<td>
Write all the option checking functions to a file
</td>
</tr>
<tr>
<td>write_to_rprofile</td>
<td></td>
<td>Write expressions to the .Rprofile</td>
<td>
Write expressions to the .Rprofile
</td>
</tr>
<tr>
<td>xpath_sibling_between</td>
<td></td>
<td>Generate xpath to find sibling nodes between two elements
The function produces a compounding xpath with each subsequent argument provided. Thus the final argument specified will be the node that is selected by the resulting xpath with the exception of nested_tag_contains which helps to identify a nested tag by its contents</td>
<td>
Generate xpath to find sibling nodes between two elements
The function produces a compounding xpath with each subsequent argument provided. Thus the final argument specified will be the node that is selected by the resulting xpath with the exception of nested_tag_contains which helps to identify a nested tag by its contents
</td>
</tr>
<tr>
<td>zchar_remove</td>
<td></td>
<td>Remove zero length strings (or string with all spaces)</td>
<td>
Remove zero length strings (or string with all spaces)
</td>
</tr>
<tr>
<td>zchar</td>
<td></td>
<td>Is zero-length character?</td>
<td>
Is zero-length character?
</td>
</tr>
</tbody>
</table>
