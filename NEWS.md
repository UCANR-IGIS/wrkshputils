# wrkshputils 0.3.0 (2022-08-06)

* `wu_gfr_elems()`, `wu_gfr_report()`, `wu_gfr_snippet()`, `wu_gfr_taglist()`, `wu_img_build()`, `wu_img_maketrans()`, and `wu_meme()` removed (split off to a new slideutils package) to reduce the number of dependencies and leave only those functions needed by students in workshops.

# wrkshputils 0.2.7 (2022-08-05)

* `wu_img_build()`: Added check for final slash on `base_dir` argument.

# wrkshputils 0.2.6 (2022-06-19)

* `wu_meme()`: New function

# wrkshputils 0.2.5 (2022-05-21)

* `wu_download()`: Additional messages provided

# wrkshputils 0.2.4 (2022-05-17)

* `wu_download()`: New function
* `magick`, `leaflet` and `ggplot2` packages moved from Imports to Suggests to make the package lighter to install for students

# wrkshputils 0.2.3 (2022-05-10)

* `wu_img_build()`: Added `base_dir` argument.

# wrkshputils 0.2.2 (2022-04-28)

* `wu_img_build()`: Added `include_css` argument.

# wrkshputils 0.2.1 (2022-04-21)

* `wu_img_maketrans()`: Arguments overhauled to make it simpler to call. See function help page.

# wrkshputils 0.2.0 (2022-02-20)

* `wu_img_maketrans()`: New function to make a folder of PNG images have transparent background
* `wu_img_build()`: New function, returns a taglist
* `wu_gfr_elems()`: Added `skip`, `pie`, and `map` as an element types
* `wu_gfr_taglist()`: New function, generates HTML tags for a gfr_elems object
* `wu_gfr_snippet()`: New function, similar to `wu_gfr_report()` but generates a snippet of HTML code to be inserted in a HTML slide deck

# wrkshputils 0.1.4 (2022-02-10)

* `wu_popup()`: Added `bgcol` argument; Fixed an error trapping bug

# wrkshputils 0.1.3 (2022-02-10)

* `wu_popup()`: Updated to use RStudio Viewer; removed dependency on `imager`

# wrkshputils 0.1.2 (2022-02-04)

* `wu_popup()`: New function to plot an image from a URL

# wrkshputils 0.1.1 (2021-11-09)

* Added support for `howmany_lbl` column - adds "Pick one" or "Select all that apply". 
* Paragraph text now supports HTML tags


# wrkshputils 0.1.0

* Initial version. `wu_grf_elems()` and `wu_gfr_report()` developed to create a HTML report of a Google Form Survey.
