## Resubmission Part 3

No changes have been made since the last resubmission on March 11, 2024, however we haven't received any communication since then and wanted to resubmit in case the last submission wasn't received. If the 3/11/24 submission is still in process please ignore!

## Resubmission Part 2

This is a second resubmission. In this version I have:

*   Further clarified "TNTP" in the R CMD check comments below (short version: it isn't an acronym anymore).
*   Changed DESCRIPTION to use an Authors@R: field
*   Re-wrote the Description: field in DESCRIPTION based on feedback from the last submission.

## Resubmission

This is a resubmission. In this version I have:

*   Clarified "TNTP" in the R CMD check comments below
*   Added documentation of values to theme_tntp.Rd and theme_tntp_2018.Rd
*   Removed \dontrun{} tags from all examples
*   Fixed the error in setup_subdirectory.Rd example code
*   Adjusted the code for setup_repo() and setup_subdirectory() so they no longer include a default path of getwd()

A comment on the original submission said: 

*"If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form authors (year) <doi:...> authors (year) <arXiv:...> authors (year, ISBN:...) or if those are not available: <[https:...]https:...> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking. (If you want to add a title as well please put it in quotes: "Title")"*

This package doesn't reference academic publications, so there are no references to document.

## R CMD check results

0 errors | 0 warnings | 1 notes

*   Note: the word "TNTP" in README.md and in DESCRIPTION is the name of an organization, not an acronym or mis-spelled word. Although TNTP used to stand for "The New Teacher Project", that name was dropped around 2013 (as the organization branched out into areas beyond new teacher training) and the current name is not an acronym for anything.
*   This is a new release.
