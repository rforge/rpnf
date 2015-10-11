## Test environments

* starfox/r-devel docker container, Ubuntu 14.04, R 3.2.2 (r-devel)
* win-builder.r-project.org

## R CMD check results

* There were no ERRORs.
* There were no WARNINGs.
* There were no NOTEs.

## Feedback from last submission

Feedback from last submission has been considered:

* "Thanks, but the first part of the description is redundant for a package of that name. Better say which kind of plots are actually drawn." => Description in DESCRIPTION has been adapted.
* "Also, your examples are mostly hidden within dontrun{}, hence not executed, please add proper examples that actually get checked." => Some freely available data have been added, so that examples can be executed without on-line connection and additional packages. All exported functions now have at least one example being executed.

## Downstream dependencies

There are currently no downstream dependencies for this package, since it will be released to CRAN for the very first time.

## Acknowledgement

Thanks for the useful hints how to improve the package and perfectly align it to CRAN policies. 
Hopefully I have learned enough to improve my submission quality to an appropriate level.
