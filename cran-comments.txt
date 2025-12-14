## Resubmission

This is a resubmission. In the previous submission, we encountered installation `ERROR`s on `R-devel` platforms.

*   The installation `ERROR`s were due to an incorrect `Rcpp` dependency. This has been fixed, and the package now installs successfully on all tested platforms.
*   We have also addressed a `NOTE` about a possible misspelling of "translational" by adding it to an `inst/WORDLIST` file.

## Test environments

*   **Local:** macOS 14.1 (aarch64), R 4.5.2: **0 errors, 0 warnings, 1 NOTE** (`License stub is invalid DCF`).
*   **R-hub `r-devel` checks:**
    *   `windows-r-devel`: **Success**
    *   `linux-r-devel`: **Success**
*   **win-builder `r-devel`:** **Success**

## R CMD check results

The package now passes `R CMD check` on R-devel for both Windows and Linux. The only remaining `NOTE` is `License stub is invalid DCF`, which is expected and ignorable for packages using the `MIT + file LICENSE` structure. The previous installation `ERROR` is resolved.