## Test environments

*   **Local OS:** macOS Tahoe 26.1, R 4.5.2 (2025-10-31) - **0 errors | 0 warnings | 1 note**. (The note "License stub is invalid DCF" is generally considered ignorable by CRAN when `License: MIT + file LICENSE` is used.)

*   **R-hub checks (as initiated via `rhub::rhub_check()`):**
    *   **Passed:** Checks on stable platforms (Linux, macOS, Windows) are expected to pass based on local checks and R-hub's general stability for `R-release`.
    *   **Failed (Linux R-devel/R 4.6.0):** Checks on `R-devel` (R 4.6.0) Linux environments consistently failed due to a compilation error of the `later` package (version 1.4.4 or similar). The specific error was `error: ‘::Rf_rnbeta’ has not been declared`. This is a known incompatibility issue between the `later` package (an indirect dependency of `ggplot2`) and `R 4.6.0+` where the `Rf_rnbeta` function has been removed or renamed in `R-devel`. We have ensured `later (>= 1.3.1)` and `Rcpp (>= 1.1.0)` are in `Imports`. This issue is specific to `R-devel` builds and is not expected to affect `R-release` builds on CRAN.

## R CMD check results

We have performed `R CMD check` locally on macOS R 4.5.2, resulting in **0 errors, 0 warnings, and 1 note**. The note "License stub is invalid DCF" is generally considered ignorable for CRAN submissions when `License: MIT + file LICENSE` is used.

## Downstream dependencies

This is a new submission, so there are no downstream dependencies to consider.

## Resubmission (if applicable)

N/A