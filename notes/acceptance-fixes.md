# 0.2.0 acceptance fixes

- [x] DESCRIPTION metadata is accepted by `R CMD build` and `R CMD check`.
- [x] `bb_segment()`, `bb_errorbar()`, and `bb_tile()` validate mapped and literal aesthetics against the number of rows drawn.
- [x] Jitter seeds reject missing, non-finite, multi-value, and integer-overflow inputs.
- [x] `make install` installs version 0.2.0 successfully.
- [x] `make check` builds the source package and reports 0 errors, 0 warnings, and 0 notes. The optional CRAN incoming-index query timed out in this environment.
