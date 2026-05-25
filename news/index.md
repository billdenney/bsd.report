# Changelog

## bsd.report (development version)

### Removed functions

- `knit_print.gg()`, `knit_print.gg_list()`, and `as_gg_list()` have
  been removed. These are superseded by the ggtibble package.

- [`patch_data()`](https://billdenney.github.io/bsd.report/reference/patch_data.md)
  has been removed. Use
  [`dplyr::rows_patch()`](https://dplyr.tidyverse.org/reference/rows.html)
  to replace only NA values, or
  [`dplyr::rows_update()`](https://dplyr.tidyverse.org/reference/rows.html)
  to replace all matched values.

- `latex_reference()`, `latex_label()`, `latex_label_clean()`, and
  `latex_label_first_last()` have been removed.

- `logLik.xpose_data()` has been removed.

- [`realize_addl_single()`](https://billdenney.github.io/bsd.report/reference/realize_addl_single.md)
  has been removed. Use `mrgsolve::realize_addl()` instead.
