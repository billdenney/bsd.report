context("knit_print.formula")

test_that("knit_print.formula, simple formulas work", {
  expect_equal(
    knit_print.formula(a~b),
    knitr::asis_output(
      "$a \\sim b$",
      cacheable=TRUE
    )
  )
})

test_that("knit_print.formula, respects inline", {
  expect_equal(
    knit_print.formula(a~b, inline=TRUE),
    knitr::asis_output(
      "$a \\sim b$",
      cacheable=TRUE
    )
  )

  expect_equal(
    knit_print.formula(a~b, inline=FALSE),
    knitr::asis_output(
      "$$a \\sim b$$",
      cacheable=TRUE
    )
  )
})

test_that("knit_print.formula, one- and two-sided formulas work", {
  expect_equal(
    knit_print.formula(~b),
    knitr::asis_output(
      "$\\sim b$",
      cacheable=TRUE
    )
  )

  expect_equal(
    knit_print.formula(a~b),
    knitr::asis_output(
      "$a \\sim b$",
      cacheable=TRUE
    )
  )

  expect_equal(
    knit_print.formula(a~b~c),
    knitr::asis_output(
      "${a}\\sim{b} \\sim c$",
      cacheable=TRUE
    )
  )
})

test_that("knit_print.formula, substitutions work as expected", {
  expect_equal(
    knit_print.formula(a~b),
    knitr::asis_output(
      "$a \\sim b$",
      cacheable=TRUE
    ),
    info="~ substitution"
  )
  
  expect_equal(
    knit_print.formula(a~b %in% c),
    knitr::asis_output(
      "$a \\sim {b} \\in {c}$",
      cacheable=TRUE
    ),
    info="%in% substitution"
  )

  expect_equal(
    knit_print.formula(a~b != c),
    knitr::asis_output(
      "$a \\sim {b} \\neq {c}$",
      cacheable=TRUE
    ),
    info="!= substitution"
  )

  expect_equal(
    knit_print.formula(a~b * c),
    knitr::asis_output(
      "$a \\sim {b} \\times {c}$",
      cacheable=TRUE
    ),
    info="* substitution"
  )
})

test_that("knit_print.formula, call substitution works as expected", {
  expect_equal(
    knit_print.formula(a ~ b %% c),
    knitr::asis_output(
      "$a \\sim {b} \\bmod {c}$",
      cacheable=TRUE
    )
  )

  expect_equal(
    knit_print.formula(a ~ b ^ c),
    knitr::asis_output(
      "$a \\sim {b}^{c}$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ b:c),
    knitr::asis_output(
      "$a \\sim {b}:{c}$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ b*c),
    knitr::asis_output(
      "$a \\sim {b} \\times {c}$",
      cacheable=TRUE
    )
  )

  expect_equal(
    knit_print.formula(a ~ b<c),
    knitr::asis_output(
      "$a \\sim {b}<{c}$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ b<=c),
    knitr::asis_output(
      "$a \\sim {b}\\leq{c}$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ b>c),
    knitr::asis_output(
      "$a \\sim {b}>{c}$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ b>=c),
    knitr::asis_output(
      "$a \\sim {b}\\geq{c}$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ b==c),
    knitr::asis_output(
      "$a \\sim {b}\\equiv{c}$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ b!=c),
    knitr::asis_output(
      "$a \\sim {b} \\neq {c}$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ b|c),
    knitr::asis_output(
      "$a \\sim {b}|{c}$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ b||c),
    knitr::asis_output(
      "$a \\sim {b}||{c}$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ b&c),
    knitr::asis_output(
      "$a \\sim {b}&{c}$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ b&&c),
    knitr::asis_output(
      "$a \\sim {b}&&{c}$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ b/c),
    knitr::asis_output(
      "$a \\sim \\frac{b}{c}$",
      cacheable=TRUE
    )
  )
})

test_that("knit_print.formula, function calls work with 0 or more arguments", {
  expect_equal(
    knit_print.formula(a ~ b()),
    knitr::asis_output(
      "$a \\sim b()$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ b(c)),
    knitr::asis_output(
      "$a \\sim b\\left(c\\right)$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ b(c,d)),
    knitr::asis_output(
      "$a \\sim b\\left(c, d\\right)$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ b(c,d,e)),
    knitr::asis_output(
      "$a \\sim b\\left(c, d, e\\right)$",
      cacheable=TRUE
    )
  )
})

test_that("knit_print.formula, other classes within formula", {
  expect_equal(
    knit_print.formula(a ~ 1),
    knitr::asis_output(
      "$a \\sim 1$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ "A"),
    knitr::asis_output(
      "$a \\sim \\textrm{``A''}$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ "ABCD"),
    knitr::asis_output(
      "$a \\sim \\textrm{``ABCD''}$",
      cacheable=TRUE
    )
  )

  expect_equal(
    knit_print.formula(a ~ TRUE),
    knitr::asis_output(
      "$a \\sim \\textrm{TRUE}$",
      cacheable=TRUE
    )
  )
  
  expect_equal(
    knit_print.formula(a ~ NA),
    knitr::asis_output(
      "$a \\sim \\textrm{NA}$",
      cacheable=TRUE
    )
  )
})

test_that("knit_print.formula, cleaning", {
  expect_equal(
    knit_print.formula(a ~ b_c),
    knitr::asis_output(
      "$a \\sim b\\_c$",
      cacheable=TRUE
    )
  )

  expect_equal(
    knit_print.formula(a ~ b %foo% c),
    knitr::asis_output(
      "$a \\sim {b}\\%foo\\%{c}$",
      cacheable=TRUE
    )
  )
})

test_that("knit_print.formula, ( work correctly", {
  expect_equal(
    knit_print.formula(a ~ (b)),
    knitr::asis_output(
      "$a \\sim \\left( b \\right)$",
      cacheable=TRUE
    )
  )
})

test_that("knit_print.formula, replacements work", {
  expect_equal(
    knit_print.formula(a ~ (b), replacements=list(b="c_{d}")),
    knitr::asis_output(
      "$a \\sim \\left( c_{d} \\right)$",
      cacheable=TRUE
    )
  )
})