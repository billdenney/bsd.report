context("replace_synonym")

test_that("replace_synonym.character works", {
  expect_equal(
    replace_synonym(
      c("A", "B", "C", "a"),
      c(A="apple", B="bear", C="cabbage")
    ),
    c("apple", "bear", "cabbage", "apple")
  )
  expect_equal(
    replace_synonym(
      c("A", "B", "C", "a"),
      c(A="apple", B="bear", C="cabbage"),
      ignore_case=FALSE
    ),
    c("apple", "bear", "cabbage", "a")
  )
  expect_equal(
    replace_synonym(
      c("A", "B", "C", "D"),
      c(A="apple", B="bear", C="cabbage"),
      ignore_case=FALSE
    ),
    c("apple", "bear", "cabbage", "D"),
    info="All returned values are not accidentally lower case."
  )
})

test_that("replace_synonym.data.frame works", {
  expect_equal(
    replace_synonym.data.frame(
      x=
        data.frame(
          A=rep(c("A", "B"), each=2),
          B=letters[1:4],
          stringsAsFactors=FALSE
        ),
      synonyms=
        data.frame(
          A="A",
          Column="B",
          Verbatim=c("a", "c"),
          Preferred=c("apple", "cherry"),
          stringsAsFactors=FALSE
        )
    ),
    data.frame(
      A=rep(c("A", "B"), each=2),
      B=c("apple", "b", "c", "d"),
      stringsAsFactors=FALSE
    )
  )
  expect_equal(
    replace_synonym.data.frame(
      x=
        data.frame(
          A=rep(c("A", "B"), each=2),
          B=letters[1:4],
          stringsAsFactors=FALSE
        ),
      synonyms=
        data.frame(
          A=rep(c("A", "B"), each=2),
          Column="B",
          Verbatim=c("a", "c", "b", "d"),
          Preferred=c("apple", "cherry", "berry", "durian"),
          stringsAsFactors=FALSE
        )
    ),
    data.frame(
      A=rep(c("A", "B"), each=2),
      B=c("apple", "b", "c", "durian"),
      stringsAsFactors=FALSE
    )
  )
})

test_that("replace_synonym.character errors appropriately", {
  expect_error(
    replace_synonym.character(1, 1),
    regexp="`x` must be a character vector",
    fixed=TRUE
  )
  expect_error(
    replace_synonym.character("A", 1),
    regexp="`synonyms` must be a character vector.",
    fixed=TRUE
  )
  expect_error(
    replace_synonym.character("A", "A"),
    regexp="`synonyms` must be named.",
    fixed=TRUE
  )
  expect_error(
    replace_synonym.character("A", c(a="A", "B")),
    regexp="All `synonyms` must be named, and no names may be blank.",
    fixed=TRUE
  )
  expect_error(
    replace_synonym.character("A", c(a="A", a="B")),
    regexp="All names of `synonyms` (verbatim values) must be unique.",
    fixed=TRUE
  )
})

test_that("replace_synonym.data.frame errors appropriately", {
  expect_error(
    replace_synonym.data.frame(1, 1),
    regexp="`synonyms` must be a data.frame",
    fixed=TRUE
  )
  expect_error(
    replace_synonym.data.frame(
      x=data.frame(A="A"),
      synonyms=data.frame(Column=1, Verbatim=1, Preferred=1, stringsAsFactors=FALSE)
    ),
    regexp="`synonyms[[replacement_column]]` must be a character column.",
    fixed=TRUE
  )
  expect_error(
    replace_synonym.data.frame(
      x=data.frame(A="A"),
      synonyms=data.frame(Column="A", Verbatim=1, Preferred=1, stringsAsFactors=FALSE)
    ),
    regexp="`synonyms[[verbatim_column]]` must be a character column.",
    fixed=TRUE
  )
  expect_error(
    replace_synonym.data.frame(
      x=data.frame(A="A"),
      synonyms=data.frame(Column="A", Verbatim="A", Preferred=1, stringsAsFactors=FALSE)
    ),
    regexp="`synonyms[[preferred_column]]` must be a character column.",
    fixed=TRUE
  )
  expect_error(
    replace_synonym.data.frame(
      x=data.frame(A="A"),
      synonyms=data.frame(Column="A", Verbatim="A", Preferred="A", stringsAsFactors=FALSE)
    ),
    regexp="`x` must be a character vector",
    fixed=TRUE
  )
  expect_error(
    replace_synonym.data.frame(
      x=data.frame(A="A", stringsAsFactors=FALSE),
      synonyms=data.frame(Column="A", Verbatim="A", Preferred="A", stringsAsFactors=FALSE),
      replacement_column=c("A", "B")
    ),
    regexp="`replacement_column` must be a scalar character string",
    fixed=TRUE
  )
  expect_error(
    replace_synonym.data.frame(
      x=data.frame(A="A", stringsAsFactors=FALSE),
      synonyms=data.frame(Column="A", Verbatim="A", Preferred="A", stringsAsFactors=FALSE),
      verbatim_column=c("A", "B")
    ),
    regexp="`verbatim_column` must be a scalar character string",
    fixed=TRUE
  )
  expect_error(
    replace_synonym.data.frame(
      x=data.frame(A="A", stringsAsFactors=FALSE),
      synonyms=data.frame(Column="A", Verbatim="A", Preferred="A", stringsAsFactors=FALSE),
      preferred_column=c("A", "B")
    ),
    regexp="`preferred_column` must be a scalar character string",
    fixed=TRUE
  )

  expect_error(
    replace_synonym.data.frame(
      x=data.frame(A="A", stringsAsFactors=FALSE),
      synonyms=data.frame(B="A", Column="A", Verbatim="A", Preferred="A", stringsAsFactors=FALSE)
    ),
    regexp="All columns in `synonyms` other than the `replacement_column`, `verbatim_column`, and `preferred_column` must be names of `x`.",
    fixed=TRUE
  )
  
})
