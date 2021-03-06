context("synonym")

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
  expect_equal(
    replace_synonym(c("A", ""), c(a="a", "B")),
    c("a", "B"),
    info="Blank values can be replaced"
  )
})

test_that("replace_synonym.data.frame works", {
  expect_equal(
    replace_synonym(
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
    dplyr::tibble(
      A=rep(c("A", "B"), each=2),
      B=c("apple", "b", "c", "d")
    )
  )
  expect_equal(
    replace_synonym(
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
    dplyr::tibble(
      A=rep(c("A", "B"), each=2),
      B=c("apple", "b", "c", "durian")
    )
  )
  expect_equal(
    replace_synonym(
      x=
        data.frame(
          A=rep(c("A", "B"), each=2),
          B=letters[1:4],
          stringsAsFactors=FALSE
        ),
      synonyms=
        list(
          data.frame(
            A=rep(c("A", "B"), each=2),
            Column="B",
            Verbatim=c("a", "c", "b", "d"),
            Preferred=c("apple", "cherry", "berry", "durian"),
            stringsAsFactors=FALSE
          ),
          data.frame(
            A="B",
            Column="B",
            Verbatim="c",
            Preferred="clementine",
            stringsAsFactors=FALSE
          )
        )
    ),
    dplyr::tibble(
      A=rep(c("A", "B"), each=2),
      B=c("apple", "b", "clementine", "durian")
    ),
    info="list synonyms work"
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
    replace_synonym.character("A", c(a="A", a="B")),
    regexp="All names of `synonyms` (verbatim values) must be unique.",
    fixed=TRUE
  )
})

test_that("replace_synonym.data.frame errors appropriately", {
  expect_error(
    replace_synonym(x=data.frame(A="A"), 1),
    regexp="`synonyms` must be a data.frame",
    fixed=TRUE
  )
  expect_error(
    replace_synonym(
      x=data.frame(A="A"),
      synonyms=data.frame(Column=1, Verbatim=1, Preferred=1, stringsAsFactors=FALSE)
    ),
    regexp="`synonyms[[replacement_column]]` must be a character column.",
    fixed=TRUE
  )
  expect_error(
    replace_synonym(
      x=data.frame(A="A"),
      synonyms=data.frame(Column="A", Verbatim=1, Preferred=1, stringsAsFactors=FALSE)
    ),
    regexp="`synonyms[[verbatim_column]]` must be a character column.",
    fixed=TRUE
  )
  expect_error(
    replace_synonym(
      x=data.frame(A="A"),
      synonyms=data.frame(Column="A", Verbatim="A", Preferred=1, stringsAsFactors=FALSE)
    ),
    regexp="`synonyms[[preferred_column]]` must be a character column.",
    fixed=TRUE
  )
  expect_error(
    replace_synonym(
      x=data.frame(A="A", stringsAsFactors=TRUE),
      synonyms=data.frame(Column="A", Verbatim="A", Preferred="A", stringsAsFactors=FALSE)
    ),
    regexp="`x` must be a character vector",
    fixed=TRUE
  )
  expect_error(
    replace_synonym(
      x=data.frame(A="A", stringsAsFactors=FALSE),
      synonyms=data.frame(Column="A", Verbatim="A", Preferred="A", stringsAsFactors=FALSE),
      replacement_column=c("A", "B")
    ),
    regexp="`replacement_column` must be a scalar character string",
    fixed=TRUE
  )
  expect_error(
    replace_synonym(
      x=data.frame(A="A", stringsAsFactors=FALSE),
      synonyms=data.frame(Column="A", Verbatim="A", Preferred="A", stringsAsFactors=FALSE),
      verbatim_column=c("A", "B")
    ),
    regexp="`verbatim_column` must be a scalar character string",
    fixed=TRUE
  )
  expect_error(
    replace_synonym(
      x=data.frame(A="A", stringsAsFactors=FALSE),
      synonyms=data.frame(Column="A", Verbatim="A", Preferred="A", stringsAsFactors=FALSE),
      preferred_column=c("A", "B")
    ),
    regexp="`preferred_column` must be a scalar character string",
    fixed=TRUE
  )

  d1 <- data.frame(A="A", stringsAsFactors=FALSE)
  expect_equal(
    expect_warning(
      replace_synonym(
        x=d1,
        synonyms=data.frame(B="A", Column="A", Verbatim="A", Preferred="A", stringsAsFactors=FALSE)
      ),
      regexp="All columns in `synonyms` other than the `replacement_column`, `verbatim_column`, and `preferred_column` must be names of `x`.*No synonyms will be applied due to the following missing columns: `B`"
    ),
    d1
  )
})

context("correct_case")

test_that("correct_case standardization", {
  expect_equal(
    correct_case(c("ABC", "Abc", "aBc", "def"), "Abc"),
    c("Abc", "Abc", "Abc", "def")
  )
  expect_equal(
    correct_case(factor(c("ABC", "Abc", "aBc", "def")), "Abc"),
    factor(c("Abc", "Abc", "Abc", "def"))
  )
  expect_equal(
    correct_case(ordered(c("ABC", "Abc", "aBc", "def")), "Abc"),
    ordered(c("Abc", "Abc", "Abc", "def"))
  )
  expect_equal(
    correct_case(
      ordered(
        c("ABC", "Abc", "aBc", "def"),
        levels=c("def", "ABC", "Abc", "aBc")
      ), "Abc"
    ),
    ordered(c("Abc", "Abc", "Abc", "def"), levels=c("def", "Abc"))
  )
  expect_equal(
    correct_case(
      ordered(
        c("ABC", "Abc", "aBc", "def"),
        levels=c("ABC", "def", "Abc", "aBc")
      ), "Abc"
    ),
    ordered(c("Abc", "Abc", "Abc", "def"), levels=c("Abc", "def"))
  )
})

test_that("correct_case errors", {
  expect_error(
    correct_case("Abc", 1),
    regexp="`preferred` must be a character vector.",
    fixed=TRUE
  )
  expect_error(
    correct_case("Abc", c("A", "a")),
    regexp="All `preferred` values must be unique, case-insensitively.",
    fixed=TRUE
  )
})

test_that("replace_synonym_list", {
  d1 <- data.frame(A=c("A", "B", "C", "a"), stringsAsFactors=FALSE)
  s1 <- data.frame(Column="A", Verbatim="A", Preferred="apple", stringsAsFactors=FALSE)
  s2 <- data.frame(Column="A", Verbatim="B", Preferred="bear", stringsAsFactors=FALSE)
  expect_equal(
    expect_message(
      replace_synonym_list(
        d1,
        list("Synonyms A"=s1, "Synonyms B"=s2)
      ),
      regexp="Using the following names as synonyms for possible replacement: `Synonyms A`, `Synonyms B`",
      fixed=TRUE
    ),
    data.frame(A=c("apple", "bear", "C", "apple"), stringsAsFactors=FALSE)
  )
  expect_equal(
    expect_message(
      replace_synonym_list(
        d1,
        list("foo A"=s1, "foo B"=s2)
      ),
      regexp="No synonym data.frames found in the list for possible replacement.",
      fixed=TRUE
    ),
    d1
  )
})

test_that("synonym errors", {
  expect_error(
    replace_synonym_list(x=1, synonyms=list("A")),
    regexp="All `synonyms` must be named",
    fixed=TRUE
  )
  expect_error(
    replace_synonym_list(x=1, synonyms=list(A="A", "B")),
    regexp="All `synonyms` must be named",
    fixed=TRUE
  )
  expect_error(
    replace_synonym_list(x=1, synonyms=data.frame(A=1)),
    regexp="`synonyms` must be a list and not a data.frame (see `replace_synonym()` for using a data.frame).",
    fixed=TRUE
  )
  expect_error(
    replace_synonym_list(x=1, synonyms=c(A=1)),
    regexp="`synonyms` must be a list and not a data.frame (see `replace_synonym()` for using a data.frame).",
    fixed=TRUE
  )
})

test_that("replace_synonym warnings", {
  expect_equal(
    expect_warning(
      replace_synonym(
        x=
          data.frame(
            A=rep(c("A", "B"), each=2),
            Column=letters[1:4],
            stringsAsFactors=FALSE
          ),
        synonyms=
          data.frame(
            A="A",
            Column="Column",
            Verbatim=c("a", "c"),
            Preferred=c("apple", "cherry"),
            stringsAsFactors=FALSE
          )
      ),
      regexp="`replacement_column` (Column) is in `names(x)`.  Renaming it in `synonyms`.",
      fixed=TRUE
    ),
    dplyr::tibble(
      A=rep(c("A", "B"), each=2),
      Column=c("apple", "b", "c", "d")
    )
  )
})
