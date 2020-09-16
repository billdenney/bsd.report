context("impute_dtc")

test_that("impute_dtc_separate", {
  # Decreasing amounts of precision
  expect_equal(
    impute_dtc_separate(data=data.frame(ADTC="2020-02-01T08:09:10", NTSFD=0)),
    data.frame(
      ADTC="2020-02-01T08:09:10",
      DATE_PART="2020-02-01",
      TIME_PART="08:09:10",
      NTSFD=0,
      ADTC_IMPUTE_METHOD="Observed date and time"
    )
  )
  expect_equal(
    impute_dtc_separate(data=data.frame(ADTC="2020-02-01T08:09:UN", NTSFD=0)),
    data.frame(
      ADTC="2020-02-01T08:09:UN",
      DATE_PART="2020-02-01",
      TIME_PART=NA_character_,
      NTSFD=0,
      ADTC_IMPUTE_METHOD=NA_character_
    )
  )
  expect_equal(
    impute_dtc_separate(data=data.frame(ADTC="2020-02-01T08:UN:UN", NTSFD=0)),
    data.frame(
      ADTC="2020-02-01T08:UN:UN",
      DATE_PART="2020-02-01",
      TIME_PART=NA_character_,
      NTSFD=0,
      ADTC_IMPUTE_METHOD=NA_character_
    )
  )
  expect_equal(
    impute_dtc_separate(data=data.frame(ADTC="2020-02-01TUN:UN:UN", NTSFD=0)),
    data.frame(
      ADTC="2020-02-01TUN:UN:UN",
      DATE_PART="2020-02-01",
      TIME_PART=NA_character_,
      NTSFD=0,
      ADTC_IMPUTE_METHOD=NA_character_
    )
  )
  expect_equal(
    impute_dtc_separate(data=data.frame(ADTC="2020-02-01TUN:UN:UN", NTSFD=NA_real_)),
    data.frame(
      ADTC="2020-02-01TUN:UN:UN",
      DATE_PART="2020-02-01",
      TIME_PART=NA_character_,
      NTSFD=NA_real_,
      ADTC_IMPUTE_METHOD="Unscheduled measurement (missing NTSFD)"
    ),
    info="Any unscheduled measurement will not have time imputed"
  )
  expect_equal(
    impute_dtc_separate(data=data.frame(ADTC="2020-02-01", NTSFD=NA_real_)),
    data.frame(
      ADTC="2020-02-01",
      DATE_PART="2020-02-01",
      TIME_PART=NA_character_,
      NTSFD=NA_real_,
      ADTC_IMPUTE_METHOD="Unscheduled measurement (missing NTSFD)"
    ),
    info="Missing time part yields NA"
  )
})

test_that("impute_dtc", {
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC="2020-02-01T08:09:10")),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC="2020-02-01T08:09:10",
      ADTC_IMPUTE_METHOD="Observed date and time", ADTC_IMPUTED="2020-02-01T08:09:10"
    )
  )
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC="2020-02-01TUN:UN:UN")),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC="2020-02-01TUN:UN:UN",
      ADTC_IMPUTE_METHOD=NA_character_, ADTC_IMPUTED=NA_character_
    )
  )
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=NA_real_, ADTC="2020-02-01TUN:UN:UN")),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=NA_real_, ADTC="2020-02-01TUN:UN:UN",
      ADTC_IMPUTE_METHOD="Unscheduled measurement (missing NTSFD)", ADTC_IMPUTED=NA_character_
    )
  )
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-01TUN:UN:UN"))),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-01TUN:UN:UN"),
      ADTC_IMPUTE_METHOD=c("Observed date and time", "Single time measurment observed for a nominal time"),
      ADTC_IMPUTED="2020-02-01T08:09:10"
    )
  )
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-03TUN:UN:UN"))),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-03TUN:UN:UN"),
      ADTC_IMPUTE_METHOD=c("Observed date and time", "Multiple dates observed during the same nominal time, not imputing"),
      ADTC_IMPUTED=c("2020-02-01T08:09:10", NA_character_)
    )
  )
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-03T09:10:11"))),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-03T09:10:11"),
      ADTC_IMPUTE_METHOD="Observed date and time",
      ADTC_IMPUTED=c("2020-02-01T08:09:10", "2020-02-03T09:10:11")
    ),
    info="Imputation does not occur if not indicated"
  )
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-03T09:10:11", NA_character_))),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-03T09:10:11", NA_character_),
      ADTC_IMPUTE_METHOD=c(rep("Observed date and time", 2), "Multiple dates observed during the same nominal time, not imputing"),
      ADTC_IMPUTED=c("2020-02-01T08:09:10", "2020-02-03T09:10:11", NA_character_)
    )
  )
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-01T09:10:11", NA_character_))),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-01T09:10:11", NA_character_),
      ADTC_IMPUTE_METHOD=c(rep("Observed date and time", 2), "Median time within the observed nominal times"),
      ADTC_IMPUTED=c("2020-02-01T08:09:10", "2020-02-01T09:10:11", "2020-02-01T08:09:10")
    )
  )
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC=rep(NA_character_, 2))),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC=rep(NA_character_, 2),
      ADTC_IMPUTE_METHOD="No dates observed during the nominal time, not imputing",
      ADTC_IMPUTED=NA_character_
    )
  )
  
  expect_error(
    impute_dtc(data.frame(DATE_PART=1)),
    regexp="`data` cannot have columns named 'DATE_PART', 'TIME_PART', or 'current_impute' as those are used internally.",
    fixed=TRUE
  )
  expect_error(
    impute_dtc(data.frame(TIME_PART=1)),
    regexp="`data` cannot have columns named 'DATE_PART', 'TIME_PART', or 'current_impute' as those are used internally.",
    fixed=TRUE
  )
  expect_error(
    impute_dtc(data.frame(current_impute=1)),
    regexp="`data` cannot have columns named 'DATE_PART', 'TIME_PART', or 'current_impute' as those are used internally.",
    fixed=TRUE
  )
})

test_that("impute_dtc_ntod", {
  expect_equal(
    impute_dtc_ntod(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC=NA_character_)),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC=NA_character_,
      ADTC_IMPUTE_METHOD=NA_character_, ADTC_IMPUTED=NA_character_
    ),
    info="Minimal columns are added, even if nothing else is done"
  )
  expect_equal(
    impute_dtc_ntod(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC="2020-02-21")),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC="2020-02-21",
      ADTC_IMPUTE_METHOD=NA_character_, ADTC_IMPUTED=NA_character_
    ),
    info="ADTC is left alone"
  )
  expect_equal(
    impute_dtc_ntod(data.frame(STUDYID=1, USUBJID=1, NTSFD=0:1, ADTC=c("2020-02-21T08:09", "2020-02-21"))),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0:1, ADTC=c("2020-02-21T08:09", "2020-02-21"),
      ADTC_IMPUTE_METHOD=c("Observed date and time", NA_character_),
      ADTC_IMPUTED=c("2020-02-21T08:09", NA_character_)
    ),
    info="No imputation for different NTOD"
  )
  expect_equal(
    impute_dtc_ntod(data.frame(STUDYID=1, USUBJID=1, NTSFD=c(0, 24), ADTC=c("2020-02-21T08:09", "2020-02-22"))),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=c(0, 24), ADTC=c("2020-02-21T08:09", "2020-02-22"),
      ADTC_IMPUTE_METHOD=c("Observed date and time", "Median time within the nominal time of day for the subject"),
      ADTC_IMPUTED=c("2020-02-21T08:09", "2020-02-22T08:09")
    ),
    info="Imputation within NTOD"
  )
})
