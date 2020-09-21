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
      ADTC_IMPUTED=NA_character_,
      ADTC_IMPUTE_METHOD="Observed date and time"
    )
  )
  expect_equal(
    impute_dtc_separate(data=data.frame(ADTC="2020-02-01T08:09:UN", NTSFD=0)),
    data.frame(
      ADTC="2020-02-01T08:09:UN",
      DATE_PART="2020-02-01",
      TIME_PART="08:09",
      NTSFD=0,
      ADTC_IMPUTED=NA_character_,
      ADTC_IMPUTE_METHOD="Observed date and time"
    )
  )
  expect_equal(
    impute_dtc_separate(data=data.frame(ADTC="2020-02-01T08:UN:UN", NTSFD=0)),
    data.frame(
      ADTC="2020-02-01T08:UN:UN",
      DATE_PART="2020-02-01",
      TIME_PART=NA_character_,
      NTSFD=0,
      ADTC_IMPUTED=NA_character_,
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
      ADTC_IMPUTED=NA_character_,
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
      ADTC_IMPUTED=NA_character_,
      ADTC_IMPUTE_METHOD=NA_character_
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
      ADTC_IMPUTED=NA_character_,
      ADTC_IMPUTE_METHOD=NA_character_
    ),
    info="Missing time part yields NA"
  )
})

test_that("impute_dtc", {
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC="2020-02-01T08:09:10")),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC="2020-02-01T08:09:10",
      ADTC_IMPUTED="2020-02-01T08:09:10",
      ADTC_IMPUTE_METHOD="Observed date and time"
    )
  )
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC="2020-02-01TUN:UN:UN")),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC="2020-02-01TUN:UN:UN",
      ADTC_IMPUTED=NA_character_,
      ADTC_IMPUTE_METHOD=NA_character_
    )
  )
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=NA_real_, ADTC="2020-02-01TUN:UN:UN")),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=NA_real_, ADTC="2020-02-01TUN:UN:UN",
      ADTC_IMPUTED=NA_character_,
      ADTC_IMPUTE_METHOD=NA_character_
    )
  )
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-01TUN:UN:UN"))),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-01TUN:UN:UN"),
      ADTC_IMPUTED="2020-02-01T08:09:10",
      ADTC_IMPUTE_METHOD=c("Observed date and time", "Single time measurment observed for a nominal time")
    )
  )
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-03TUN:UN:UN"))),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-03TUN:UN:UN"),
      ADTC_IMPUTED=c("2020-02-01T08:09:10", NA_character_),
      ADTC_IMPUTE_METHOD=c("Observed date and time", NA_character_)
    )
  )
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-03T09:10:11"))),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-03T09:10:11"),
      ADTC_IMPUTED=c("2020-02-01T08:09:10", "2020-02-03T09:10:11"),
      ADTC_IMPUTE_METHOD="Observed date and time"
    ),
    info="Imputation does not occur if not indicated"
  )
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-03T09:10:11", NA_character_))),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-03T09:10:11", NA_character_),
      ADTC_IMPUTED=c("2020-02-01T08:09:10", "2020-02-03T09:10:11", NA_character_),
      ADTC_IMPUTE_METHOD=c(rep("Observed date and time", 2), NA_character_)
    )
  )
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-01T09:10:11", NA_character_))),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-01T09:10:11", NA_character_),
      ADTC_IMPUTED=c("2020-02-01T08:09:10", "2020-02-01T09:10:11", "2020-02-01T08:09:10"),
      ADTC_IMPUTE_METHOD=
        c(rep("Observed date and time", 2),
          "Single date for the nominal time; Median time within the observed nominal times"
        )
    )
  )
  expect_equal(
    impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC=rep(NA_character_, 2))),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC=rep(NA_character_, 2),
      ADTC_IMPUTED=NA_character_,
      ADTC_IMPUTE_METHOD=NA_character_
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
      ADTC_IMPUTED=NA_character_,
      ADTC_IMPUTE_METHOD=NA_character_
    ),
    info="Minimal columns are added, even if nothing else is done"
  )
  expect_equal(
    impute_dtc_ntod(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC="2020-02-21")),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC="2020-02-21",
      ADTC_IMPUTED=NA_character_,
      ADTC_IMPUTE_METHOD=NA_character_
    ),
    info="ADTC is left alone"
  )
  expect_equal(
    impute_dtc_ntod(data.frame(STUDYID=1, USUBJID=1, NTSFD=0:1, ADTC=c("2020-02-21T08:09", "2020-02-21"))),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0:1, ADTC=c("2020-02-21T08:09", "2020-02-21"),
      ADTC_IMPUTED=c("2020-02-21T08:09", NA_character_),
      ADTC_IMPUTE_METHOD=c("Observed date and time", NA_character_)
    ),
    info="No imputation for different NTOD"
  )
  expect_equal(
    impute_dtc_ntod(data.frame(STUDYID=1, USUBJID=1, NTSFD=c(0, 24), ADTC=c("2020-02-21T08:09", "2020-02-22"))),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=c(0, 24), ADTC=c("2020-02-21T08:09", "2020-02-22"),
      ADTC_IMPUTED=c("2020-02-21T08:09", "2020-02-22T08:09"),
      ADTC_IMPUTE_METHOD=c("Observed date and time", "Median time within the nominal time of day for the subject")
    ),
    info="Imputation within NTOD"
  )
  expect_equal(
    impute_dtc_ntod(
      impute_dtc(data.frame(STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-03TUN:UN:UN")))
    ),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=0, ADTC=c("2020-02-01T08:09:10", "2020-02-03TUN:UN:UN"),
      ADTC_IMPUTED=c("2020-02-01T08:09:10", "2020-02-03T08:09:10"),
      ADTC_IMPUTE_METHOD=c("Observed date and time", "Median time within the nominal time of day for the subject")
    ),
    info="Multiple dates are overridden"
  )
  expect_equal(
    impute_dtc_ntod(
      data.frame(
        STUDYID=1, USUBJID=1,
        NTSFD=c(0, 24, NA_real_),
        ADTC=c("2020-02-21T08:09", "2020-02-22", "2020-02-22")
      )
    ),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=c(0, 24, NA_real_),
      ADTC=c("2020-02-21T08:09", "2020-02-22", "2020-02-22"),
      ADTC_IMPUTED=c("2020-02-21T08:09", "2020-02-22T08:09", NA_character_),
      ADTC_IMPUTE_METHOD=c("Observed date and time", "Median time within the nominal time of day for the subject", NA_character_)
    ),
    info="Unscheduled measurements are not imputed"
  )
  expect_equal(
    impute_dtc_ntod(
      data.frame(
        STUDYID=1, USUBJID=1,
        NTSFD=c(0, 24, NA_real_),
        ADTC=c("2020-02-21T08:09", "2020-02-22", "2020-02-22")
      ),
      na_ntod=0
    ),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=c(0, 24, NA_real_),
      ADTC=c("2020-02-21T08:09", "2020-02-22", "2020-02-22"),
      ADTC_IMPUTED=c("2020-02-21T08:09", "2020-02-22T08:09", "2020-02-22T08:09"),
      ADTC_IMPUTE_METHOD=
        c(
          "Observed date and time",
          "Median time within the nominal time of day for the subject",
          "Assumed nominal time of day for unscheduled measure; Median time within the nominal time of day for the subject"
        )
    ),
    info="Unscheduled measurements with and assumed time of day are imputed, if there are other measurements at the same time of day."
  )
  expect_equal(
    impute_dtc_ntod(
      data.frame(
        STUDYID=1, USUBJID=1,
        NTSFD=c(0, 24, NA_real_),
        ADTC=c("2020-02-21T08:09", "2020-02-22", "2020-02-22")
      ),
      na_ntod=1
    ),
    tibble::tibble(
      STUDYID=1, USUBJID=1, NTSFD=c(0, 24, NA_real_),
      ADTC=c("2020-02-21T08:09", "2020-02-22", "2020-02-22"),
      ADTC_IMPUTED=c("2020-02-21T08:09", "2020-02-22T08:09", NA_character_),
      ADTC_IMPUTE_METHOD=
        c(
          "Observed date and time",
          "Median time within the nominal time of day for the subject",
          "Assumed nominal time of day for unscheduled measure"
        )
    ),
    info="Unscheduled measurements with and assumed time of day are not imputed, if there are not other measurements at the same time of day."
  )
})

test_that("impute_time_act_nom", {
  expect_equal(
    impute_time_act_nom(actual=1:3, nominal=rep(NA_real_, 3)),
    data.frame(imputed=1:3, method="Observed actual")
  )
  expect_equal(
    expect_warning(
      impute_time_act_nom(actual=3:1, nominal=1:3),
      regexp="Some 'actual' times are not in 'nominal' time order"
    ),
    data.frame(imputed=3:1, method="Observed actual")
  )
  expect_equal(
    impute_time_act_nom(actual=1:3, nominal=1:3),
    data.frame(imputed=1:3, method="Observed actual")
  )
  expect_equal(
    impute_time_act_nom(actual=c(1, NA, 3), nominal=1:3),
    data.frame(
      imputed=1:3,
      method=c("Observed actual", "<24 hr between two observed points", "Observed actual")
    )
  )
  expect_equal(
    impute_time_act_nom(actual=c(1, NA, 4), nominal=1:3),
    data.frame(
      imputed=c(1, 2.5, 4),
      method=c("Observed actual", "<24 hr between two observed points", "Observed actual")
    )
  )
  expect_equal(
    impute_time_act_nom(actual=c(1, NA, NA, 4), nominal=1:4),
    data.frame(
      imputed=c(1, 2, 3, 4),
      method=c("Observed actual", rep("<24 hr between two observed points", 2), "Observed actual")
    )
  )
  expect_equal(
    impute_time_act_nom(actual=c(1, NA, NA, 5), nominal=1:4),
    data.frame(
      imputed=c(1, 7/3, 11/3, 5),
      method=c("Observed actual", rep("<24 hr between two observed points", 2), "Observed actual")
    )
  )
  expect_equal(
    impute_time_act_nom(actual=c(1, NA), nominal=1:2),
    data.frame(
      imputed=c(1, 2),
      method=c("Observed actual", "Extrapolate forward <24h")
    )
  )
  expect_equal(
    impute_time_act_nom(actual=c(NA, 2), nominal=1:2),
    data.frame(
      imputed=c(1, 2),
      method=c("Extrapolate backward <24h", "Observed actual")
    )
  )
  expect_equal(
    impute_time_act_nom(actual=c(1, NA, 50), nominal=c(1:2, 48)),
    data.frame(
      imputed=c(1, 2, 50),
      method=c("Observed actual", "Extrapolate forward <24h", "Observed actual")
    )
  )
  expect_equal(
    impute_time_act_nom(actual=c(2, NA, 48), nominal=c(1, 47:48)),
    data.frame(
      imputed=c(2, 47, 48),
      method=c("Observed actual", "Extrapolate backward <24h", "Observed actual")
    )
  )

  expect_equal(
    impute_time_act_nom(actual=c(2, NA, 48), nominal=c(1, 47, 96)),
    data.frame(
      imputed=c(2, NA, 48),
      method=c("Observed actual", NA, "Observed actual")
    ),
    info="Imputation only occurs within 24 nominal hours of an actual time"
  )
})
