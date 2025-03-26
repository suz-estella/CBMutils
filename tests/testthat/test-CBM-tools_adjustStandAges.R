
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("adjustStandAges", {

  # Test: step ages forward in time without disturbances
  agesAdjust <- adjustStandAges(
    yearInput = 2000, yearOutput = 2010,
    standAges = data.frame(pixelIndex = 1:5, age = c(1:4, NA))
  )
  expect_equal(as.data.frame(agesAdjust), data.frame(pixelIndex = 1:5, age = c(11:14, NA)))

  # Test: step ages backwards in time without disturbances
  agesAdjust <- adjustStandAges(
    yearInput = 2000, yearOutput = 1990,
    standAges = data.frame(pixelIndex = 1:5, age = c(11:14, NA))
  )
  expect_equal(as.data.frame(agesAdjust), data.frame(pixelIndex = 1:5, age = c(1:4, NA)))

  ## Test stepping it back further than the stand ages
  expect_error(
    adjustStandAges(
      yearInput = 2000, yearOutput = 1985,
      standAges = data.frame(pixelIndex = 1:5, age = c(11:14, NA)),
    ))
  agesAdjust <- adjustStandAges(
    yearInput = 2000, yearOutput = 1987,
    standAges = data.frame(pixelIndex = 1:5, age = c(11:14, NA)),
    defaultAge = 100
  )
  expect_equal(as.data.frame(agesAdjust), data.frame(pixelIndex = 1:5, age = c(100, 100, 0, 1, NA)))


  # Test: step ages forward in time with disturbances
  disturbanceEvents <- as.data.frame(rbind(
    c(id = 1, year = 1985),
    c(id = 1, year = 1995),
    c(id = 1, year = 2005),
    c(id = 1, year = 2009)
  ))

  agesAdjust <- adjustStandAges(
    yearInput = 2000, yearOutput = 2010,
    standAges = data.frame(id = 1:5, age = c(1:4, NA)),
    disturbanceEvents = disturbanceEvents
  )
  expect_equal(as.data.frame(agesAdjust), data.frame(
    id = 1:5, age = c(1, 12:14, NA)
  ))

  ## Check adding a regeneration delay
  agesAdjust <- adjustStandAges(
    yearInput = 2000, yearOutput = 2010,
    standAges = data.frame(id = 1:5, age = c(1:4, NA)),
    disturbanceEvents = disturbanceEvents,
    delay = 2
  )
  expect_equal(as.data.frame(agesAdjust), data.frame(
    id = 1:5, age = c(0, 12:14, NA)
  ))
  expect_equal(
    agesAdjust,
    adjustStandAges(
      yearInput = 2000, yearOutput = 2010,
      standAges = data.frame(id = 1:5, age = c(1:4, NA), delay = 2),
      disturbanceEvents = disturbanceEvents
    )
  )

  # Test: step ages backwards in time with disturbances
  agesAdjust <- adjustStandAges(
    yearInput = 2000, yearOutput = 1990,
    standAges = data.frame(id = 1:5, age = c(5, 12:14, NA)),
    disturbanceEvents = disturbanceEvents
  )
  expect_equal(as.data.frame(agesAdjust), data.frame(
    id = 1:5,
    age = c(5, 2:4, NA)
  ))

  ## Check adding a regeneration delay
  agesAdjust <- adjustStandAges(
    yearInput = 2000, yearOutput = 1990,
    standAges = data.frame(id = 1:5, age = c(5, 12:14, NA)),
    disturbanceEvents = disturbanceEvents,
    delay = 2
  )
  expect_equal(as.data.frame(agesAdjust), data.frame(
    id = 1:5, age = c(3, 2:4, NA)
  ))
  expect_equal(
    agesAdjust,
    adjustStandAges(
      yearInput = 2000, yearOutput = 1990,
      standAges = data.frame(id = 1:5, age = c(5, 12:14, NA), delay = 2),
      disturbanceEvents = disturbanceEvents
    )
  )


  # Test: special case: disturbance events are provided that don't match the stand ages
  agesAdjust <- expect_warning(
    adjustStandAges(
      yearInput = 2000, yearOutput = 1990,
      standAges = data.frame(id = 1, age = 10),
      disturbanceEvents = disturbanceEvents
    ),
    "unexpectedly high age"
  )
  expect_equal(as.data.frame(agesAdjust), data.frame(id = 1, age = 5))

})

