
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

# Set output path
destinationPath <- file.path(testDirs$temp$outputs, "dataPrep_disturbanceRasters")
dir.create(destinationPath)

test_that("dataPrep_disturbanceRastersURL", {

  # Test: archive file with 1 raster file per year
  disturbanceRasters <- dataPrep_disturbanceRastersURL(
    destinationPath       = destinationPath,
    disturbanceRastersURL = "https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt",
  )

  expect_true(is.list(disturbanceRasters))
  expect_true(all(sapply(disturbanceRasters, class) == "SpatRaster"))
  expect_identical(names(disturbanceRasters), as.character(1985:2011))

  # Test: single file with 1 raster band per year
  disturbanceRasters <- dataPrep_disturbanceRastersURL(
    destinationPath       = destinationPath,
    disturbanceRastersURL = "https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt",
    targetFile            = "disturbance_testArea/SaskDist_1985.grd",
    #alsoExtract           = "similar",
    bandYears             = 1985
  )

  expect_s4_class(disturbanceRasters, "SpatRaster")
  expect_identical(names(disturbanceRasters), as.character(1985))
  expect_match(basename(terra::sources(disturbanceRasters)), as.character(1985))

})

test_that("dataPrep_disturbanceRasters", {

  distRasters <- list(
    bands = dataPrep_disturbanceRastersURL(
      destinationPath       = destinationPath,
      disturbanceRastersURL = "https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt",
      targetFile            = "disturbance_testArea/SaskDist_1985.grd",
      #alsoExtract           = "similar",
      bandYears             = 1985
    ),
    files = dataPrep_disturbanceRastersURL(
      destinationPath       = destinationPath,
      disturbanceRastersURL = "https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt",
    )
  )

  # Expect error: wrong input format
  expect_error(dataPrep_disturbanceRasters(distRasters[["bands"]]), "list")
  expect_error(dataPrep_disturbanceRasters(distRasters[["files"]]), "year")

  # Test: single file with 1 raster band per year
  disturbanceEvents <- dataPrep_disturbanceRasters(unname(distRasters["bands"]))

  expect_true(inherits(disturbanceEvents, "data.table"))
  for (colName in c("pixelIndex", "year", "eventID")){
    expect_true(colName %in% names(disturbanceEvents))
    expect_true(is.integer(disturbanceEvents[[colName]]))
    expect_true(all(!is.na(disturbanceEvents[[colName]])))
  }
  expect_equal(nrow(disturbanceEvents), 1857)
  expect_setequal(unique(disturbanceEvents$eventID), c(1L, 2L, 3L, 5L))
  expect_equal(sum(disturbanceEvents$pixelIndex), 4789432666)
  expect_true(all(disturbanceEvents$year == 1985))

  # Test: archive file with 1 raster file per year
  disturbanceEvents <- dataPrep_disturbanceRasters(unname(distRasters["files"]))

  expect_true(inherits(disturbanceEvents, "data.table"))
  for (colName in c("pixelIndex", "year", "eventID")){
    expect_true(colName %in% names(disturbanceEvents))
    expect_true(is.integer(disturbanceEvents[[colName]]))
    expect_true(all(!is.na(disturbanceEvents[[colName]])))
  }
  expect_equal(nrow(disturbanceEvents), 295569)
  expect_true(all(disturbanceEvents$eventID %in% c(1L:5L)))

  distEventsByYear <- disturbanceEvents[, .(count = .N), by = year]
  expect_equal(setNames(distEventsByYear$count, distEventsByYear$year), c(
    `1985` =  1857, `1986` =  2364, `1987` =  7905, `1988` =   519, `1989` =   939,
    `1990` =  2672, `1991` =  1280, `1992` =   906, `1993` = 23477, `1994` = 13725,
    `1995` =  8672, `1996` =  2775, `1997` =  3269, `1998` = 42812, `1999` = 49568,
    `2000` = 14790, `2001` =  8386, `2002` = 19817, `2003` = 12713, `2004` =  3920,
    `2005` = 12289, `2006` = 30593, `2007` =  9060, `2008` =  3815, `2009` = 14146,
    `2010` =  1329, `2011` =  1971
  ))

  # Test: one raster or list of raster files for each disturbance type
  disturbanceEvents <- dataPrep_disturbanceRasters(setNames(distRasters, c(2, 1)))

  expect_true(inherits(disturbanceEvents, "data.table"))
  for (colName in c("pixelIndex", "year", "eventID")){
    expect_true(colName %in% names(disturbanceEvents))
    expect_true(is.integer(disturbanceEvents[[colName]]))
    expect_true(all(!is.na(disturbanceEvents[[colName]])))
  }
  expect_equal(nrow(disturbanceEvents), 295569 + 1857)
  expect_true(all(disturbanceEvents$eventID %in% c(1L:2L)))

  distEventsByYear <- subset(disturbanceEvents, eventID == 1L)[, .(count = .N), by = year]
  expect_equal(setNames(distEventsByYear$count, distEventsByYear$year), c(
    `1985` =  1857, `1986` =  2364, `1987` =  7905, `1988` =   519, `1989` =   939,
    `1990` =  2672, `1991` =  1280, `1992` =   906, `1993` = 23477, `1994` = 13725,
    `1995` =  8672, `1996` =  2775, `1997` =  3269, `1998` = 42812, `1999` = 49568,
    `2000` = 14790, `2001` =  8386, `2002` = 19817, `2003` = 12713, `2004` =  3920,
    `2005` = 12289, `2006` = 30593, `2007` =  9060, `2008` =  3815, `2009` = 14146,
    `2010` =  1329, `2011` =  1971
  ))

  distEventsByYear <- subset(disturbanceEvents, eventID == 2L)[, .(count = .N), by = year]
  expect_equal(setNames(distEventsByYear$count, distEventsByYear$year), c(`1985` = 1857))

  # Test: subset by year
  disturbanceEvents <- dataPrep_disturbanceRasters(setNames(distRasters, c(2, 1)), year = 1985)

  expect_true(inherits(disturbanceEvents, "data.table"))
  for (colName in c("pixelIndex", "year", "eventID")){
    expect_true(colName %in% names(disturbanceEvents))
    expect_true(is.integer(disturbanceEvents[[colName]]))
    expect_true(all(!is.na(disturbanceEvents[[colName]])))
  }
  expect_equal(nrow(disturbanceEvents), 1857 + 1857)
  expect_true(all(disturbanceEvents$eventID %in% c(1L:2L)))

  distEventsByYear <- subset(disturbanceEvents, eventID == 1L)[, .(count = .N), by = year]
  expect_equal(setNames(distEventsByYear$count, distEventsByYear$year), c(`1985` = 1857))

  distEventsByYear <- subset(disturbanceEvents, eventID == 2L)[, .(count = .N), by = year]
  expect_equal(setNames(distEventsByYear$count, distEventsByYear$year), c(`1985` = 1857))

  # Test: with template: archive file with 1 raster file per year

  ## upsample
  disturbanceEvents <- dataPrep_disturbanceRasters(
    disturbanceRasters = list(distRasters[["files"]][1]),
    templateRast       = terra::rast(
      res  = 10, vals = NA,
      xmin = -677500, xmax = -677500 + 10000,
      ymin =  704500, ymax =  704500 + 10000,
      crs  = terra::crs(distRasters[["files"]][[1]])
    )
  )

  expect_true(inherits(disturbanceEvents, "data.table"))
  for (colName in c("pixelIndex", "year", "eventID")){
    expect_true(colName %in% names(disturbanceEvents))
    expect_true(is.integer(disturbanceEvents[[colName]]))
    expect_true(all(!is.na(disturbanceEvents[[colName]])))
  }

  distEventsByEvent <- disturbanceEvents[, .(count = .N), by = eventID]
  data.table::setkey(distEventsByEvent, eventID)
  expect_equal(setNames(distEventsByEvent$count, distEventsByEvent$eventID), c(
    `2` = 1296,
    `3` =  684,
    `5` = 9441
  ))

  ## downsample
  disturbanceEvents <- dataPrep_disturbanceRasters(
    disturbanceRasters = list(distRasters[["files"]][1]),
    templateRast       = terra::rast(
      res  = 100, vals = NA,
      xmin = -677500, xmax = -677500 + 10000,
      ymin =  704500, ymax =  704500 + 10000,
      crs  = terra::crs(distRasters[["files"]][[1]])
    )
  )

  distEventsByEvent <- disturbanceEvents[, .(count = .N), by = eventID]
  data.table::setkey(distEventsByEvent, eventID)
  expect_equal(setNames(distEventsByEvent$count, distEventsByEvent$eventID), c(
    `2` = 12,
    `3` =  4,
    `5` = 95
  ))
})





