
utils::globalVariables(c(
  "year", "age", "ageCalc"
))

#' Adjust stand ages
#'
#' Adjust stand ages to represent a year before or after a given date.
#' Optionally include disturbance events that reset ages to year 0.
#'
#' @param standAges data.table. Table of stand ages
#' with an ID column and the numeric columns 'age'.
#' Optionally include a 'delay' column with regeneration delays for each stand.
#' A standard delay can also be provided with the \code{delay} argument.
#' @param yearInput integer. The year that the 'ages' column in \code{standAges} represents.
#' @param yearOutput integer. The year that stand ages must be adjusted to.
#' @param disturbanceEvents data.table. Optional.
#' Table of disturbance events with the ID column in \code{standAges}
#' and another column 'year' of when the disturbance occurred.
#' @param defaultAge integer. A default age for stands before the first disturbance.
#' If \code{yearOutput} precedes \code{yearInput}, stands with ages
#' lesser than the difference in these years may be calculated to have an age <0,
#' suggesting that a disturbance must have had occurred when age == 0.
#' if \code{disturbanceEvents} includes an event before this date,
#' stand age will be calculated from this date.
#' Otherwise, negative ages are replaced with \code{defaultAge}.
#' @param delay integer. Optional. Regeneration delay after a disturbance event.
#'
#' @return \code{standAges} with ages adjusted to \code{yearOutput}.
#'
#' @importFrom data.table as.data.table setkeyv
#' @export
adjustStandAges <- function(standAges, yearInput, yearOutput,
                            disturbanceEvents = NULL, defaultAge = NULL, delay = NULL){

  # Check standAges input
  standAges <- data.table::as.data.table(standAges)
  ageKey <- setdiff(names(standAges), "age")[[1]]
  if (!"age" %in% names(standAges)) stop("'standAges' must have column 'age'")

  # Adjust ages as if no disturbances
  ageAdjust <- copy(standAges)[, age := age + yearOutput - yearInput]

  # Add delays to table
  if (!is.null(delay)) ageAdjust$delay <- delay
  if (!"delay" %in% names(ageAdjust)) ageAdjust$delay <- 0

  if (!is.null(disturbanceEvents)){

    disturbanceEvents <- data.table::as.data.table(disturbanceEvents)

    if (!ageKey %in% names(disturbanceEvents)) stop("'disturbanceEvents' must have column '", ageKey, "'")
    if (!"year" %in% names(disturbanceEvents)) stop("'disturbanceEvents' must have column 'year'")

    # Warn if the input ages don't match the disturbance events
    if (yearOutput < yearInput){

      prevEvent <- disturbanceEvents[
        disturbanceEvents[[ageKey]] %in% standAges[[ageKey]] &
          disturbanceEvents$year <= yearInput,]

      if (nrow(prevEvent) > 0){

        prevEvent <- unique(prevEvent[, year := max(year), by = ageKey])
        prevEvent$ageCalc <- yearInput - prevEvent$year

        prevEvent <- merge(
          prevEvent,
          standAges[, c(ageKey, "age"), with = FALSE],
          by = ageKey, all.x = TRUE)

        agesTooHigh <- subset(prevEvent, age > ageCalc)[[ageKey]]
        if (length(agesTooHigh) > 0) warning(
          length(agesTooHigh),
          " stand(s) with unexpectedly high age(s) that does not reflect a reset to age 0 ",
          "after a provided disturbance event")

        rm(prevEvent)
      }
    }

    # Find the most recent disturbance before the year required
    lastEvent <- disturbanceEvents[
      disturbanceEvents[[ageKey]] %in% standAges[[ageKey]] &
        disturbanceEvents$year <= yearOutput,]

    if (nrow(lastEvent) > 0){

      lastEvent <- unique(lastEvent[, year := max(year), by = ageKey])

      # Add delays to table
      lastEvent <- merge(
        lastEvent,
        ageAdjust[, c(ageKey, "delay"), with = FALSE],
        by = ageKey, all.x = TRUE)

      # Calculate the age as the number of years after the disturbance
      lastEvent$age <- max(yearOutput - lastEvent$year - lastEvent$delay, 0)

      # Apply new ages
      ageAdjust <- rbind(
        ageAdjust[!ageAdjust[[ageKey]] %in% lastEvent[[ageKey]],][, c(ageKey, "age"), with = FALSE],
        lastEvent[, c(ageKey, "age"), with = FALSE]
      )

      rm(lastEvent)
    }
  }

  # Replace negative ages with the default age before a disturbance
  negAges <- (ageAdjust$age < 0) %in% TRUE
  if (any(negAges)){
    if (is.null(defaultAge)) stop(
      "'defaultAge' required for stands with an adjusted age of <0")
    ageAdjust$age[negAges] <- defaultAge
  }

  # Set key and return
  data.table::setkeyv(ageAdjust, ageKey)
  ageAdjust[, c(ageKey, "age"), with = FALSE]

}

