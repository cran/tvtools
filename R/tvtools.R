#' calculate.utilization
#'
#' @description Calculates the amount or proportion of time that a condition
#' (e.g., use of a medication) is met for each subject over a specified observation period.
#'
#' @param dat A data frame structured as a panel data set.
#' @param outcome.names A character vector of variable names from dat representing binary
#' conditions (1/0, TRUE/FALSE) whose utilization is to be calculated.
#' @param begin The numeric starting time for the interval of interest.
#' @param end The numeric ending time for the interval of interest.
#' @param id.name The character name of the identifying variable within dat,
#' used to track subjects across multiple rows.
#' @param t1.name The character name of the time-tracking variable within dat
#' representing the start (left endpoint) of each observation interval.
#' @param t2.name The character name of the time-tracking variable within dat
#' representing the end (right endpoint) of each observation interval.
#' @param type A character string specifying the type of utilization to calculate:
#' "rate" for proportion of time, or "total"/"count" for the count of days. Defaults to "rate".
#' @param full.followup A logical value indicating whether to include all subjects
#' or only those with fully observed records in the specified interval. Defaults to FALSE.
#' @param na.rm A logical value indicating whether missing values should be excluded
#' from the calculation. Defaults to TRUE.
#' @import data.table
#' @export
#'
#' @return Returns a data.table object that contains the calculated utilization
#' information for each subject specified by id.name. If type is "rate", it returns
#' the proportion of the interval [begin, end) during which the conditions specified
#' in outcome.names are met. If type is "total" or "count", it returns the total count
#' of days within the interval where the conditions are met. The result is grouped by
#' the id.name variable, ensuring each subject's data is aggregated separately.


calculate.utilization <- function(dat, outcome.names, begin, end, id.name = "id", t1.name = "t1", t2.name = "t2", type = "rate", full.followup = FALSE, na.rm = TRUE){
  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }

  all.variable.names <- unique(names(dat))

  if((id.name %in% all.variable.names) == FALSE){
    stop("Error:  id.name must be a variable name from names(dat).")
  }
  if((t1.name %in% all.variable.names) == FALSE){
    stop("Error: t1.name must be a variable name from names(dat).")
  }
  if((t2.name %in% all.variable.names) == FALSE){
    stop("Error: t2.name must be a variable name from names(dat).")
  }
  outcome.names <- unique(outcome.names[outcome.names %in% all.variable.names])
  if(length(outcome.names) == 0){
    outcome.names <- NULL
  }

  # require(data.table)
  format.dt <- is.data.table(x = dat)

  setDT(x = dat)

  if(type %in% c("total", "count")){
    type <- "total"
  }
  if(type != "total"){
    type <- "rate"
  }

  binary.outcomes.tab <- dat[, .(variable = outcome.names, qualifies = lapply(X = .SD, FUN = function(x){return(is.logical(x) | (is.numeric(x) & mean(sort(unique(x[!is.na(x)])) == c(0,1)) == 1))})), .SDcols = outcome.names]

  outcome.names <- binary.outcomes.tab[qualifies == TRUE, variable]

  if(full.followup == TRUE){
    qualifying.ids <- dat[, .(qualifies = one.record.calculate.utilization(x = rep.int(x = 1, times = length(get(t1.name))), t1 = get(t1.name), t2 = get(t2.name), begin = begin, end = end, type = "total") == end - begin), by = id.name][qualifies == TRUE, unique(get(id.name))]
  }
  if(full.followup == FALSE){
    qualifying.ids <- dat[, unique(get(id.name))]
  }

  tab <- dat[get(id.name) %in% qualifying.ids, lapply(X = .SD, FUN = "one.record.calculate.utilization", t1 = get(t1.name), t2 = get(t2.name), begin = begin, end = end, type = type), .SDcols = outcome.names, by = id.name]

  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = tab)
  }

  return(tab)
}


# Internal function, not exported
calc.event.time <- function(outcome, times, time.function = "min", na.rm = TRUE){
  w <- which(outcome == 1)
  if(length(w) > 0){
    res <- unlist(lapply(X = list(times[w]), FUN = time.function, na.rm = na.rm))
    if(is.numeric(times)){
      res <- as.numeric(res)
    }
    if(is.integer(times)){
      res <- as.integer(res)
    }
  }
  if(length(w) == 0){
    if(is.numeric(times)){
      res <- NA_real_
    }
    if(is.integer(times)){
      res <- NA_integer_
    }
  }
  return(res)
}

#' count.events
#'
#' @description Creates a count of the number of events that occurred within each group
#' from a panel data structure, based on specified binary outcome variables.
#'
#' @param dat A data frame structured as a panel data set.
#' @param outcome.names A character vector of variable names from dat that are expected
#' to be binary (1/0, TRUE/FALSE). The function calculates the count of these variables
#' being TRUE/1 in the specified interval. Variables not found in dat or not binary
#' will be disregarded.
#' @param grouping.variables A character vector of variable names from dat to group the
#' resulting counts. If NULL, the function computes the overall count without grouping.
#' @param type Specifies the counting method: "distinct" for counting only new occurrences
#' separated by zeros (useful for events like hospitalizations spanning multiple records),
#' or "overall" (default) for counting all records with the value of TRUE/1.
#' @param na.rm A logical indicating whether missing values should be ignored in the
#' calculations. Defaults to TRUE.
#' @return Returns a data.table object containing the counts of events. The counts are
#' aggregated based on the specified 'grouping.variables'. Each row corresponds to a
#' group defined by 'grouping.variables' and contains counts of the specified
#' 'outcome.names'. If 'type' is "distinct", the count reflects distinct occurrences of
#' events; if 'type' is "overall", it reflects the total count of records with TRUE/1
#' for the 'outcome.names'. The output structure makes it easy to understand the
#' distribution of events across the different groups or categories defined in the
#' data set.
#' @import DTwrappers
#' @import data.table
#'

#' @export

count.events <- function(dat, outcome.names, grouping.variables = NULL, type = "overall", na.rm = TRUE){
  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }
  # require(data.table)
  # require(DTwrappers)
  all.variable.names <- unique(names(dat))


  outcome.names <- unique(outcome.names[outcome.names %in% all.variable.names])
  if(length(outcome.names) == 0){
    stop("outcome.names must include variable names from names(dat).")
  }

  grouping.variables <- unique(grouping.variables[grouping.variables %in% all.variable.names])
  if(length(grouping.variables) == 0){
    grouping.variables <- NULL
  }

  # require(data.table)
  format.dt <- is.data.table(x = dat)

  setDT(x = dat)

  res <- dat[, lapply(X = .SD, FUN = "internal.count.events", type = type, na.rm = na.rm), .SDcols = outcome.names, keyby = grouping.variables]

  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = res)
  }

  return(res)
}

#' create.baseline
#'
#' @description Creates a baseline cohort from panel data at the initial time point (t=0).
#' This function is tailored for scenarios where the baseline information is critical,
#' particularly when the outcomes are considered in terms of the time elapsed since the baseline.
#' For time points other than the baseline, consider using the function `cross.sectional.data()`.
#'
#' @param dat A data frame or data table structured as panel data.
#' @param id.name The character name of the identifying variable within dat,
#' used to track subjects across multiple rows of data.
#' @param t1.name The character name of the time variable within dat that
#' represents the left endpoints (start of the observation interval).
#' @param t2.name The character name of the time variable within dat that
#' represents the right endpoints (end of the observation interval).
#' @param outcome.names A character vector of outcome variable names from dat,
#' expected to be binary. The function identifies the first occurrence of each
#' outcome being 1 for each unique id.
#' @export
#'
#' @return Returns a data frame or data table (depending on the input type) representing
#' the baseline cohort of the provided panel data. This output is essentially a snapshot
#' of the data at the initial time point (t=0). The function extracts and formats this
#' baseline information based on the specified id, time, and outcome variables. The resulting
#' dataset provides a foundation for subsequent analyses, particularly for tracking the
#' onset or occurrence of specified outcomes from the start of the observation period.


create.baseline <- function(dat, id.name = "id", t1.name = "t1", t2.name = "t2", outcome.names = NULL){
  return(cross.sectional.data(dat = dat, time.point = 0, id.name = id.name, t1.name = t1.name, t2.name = t2.name, outcome.names = outcome.names))
}


#' cross.sectional.data
#'
#' @description Creates a cross-sectional cohort from a panel data structure at a specified
#' time point. It focuses on outcome variables, indicating the time elapsed after the
#' specified point until an event occurred.
#'
#' @param dat A data frame structured as panel data.
#' @param time.point The numeric time at which to create the cross-sectional data.
#' This represents the point in time for which the data snapshot is taken. Subjects are
#' included in the snapshot if they have data recorded at this time point.
#' @param id.name The character name of the identifying variable within dat, used to
#' track subjects across multiple rows of data.
#' @param t1.name The character name of the time variable within dat representing the
#' start (left endpoint) of observation intervals.
#' @param t2.name The character name of the time variable within dat representing the
#' end (right endpoint) of observation intervals.
#' @param outcome.names A character vector of outcome variable names from dat, which
#' should be binary. The function calculates the time since the time.point that each
#' outcome first becomes 1 for each unique id.
#' @param relative.followup A logical indicating whether to return the outcomes in
#' absolute time (FALSE) or relative to the time.point (TRUE). Outcomes before the
#' time.point are disregarded when TRUE.
#' @import data.table
#' @export
#'
#' @return Returns a data frame or data table, depending on the input, containing the
#' cross-sectional data extracted at the specified time.point. The dataset includes each
#' subject observed at the time.point, with the relevant outcomes and other variables
#' adjusted based on the specified parameters. If outcome.names are provided, it includes
#' the calculated times from the specified time.point to the first occurrence of the
#' outcomes for each subject. If relative.followup is TRUE, these times are relative to
#' the time.point; otherwise, they are in absolute terms. The structure of the returned
#' data is ideal for analyses focusing on the status of subjects at a specific moment
#' in the study period.


cross.sectional.data <- function(dat, time.point = 0, id.name = "id", t1.name = "t1", t2.name = "t2", outcome.names = NULL, relative.followup = FALSE){

  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }

  all.variable.names <- unique(names(dat))

  if((id.name %in% all.variable.names) == FALSE){
    stop("Error:  id.name must be a variable name from names(dat).")
  }
  if((t1.name %in% all.variable.names) == FALSE){
    stop("Error: t1.name must be a variable name from names(dat).")
  }
  if((t2.name %in% all.variable.names) == FALSE){
    stop("Error: t2.name must be a variable name from names(dat).")
  }

  outcome.names <- unique(outcome.names[outcome.names %in% all.variable.names])
  if(length(outcome.names) == 0){
    outcome.names <- NULL
  }

  # require(data.table)
  format.dt <- is.data.table(x = dat)

  setDT(x = dat)

  w <- which(dat[,get(t1.name)] <= time.point & dat[,get(t2.name)] > time.point)

  baseline <- dat[w, .SD, .SDcols = names(dat)[!(names(dat) %in% outcome.names)]]

  res <- baseline[, .SD, .SDcols = names(baseline)[!(names(baseline) %in% c(t1.name, t2.name))]]
  res[, eval("cross.sectional.time") := time.point]

  if(!is.null(outcome.names)){

    first.ones <- first.event(dat = dat[get(t1.name) >= time.point,], id.name = id.name, outcome.names = outcome.names, t1.name = t1.name, append.to.table = FALSE)

    if(relative.followup == T){
      # Subtract cross-sectional time as baseline to produce relative time.
      first.ones[, (sprintf("%s.first.event", outcome.names)) := lapply(X = .SD, FUN = function(x, y){return(x - y)}, y = time.point), .SDcols = sprintf("%s.first.event", outcome.names)]
    }

    folls <- followup.time(dat = dat, id.name = id.name, t1.name = t1.name, t2.name = t2.name, append.to.data = F)

    if(relative.followup == T){
      # Subtract cross-sectional time as baseline to produce relative time.
      folls[, eval("followup.time") := get("followup.time") - time.point]
    }
    event.time <- merge(x = first.ones, y = folls[, .SD, .SDcols = c(id.name, "followup.time")], by = id.name)

    res <- merge(x = baseline, y = event.time, by = id.name, all.x = TRUE)

    res[, eval("cross.sectional.time") := time.point]

    res <- res[, .SD, .SDcols = names(res)[!(names(res) %in% c(t1.name, t2.name))]]
  }

  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(res)
  }

  return(res[])
}


#' crude.rates
#'
#' @description Calculates the rate of specified events relative to the amount of
#' follow-up time, based on panel data. It allows for the segmentation of data into different
#' time periods (eras) and computes the event rates for these periods.
#'
#' @param dat A data frame structured as panel data.
#' @param outcome.names A character vector of variable names from dat, expected to be binary,
#' representing the events of interest. The function calculates the rates of these events
#' within the specified time intervals. Variables not in dat or non-binary will be ignored.
#' @param cut.points A numeric vector specifying the end points of time intervals
#' for rate calculation. The data is split into eras based on these points, and rates
#' are computed for each interval.
#' @param time.multiplier A numeric value that scales the computed rates, useful for
#' converting rates to a standard time unit (e.g., per 1000 person-years).
#' @param id.name The character name of the identifying variable within dat, used for
#' tracking individuals across multiple data rows.
#' @param t1.name The character name of the time variable within dat representing the
#' start (left endpoint) of observation intervals.
#' @param t2.name The character name of the time variable within dat representing the
#' end (right endpoint) of observation intervals.
#' @param grouping.variables A character vector of variables in dat to group the results by.
#' @param type Specifies the counting method: "distinct" for counting only new occurrences
#' separated by zeros, or "overall" (default) for counting all records with the event.
#' @param na.rm A logical indicating whether to exclude missing values from the calculations.
#' @param era.name The character string used to name the time period column in the resulting table.
#' @import data.table
#' @export
#'
#' @return Returns a data table containing the calculated rates of the specified events for each
#' group and time period (era). The rates are presented alongside the grouping variables and the
#' specified era. Each row corresponds to a unique combination of the grouping variables and time
#' period, with the event rates adjusted according to the specified time.multiplier. The output
#' facilitates the comparison of event rates across different segments of follow-up time and
#' subgroups within the data.


crude.rates <- function(dat, outcome.names, cut.points = NULL, time.multiplier = 1, id.name = "id", t1.name = "t1", t2.name = "t2", grouping.variables = NULL, type = "overall", na.rm = TRUE, era.name = "period"){

  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }

  all.variable.names <- unique(names(dat))

  if((t1.name %in% all.variable.names) == FALSE){
    stop("Error: t1.name must be a variable name from names(dat).")
  }
  if((t2.name %in% all.variable.names) == FALSE){
    stop("Error: t2.name must be a variable name from names(dat).")
  }
  outcome.names <- unique(outcome.names[outcome.names %in% all.variable.names])
  if(length(outcome.names) == 0){
    outcome.names <- NULL
  }

  grouping.variables <- unique(grouping.variables[grouping.variables %in% all.variable.names])
  if(length(grouping.variables) == 0){
    grouping.variables <- NULL
  }

  # require(data.table)
  format.dt <- is.data.table(x = dat)


  setDT(x = dat)

  cut.points <- as.numeric(sort(unique(cut.points[cut.points <= dat[,max(get(t2.name))] & cut.points >= dat[, min(get(t1.name))]])))


  dd <- era.splits(dat = dat, cut.points = cut.points, id.name = id.name, t1.name = t1.name, t2.name = t2.name)

  num.cut.points <- length(cut.points)
  if(num.cut.points == 0){
    dd[, eval(era.name) := "All Follow-Up"]
  }
  if(num.cut.points > 0){
    for(i in 1:(num.cut.points + 1)){
      if(i == 1){
        dd[get(t2.name) <= cut.points[i], eval(era.name) := sprintf("Before %s", cut.points[i])]
      }
      if(i > 1 & i < num.cut.points + 1){
        dd[get(t1.name) >= cut.points[i-1] & get(t2.name) <= cut.points[i], eval(era.name) := sprintf("[%s, %s)", cut.points[i-1], cut.points[i])]
      }
      if(i == num.cut.points + 1){
        dd[get(t1.name) >= cut.points[i-1], eval(era.name) := sprintf("On or After %s", cut.points[i-1])]
      }
    }
  }


  the.counts <- count.events(dat = dd, outcome.names = outcome.names, grouping.variables = c(grouping.variables, era.name), type = type, na.rm = na.rm)

  observed <- dd[, .(observation.time = sum(get(t2.name) - get(t1.name))), by = c(grouping.variables, era.name)]

  res <- merge(x = observed, y = the.counts, by = c(grouping.variables, era.name))

  for(i in 1:length(outcome.names)){
    res[, eval(sprintf("%s.rate", outcome.names[i])) := get(outcome.names[i]) * time.multiplier / observation.time]
  }

  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = res)
  }

  return(res[])
}

#' era.splits
#'
#' @description Restructures panel data by dividing records into distinct eras based on specified
#' cut points. Rows that span multiple eras are split into separate rows for each era they
#' encompass. For example, a record spanning from 0 to 2 years could be split into two records:
#' one for the first year (0-1) and another for the second year (1-2), if the cut points are
#' set at each year mark. This process is useful for analyzing data within specific time intervals.
#'
#' @param dat A data frame structured as panel data.
#' @param cut.points A numeric vector specifying the endpoints of each era. Each value defines
#' the end of one era and the beginning of the next, allowing records to be split into intervals
#' such as [min(x), 10), [10, 20), and [20, max(x)). A row with t1 = 0 and t2 = 30, and cut points
#' at 10 and 20, would be divided into intervals of [0,10), [10,20), and [20,30).
#' @param id.name The character name of the identifying variable within dat, used for
#' tracking subjects across multiple rows.
#' @param t1.name The character name of the time variable within dat representing the
#' start (left endpoint) of observation intervals.
#' @param t2.name The character name of the time variable within dat representing the
#' end (right endpoint) of observation intervals.
#' @import data.table
#' @export
#'
#' @return Returns a data table that has been restructured to reflect the specified era splits.
#' Original rows overlapping multiple eras are divided into multiple rows, each representing a
#' discrete interval within the specified eras. The function ensures each subject's observation
#' period is accurately represented according to the specified time intervals. The output is
#' sorted by the identifying variable and the start time of each interval, facilitating further
#' analysis or processing that depends on temporal segmentation.

era.splits <- function(dat, cut.points, id.name = "id", t1.name = "t1", t2.name = "t2"){

  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }

  all.variable.names <- unique(names(dat))

  if((t1.name %in% all.variable.names) == FALSE){
    stop("Error: t1.name must be a variable name from names(dat).")
  }
  if((t2.name %in% all.variable.names) == FALSE){
    stop("Error: t2.name must be a variable name from names(dat).")
  }

  # require(data.table)
  format.dt <- is.data.table(x = dat)

  setDT(x = dat)

  cut.points <- sort(unique(cut.points[cut.points <= dat[,max(get(t2.name))] & cut.points >= dat[, min(get(t1.name))]]))

  res <- dat

  res[, eval(t1.name) := as.numeric(get(t1.name))]
  res[, eval(t2.name) := as.numeric(get(t2.name))]

  for(cutoff in cut.points){
    w <- which(res[,get(t1.name)] < cutoff & res[,get(t2.name)] > cutoff)

    if(length(w)>0){
      dd <- res[w,]
      dd[, eval(t1.name) := cutoff]
      res[w, eval(t2.name) := cutoff]

      res <- rbindlist(l = list(res, dd), fill = TRUE)
    }
  }

  setorderv(x = res, cols = c(id.name, t1.name))
  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = res)
  }

  return(res)
}


#' event.time
#'
#' @description Calculates the time to an event in a panel data structure, based on binary
#' outcome variables. It can determine the time of the first, last, or other statistical measures
#' (like mean or median) of event occurrences for each subject.
#'
#' @param dat A data frame structured as panel data.
#' @param id.name The character name of the identifying variable within dat, used for
#' tracking subjects across multiple rows.
#' @param outcome.names A character vector of variable names from dat that are expected to be binary,
#' representing the events of interest. The function calculates the time to these events
#' based on the specified function (e.g., first occurrence).
#' @param t1.name The character name of the time variable within dat representing the
#' start (left endpoint) of observation intervals.
#' @param time.function The function to apply to the event times for each subject and outcome.
#' Options include "min" for the time of the first event, "max" for the last event, and
#' others like "mean" or "median" for average or median event times.
#' @param append.to.table A logical indicating whether to append the calculated event times
#' as new columns to the original data.frame (TRUE) or return them as a separate data frame (FALSE).
#' @param event.name The name to give the event time columns when they are appended to the data.
#' @import data.table
#' @export
#'
#' @return If append.to.table is FALSE, returns a data table with the calculated event times for
#' each subject and specified outcome, keyed by the id.name. Each outcome will have its own column
#' named according to the original outcome name with the specified event.name appended (e.g.,
#' "outcome.first.event" for the first event times). If append.to.table is TRUE, the original data
#' table is returned with these new columns appended. This facilitates analyses focused on the timing
#' of events relative to the subjects' observation periods in the panel data.


event.time <- function(dat, id.name, outcome.names, t1.name, time.function = "min", append.to.table = FALSE, event.name = "first.event"){

  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }

  all.variable.names <- unique(names(dat))

  if(!is.null(id.name)){
    if((id.name %in% all.variable.names) == FALSE){
      stop("Error:  id.name must be a variable name from names(dat) that identifies subjects across the rows.")
    }
  }
  if((t1.name %in% all.variable.names) == FALSE){
    stop("Error: t1.name must be a variable name from names(dat) that represents the end of a time interval.")
  }

  # require(data.table)
  format.dt <- is.data.table(x = dat)

  setDT(x = dat)

  outcome.names <- outcome.names[outcome.names %in% names(dat)]

  if(length(outcome.names) == 0){
    stop("Error: outcome.names must be variable names in names(dat).")
  }

  res <- dat[, lapply(X = .SD, FUN = "calc.event.time", times = get(t1.name), time.function = time.function), .SDcols = outcome.names, by = id.name]

  setnames(x = res, old = outcome.names, new = sprintf("%s.%s", outcome.names, event.name))

  if(append.to.table == TRUE){
    res <- merge(x = dat, y = res, by = "id", all.x = TRUE)
  }

  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = res)
  }

  return(res)
}


#' first.event
#'
#' @description Calculates the time to the first occurrence of specified events for each
#' subject in a panel data structure. This function is particularly useful for longitudinal
#' data analysis where the timing of first events is crucial for subsequent analyses.
#'
#' @param dat A data frame structured as panel data.
#' @param id.name The character name of the identifying variable within dat, used to
#' track subjects across multiple rows.
#' @param outcome.names A character vector of variable names from dat, expected to be binary,
#' representing the events of interest. The function determines the first time each outcome
#' becomes true (1) for each unique id.
#' @param t1.name The character name of the time variable within dat representing the
#' start (left endpoint) of observation intervals.
#' @param append.to.table A logical indicating whether to append the calculated first event
#' times as new columns to the original data.frame (TRUE) or return them as a separate data frame (FALSE).
#' @param event.name The name to give the event time columns when they are appended to the data,
#' specifically for the first event times.
#' @export
#'
#' @return If append.to.table is FALSE, the function returns a data table with the calculated
#' times to the first event for each subject and specified outcome, keyed by the id.name. Each
#' outcome will have its own column named according to the event.name parameter, appended with
#' the outcome name (e.g., "outcome.first.event" for first event times). If append.to.table
#' is TRUE, the original data table is returned with these new columns appended. This facilitates
#' detailed analysis on the timing of first events in relation to the subjects' overall observation
#' periods within the panel data structure.


first.event <- function(dat, id.name, outcome.names, t1.name, append.to.table = FALSE, event.name = "first.event"){
  res <- event.time(dat = dat, id.name = id.name, outcome.names = outcome.names, t1.name = t1.name, time.function = "min", append.to.table = append.to.table, event.name = event.name)

  return(res)
}


#' first.panel.gap
#'
#' @description Identifies the first occurrence of a gap in observation periods for each unique subject
#' within a panel data structure. A gap is defined as a period where no data were recorded for an
#' expected interval between observations.
#'
#' @param dat A data frame structured as panel data.
#' @param id.name The character name of the identifying variable within dat, used to track subjects
#' across multiple rows of data.
#' @param t1.name The character name of the time variable within dat representing the start (left endpoint)
#' of observation intervals.
#' @param t2.name The character name of the time variable within dat representing the end (right endpoint)
#' of observation intervals.
#' @param gap.name A character value for the name of the variable to be used or created that specifies
#' whether a gap is observed before the record.
#' @param first.value The numeric value indicating the expected beginning time of the observation period for each subject.
#' @param expected.gap.between The numeric value indicating the expected amount of time between the end of one
#' record and the start of the next; the default is zero, assuming continuous observation.
#' @param append.to.table A logical value indicating whether the identified first gap times should be appended
#' as a new column to the existing data.frame (TRUE) or returned as a separate data frame (FALSE, default).
#' @export
#'
#' @return If append.to.table is FALSE, the function returns a data table with the identified first gap time
#' for each subject, keyed by the id.name. Each subject will have a corresponding gap time, indicating the
#' first observed gap in their data. If append.to.table is TRUE, the original data table is returned with a
#' new column appended, containing the first gap times for each subject. This functionality is critical for
#' longitudinal studies where maintaining continuous observation of subjects is necessary, and identifying
#' gaps can highlight data collection issues or subject attrition.


first.panel.gap <- function(dat, id.name = "id", t1.name = "t1", t2.name = "t2", gap.name = "gap_before", first.value = 0, expected.gap.between = 0, append.to.table = FALSE){

  the.gaps <- panel.gaps(dat = dat, id.name = id.name, t1.name = t1.name, t2.name = t2.name, gap.name = gap.name, first.value = first.value, expected.gap.between = expected.gap.between)

  res <- first.event(dat = the.gaps, id.name = id.name, t1.name = t1.name, outcome.names = gap.name, append.to.table = append.to.table)

  return(res)
}


#' followup.time
#'
#' @description Computes the total or maximum follow-up time for each subject in a panel data structure,
#' accounting for observation endpoints like death, loss to follow-up, or study conclusion.
#'
#' @param dat A data.frame structured as panel data.
#' @param id.name The character name of the identifying variable within dat, used to
#' track subjects across multiple rows.
#' @param t1.name The character name of the time variable within dat representing the
#' start (left endpoint) of observation intervals.
#' @param t2.name The character name of the time variable within dat representing the
#' end (right endpoint) of observation intervals.
#' @param followup.name The character name for the new variable to display the calculated
#' follow-up time for each subject.
#' @param calculate.as A character value specifying the calculation method for follow-up time:
#' "max" for the maximum observed time point for each subject, or "total" for the total observed
#' time across all records for each subject (default). Note: "max" and "total" yield the same
#' result when records start at time 0, have no gaps, and don't overlap.
#' @param append.to.data A logical indicating whether to append the calculated follow-up time
#' as a new column to the original data.frame (TRUE), or return it as a separate data frame (FALSE, default).
#' @import data.table
#' @export
#'
#' @return Returns a modified version of the input data frame or a new data frame based on the
#' append.to.data parameter. If append.to.data is TRUE, the original data frame is returned with an
#' additional column named as specified by followup.name, containing the calculated follow-up times
#' for each subject. If FALSE, a new data frame is returned containing the id.name and the calculated
#' follow-up times under the followup.name column. This functionality is essential for analyses that
#' require understanding the duration of subject participation or observation within the study period.


followup.time <- function(dat, id.name = "id", t1.name = "t1", t2.name="t2", followup.name = "followup.time", calculate.as = "total", append.to.data = FALSE){

  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }

  value.max <- "max"
  value.total <- "total"

  all.variable.names <- unique(names(dat))

  if((id.name %in% all.variable.names) == FALSE){
    stop("Error:  id.name must be a variable name from names(dat) that identifies subjects across the rows.")
  }
  if(calculate.as == value.total){
    if((t1.name %in% all.variable.names) == FALSE){
      stop("Error: t1.name must be a variable name from names(dat) that represents the end of a time interval.")
    }
  }

  if((t2.name %in% all.variable.names) == FALSE){
    stop("Error: t2.name must be a variable name from names(dat) that represents the end of a time interval.")
  }

  # require(data.table)
  format.dt <- is.data.table(x = dat)

  setDT(dat)

  if(calculate.as != value.max){
    calculate.as <- value.total
  }

  if(append.to.data == TRUE){
    if(calculate.as == value.max){
      tab <- dat[, eval(followup.name) := max(get(t2.name)), by = id.name]
    }
    if(calculate.as == value.total){
      tab <- dat[, eval(followup.name) := sum(get(t2.name) - get(t1.name), na.rm = TRUE), by = id.name]
    }
  }
  if(append.to.data == FALSE){
    if(calculate.as == value.max){
      tab <- dat[, .(V1 = max(get(t2.name))), by = id.name]
      setnames(x = tab, old = "V1", new = followup.name)
    }
    if(calculate.as == value.total){
      tab <- dat[, .(V1 = sum(get(t2.name) - get(t1.name), na.rm = TRUE)), by = id.name]
      setnames(x = tab, old = "V1", new = followup.name)
    }
  }

  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = tab)
  }

  return(tab)
}

# internal function, do not export

id.panel.overlaps.one.patient <- function(patient.dat, id.name, t1.name, t2.name, row.index.name){
  # require(data.table)
  setDT(patient.dat)

  beginning.times <- patient.dat[, get(t1.name)]
  ending.times <- patient.dat[, get(t2.name)]

  overlapping.results <- patient.dat[, sum((get(t1.name) > beginning.times & get(t1.name) < ending.times) | (get(t2.name) < beginning.times & get(t2.name) > ending.times)) > 0, by = row.index.name][, sum(get("V1")) > 0]

  return(overlapping.results)
}

# Internal function, do not export
# Assumes that the data are all for a single patient (same id) and sorted in increasing order of t1.name
identify.panel.gaps.one.patient <- function(patient.dat, t1.name, t2.name, first.value = 0, expected.gap.between = 0){
  # require(data.table)
  setDT(patient.dat)

  gap.first.row <- (patient.dat[1, get(t1.name) > first.value])
  n <- patient.dat[, .N]

  if(n == 1){
    res <- gap.first.row
  }
  if(n > 1){
    t2.values <- patient.dat[1:(n-1), get(t2.name)]
    gaps.other.rows <- patient.dat[2:n, get(t1.name) > t2.values + expected.gap.between]
    res <- c(gap.first.row, gaps.other.rows)
  }
  return(res)
}

# Internal Function:  do not export.

# x:  A logical or binary numeric (0/1) vector.

# type:  The type of counting to perform.  If "distinct", then only new occurrences (separated by zeros) are counted.  Distinct counts handle events such as hospitalizations that span multiple records.  Otherwise the total number of records with a value of TRUE or 1 are counted.

# na.rm:  A (logical) variable indicating whether missing values should be removed when calculating the utilization.

internal.count.events <- function(x, type = "overall", na.rm = T){

  if(type == "distinct"){
    y <- c(0, x[1:(length(x)-1)])

    the.count <- sum(x == 1 & (x != y), na.rm = na.rm)
  }
  if(type != "distinct"){
    the.count = sum(x == 1, na.rm = na.rm)
  }
  return(the.count)
}


#' last.event
#'
#' @description Calculates the time to the last occurrence of specified binary events
#' for each subject in a panel data structure. This is particularly useful for understanding
#' the timing of the last event in longitudinal analyses.
#'
#' @param dat A data frame structured as panel data.
#' @param id.name The character name of the identifying variable within dat, used to
#' track subjects across multiple rows of data.
#' @param outcome.names A character vector of variable names from dat, expected to be binary,
#' indicating the events of interest. The function determines the last time each outcome
#' becomes true (1) for each unique id.
#' @param t1.name The character name of the time variable within dat representing the
#' start (left endpoint) of observation intervals.
#' @param append.to.table A logical indicating whether to append the calculated last event
#' times as new columns to the original data frame (TRUE) or return them as a separate data frame (FALSE).
#' @param event.name The name to give the event time columns when they are appended to the data,
#' specifically for the last event times.
#' @export
#'
#' @return If append.to.table is FALSE, the function returns a data table with the calculated
#' times to the last event for each subject and specified outcome, keyed by the id.name. Each
#' outcome will have its own column named according to the event.name parameter, appended with
#' the outcome name (e.g., "outcome.last.event" for last event times). If append.to.table
#' is TRUE, the original data table is returned with these new columns appended. This enables
#' detailed analysis on the timing of last events relative to the subjects' overall observation
#' periods within the panel data.


last.event <- function(dat, id.name, outcome.names, t1.name, append.to.table = FALSE, event.name = "last.event"){
  res <- event.time(dat = dat, id.name = id.name, outcome.names = outcome.names, t1.name = t1.name, time.function = "max", append.to.table = append.to.table, event.name = event.name)

  return(res)
}


#' last.panel.gap
#'
#' @description Identifies the time point of the last observed gap in observation for each unique
#' subject in a panel data structure. A gap represents a missing period between recorded observations,
#' important for assessing data completeness and continuity.
#'
#' @param dat A data frame structured as panel data.
#' @param id.name The character name of the identifying variable within dat, used to
#' track subjects across multiple rows of data.
#' @param t1.name The character name of the time variable within dat representing the
#' start (left endpoint) of observation intervals.
#' @param t2.name The character name of the time variable within dat representing the
#' end (right endpoint) of observation intervals.
#' @param gap.name A character value for the name of the variable indicating whether a
#' gap is observed before the record.
#' @param first.value The numeric value indicating the expected beginning time of the
#' observation period for each subject.
#' @param expected.gap.between The numeric value specifying the expected time between
#' the end of one record and the start of the next; defaults to zero for continuous observation.
#' @param append.to.table A logical indicating whether to append the identified last gap
#' times as a new column to the existing data.frame (TRUE) or return them as a separate data frame (FALSE, default).
#' @export
#'
#' @return If append.to.table is FALSE, returns a data table with the time points of the last gap
#' for each subject, keyed by the id.name. Each row will correspond to a unique subject, including
#' the time point of their last observed gap, if any. If append.to.table is TRUE, the original data
#' table is returned with an additional column containing these time points for each subject. This
#' functionality aids in analyzing and understanding the patterns of missing observations or breaks
#' in data collection within the study period.


last.panel.gap <- function(dat, id.name, t1.name, t2.name, gap.name = "gap_before", first.value = 0, expected.gap.between = 0, append.to.table = FALSE){

  the.gaps <- panel.gaps(dat = dat, id.name = id.name, t1.name = t1.name, t2.name = t2.name, gap.name = gap.name, first.value = first.value, expected.gap.between = expected.gap.between)

  res <- last.event(dat = the.gaps, id.name = id.name, t1.name = t1.name, outcome.names = gap.name, append.to.table = append.to.table)

  return(res)
}


#' measurement.rate
#'
#' @description Calculates the proportion of unique subjects remaining under observation
#' and those no longer observed at a specified time point during the follow-up period
#' within a panel data structure. This metric is crucial for evaluating the coverage and
#' retention of subjects in longitudinal studies.
#'
#' @param dat A data frame structured as panel data.
#' @param id.name The character name of the identifying variable within dat, used to
#' track subjects across multiple rows.
#' @param t1.name The character name of the time variable within dat representing the
#' start (left endpoint) of observation intervals.
#' @param t2.name The character name of the time variable within dat representing the
#' end (right endpoint) of observation intervals.
#' @param time.point A numeric value specifying the point in time at which the measurement
#' rate should be calculated. Subjects observed at this point are considered active.
#' @param grouping.variables A character vector of variable names from dat used to group
#' the results. Proportions are calculated within these groups.
#' @import data.table
#' @export
#'
#' @return Returns a data table that includes the specified grouping variables, the number
#' of subjects observed at the given time.point, the total number of unique subjects, and
#' two calculated rates: 'rate.observed' and 'rate.not.observed'. 'rate.observed' is the
#' proportion of subjects active at the specified time point, and 'rate.not.observed' is the
#' proportion no longer observed. This output is instrumental for analyzing subject retention
#' and attrition over the course of the study.


measurement.rate <- function(dat, id.name = "id", t1.name = "t1", t2.name = "t2", time.point = 0, grouping.variables = NULL){

  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }

  all.variable.names <- unique(names(dat))

  if((t1.name %in% all.variable.names) == FALSE){
    stop("Error: t1.name must be a variable name from names(dat).")
  }
  if((t2.name %in% all.variable.names) == FALSE){
    stop("Error: t2.name must be a variable name from names(dat).")
  }

  grouping.variables <- unique(grouping.variables[grouping.variables %in% all.variable.names])
  if(length(grouping.variables) == 0){
    grouping.variables <- NULL
  }

  # require(data.table)
  format.dt <- is.data.table(x = dat)

  setDT(x = dat)

  obs.count <- dat[get(t1.name) <= time.point & get(t2.name) > time.point, .(time = time.point, observed = length(unique(get(id.name)))), by = eval(grouping.variables)]

  full.count = dat[, .(total.subjects = length(unique(id))), by = grouping.variables]

  if(is.null(grouping.variables)){
    res <- data.table(obs.count, full.count)
  }
  if(!is.null(grouping.variables)){
    res <- merge(x = obs.count, y = full.count, by = grouping.variables)
  }

  res[, eval("rate.observed") := observed / total.subjects]
  res[, eval("rate.not.observed") := 1 - observed / total.subjects]

  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = res)
  }

  return(res[])
}



# Internal function.  Do not export.

# Calculates utilization for one variable for a single patient.

one.record.calculate.utilization <- function(x, t1, t2, begin = 0, end = 365, type = "rate", na.rm = TRUE){
  w <- which(t1 < end & t2 > begin)

  tot <- sum((pmin(end, t2[w]) - pmax(begin, t1[w])) * x[w], na.rm = na.rm)

  if(type == "total"){
    res <- tot
  }
  if(type != "total"){
    res <- tot / sum((pmin(end, t2[w]) - pmax(begin, t1[w])), na.rm = na.rm)
  }
  return(res)
}


#' panel.gaps
#'
#' @description Identifies gaps in observation periods within panel data, marking each record
#' with a flag indicating whether it was preceded by a gap. A gap reflects a missing period
#' of observation between this and the previous record for a subject, which is crucial for
#' assessing data integrity and continuity.
#'
#' @param dat A data frame structured as panel data.
#' @param id.name The character name of the identifying variable within dat, used to track
#' subjects across multiple rows of data.
#' @param t1.name The character name of the time variable within dat representing the start
#' (left endpoint) of observation intervals.
#' @param t2.name The character name of the time variable within dat representing the end
#' (right endpoint) of observation intervals.
#' @param gap.name A character value specifying the name of the new variable to be created that
#' indicates whether a gap is observed before the record.
#' @param first.value The numeric expected beginning time of the observation period for each subject.
#' @param expected.gap.between The numeric amount of time expected between the end of one record
#' and the start of the next; defaults to zero for continuous observation without expected gaps.
#' @import data.table
#' @export
#'
#' @return Returns the original data frame with an additional column (named according to the
#' gap.name parameter) for each record, indicating whether a gap in observation was detected
#' before that record. The gap flag is determined based on the specified expected beginning time
#' and the expected gap between records. This enhanced data frame is instrumental for subsequent
#' analyses that require understanding of observation continuity and identifying subjects with
#' missing data periods.


panel.gaps <- function(dat, id.name = "id", t1.name = "t1", t2.name = "t2", gap.name = "gap_before", first.value = 0, expected.gap.between = 0){

  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }

  all.variable.names <- unique(names(dat))

  if((id.name %in% all.variable.names) == FALSE){
    stop("Error:  id.name must be a variable name from names(dat).")
  }
  if((t1.name %in% all.variable.names) == FALSE){
    stop("Error: t1.name must be a variable name from names(dat).")
  }
  if((t2.name %in% all.variable.names) == FALSE){
    stop("Error: t2.name must be a variable name from names(dat).")
  }

  # require(data.table)
  format.dt <- is.data.table(x = dat)

  setDT(x = dat)

  setorderv(x = dat, cols = c(id.name, t1.name), order = 1)

  dat[, eval(gap.name) := identify.panel.gaps.one.patient(patient.dat = .SD, t1.name = t1.name, t2.name = t2.name, first.value = first.value, expected.gap.between = expected.gap.between), by = id.name]

  if(format.dt == FALSE){
    setDF(x = dat)
  }


  return(dat[])
}


#' panel.overlaps
#'
#' @description Identifies records within a panel data set that have overlapping observation
#' periods for the same subject. Overlaps can indicate data entry errors or issues with
#' data collection protocols.
#'
#' @param dat A data frame structured as panel data.
#' @param id.name The character name of the identifying variable within dat, used to
#' track subjects across multiple rows.
#' @param t1.name The character name of the time variable within dat representing the
#' start (left endpoint) of observation intervals.
#' @param t2.name The character name of the time variable within dat representing the
#' end (right endpoint) of observation intervals.
#' @import data.table
#' @export
#'
#' @return Returns a data table listing subjects (identified by id.name) who have at least
#' one instance of overlapping observation periods. Each row corresponds to a unique subject
#' identified as having overlaps, with additional information or indicators related to the
#' nature or extent of these overlaps. This output is crucial for data cleaning and ensuring
#' the temporal accuracy of the panel data, allowing researchers to identify and rectify
#' anomalies before conducting further analysis.


panel.overlaps <- function(dat, id.name = "id", t1.name = "t1", t2.name = "t2"){
  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }

  all.variable.names <- unique(names(dat))

  if((id.name %in% all.variable.names) == FALSE){
    stop("Error:  id.name must be a variable name from names(dat).")
  }
  if((t1.name %in% all.variable.names) == FALSE){
    stop("Error: t1.name must be a variable name from names(dat).")
  }
  if((t2.name %in% all.variable.names) == FALSE){
    stop("Error: t2.name must be a variable name from names(dat).")
  }

  # require(data.table)
  format.dt <- is.data.table(x = dat)

  if(format.dt == FALSE){
    setDT(x = dat)
  }

  setorderv(x = dat, cols = c(id.name, t1.name), order = 1)

  dat[, eval("record.index") := 1:.N, by = id.name]

  ids.with.overlaps <- dat[, id.panel.overlaps.one.patient(patient.dat = .SD, id.name = id.name, t1.name = t1.name, t2.name = t2.name, row.index.name = "record.index"), by = id.name]

  setnames(x = ids.with.overlaps, old = c("V1"), new = c("overlapping_panels"))

  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = ids.with.overlaps)
  }

  return(ids.with.overlaps)
}



#' structure.panel
#'
#' @description Sorts the panel data by subject identifier (id) and the beginning of each
#' observation period (time), ensuring the data is organized sequentially for each subject.
#' This is a crucial step in preparing panel data for time-series or longitudinal analysis,
#' where the order of records affects the analysis outcome.
#'
#' @param dat A data frame structured as panel data.
#' @param id.name The character name of the identifying variable within dat, used to
#' track subjects across multiple rows of data.
#' @param t1.name The character name of the time variable within dat representing the
#' start (left endpoint) of observation intervals.
#' @import data.table
#' @export
#'
#' @return Returns the input data frame sorted by the specified id.name and t1.name,
#' ensuring that the data for each subject is in chronological order based on the start
#' time of each observation period. This structured panel data is essential for any
#' subsequent analyses that depend on the temporal sequence of observations, such as
#' time-to-event analysis, longitudinal modeling, or any study of changes over time within
#' subjects.


structure.panel <- function(dat, id.name = "id", t1.name = "t1"){
  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }

  all.variable.names <- unique(names(dat))

  if((id.name %in% all.variable.names) == FALSE){
    stop("Error:  id.name must be a variable name from names(dat).")
  }
  if((t1.name %in% all.variable.names) == FALSE){
    stop("Error: t1.name must be a variable name from names(dat).")
  }

  # require(data.table)
  format.dt <- is.data.table(x = dat)

  if(format.dt == FALSE){
    setDT(x = dat)
  }

  res <- setorderv(x = dat, cols = c(id.name, t1.name))

  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = res)
  }

  return(res)
}



#' summarize.panel
#'
#' @description Provides summary statistics for panel data, including the total number of
#' records, the count of unique subjects, average records per subject, total and maximum
#' follow-up time. This summary is essential for understanding the dataset's structure
#' and the extent of data available for each subject.
#'
#' @param dat A data frame structured as panel data.
#' @param id.name The character name of the identifying variable within dat, used to
#' track subjects across multiple rows of data.
#' @param t1.name The character name of the time variable within dat representing the
#' start (left endpoint) of observation intervals.
#' @param t2.name The character name of the time variable within dat representing the
#' end (right endpoint) of observation intervals.
#' @param grouping.variables A character vector of variable names from dat for grouping
#' the resulting summary statistics. If NULL, summary statistics are computed for the
#' entire dataset.
#' @import data.table
#' @import DTwrappers
#' @export
#'
#' @return Returns a data.table with summary statistics for the panel data, grouped by the
#' specified grouping variables (if provided). The summary includes the total number of
#' records (total.records), the number of unique subjects (unique.ids), average number of
#' records per subject (mean.records.per.id), total follow-up time (total.followup), and
#' the maximum follow-up time (max.followup) for each group. This summary provides a
#' comprehensive overview of the data's coverage and depth, aiding in its interpretation
#' and the planning of subsequent analyses.


summarize.panel <- function(dat, id.name, t1.name, t2.name, grouping.variables = NULL){
  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }

  all.variable.names <- names(dat)
  if(!is.null(id.name)){
    if((id.name %in% all.variable.names) == FALSE){
      stop("Error:  id.name must be a variable name from names(dat) that identifies subjects across the rows.")
    }
  }
  if((t1.name %in% all.variable.names) == FALSE){
    stop("Error: t1.name must be a variable name from names(dat) that represents the end of a time interval.")
  }
  if((t2.name %in% all.variable.names) == FALSE){
    stop("Error: t2.name must be a variable name from names(dat) that represents the end of a time interval.")
  }

  # require(data.table)
  # require(DTwrappers)
  format.dt <- is.data.table(x = dat)

  dat= as.data.table(dat)

  res <- dat[, .(total.records = .N, unique.ids = length(unique(get(id.name))), mean.records.per.id = .N / length(unique(get(id.name))), total.followup = sum(get(t2.name) - get(t1.name)), max.followup = max(get(t2.name))), keyby = grouping.variables]

  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = res)
  }
  return(res)
}


#' unusual.duration
#'
#' @description Identifies records within panel data where specified events occur for an unusually
#' long duration, exceeding a predefined maximum length. This is critical for detecting outliers
#' or anomalous data points in longitudinal studies.
#'
#' @param dat A data frame structured as panel data.
#' @param outcome.name The character name of a binary variable within dat, representing the
#' event of interest. Only records where this event is true (1) are considered for analysis.
#' @param max.length A numeric value specifying the maximum allowed duration for the event.
#' Records where the event duration exceeds this threshold are identified as unusual.
#' @param t1.name The character name of the time variable within dat representing the start
#' (left endpoint) of observation intervals.
#' @param t2.name The character name of the time variable within dat representing the end
#' (right endpoint) of observation intervals.
#' @import data.table
#' @export
#'
#' @return Returns a subset of the original data frame containing only those records where
#' the specified event occurs for a duration longer than the max.length parameter. Each row
#' in this subset corresponds to an event considered unusually long, allowing for easy
#' identification and further examination of these cases. This filtered dataset is instrumental
#' in quality control and ensuring the accuracy and reliability of longitudinal data analyses.


unusual.duration <- function(dat, outcome.name, max.length, t1.name = "t1", t2.name = "t2"){

  if(is.data.frame(x = dat) == FALSE){
    stop("Error:  dat must be a data.frame object.")
  }

  all.variable.names <- unique(names(dat))

  if((t1.name %in% all.variable.names) == FALSE){
    stop("Error: t1.name must be a variable name from names(dat).")
  }
  if((t2.name %in% all.variable.names) == FALSE){
    stop("Error: t2.name must be a variable name from names(dat).")
  }
  if(outcome.name %in% all.variable.names == FALSE){
    stop("Error:  outcome.name must be a variable name from names(dat).")
  }

  # require(data.table)
  format.dt <- is.data.table(x = dat)

  if(format.dt == FALSE){
    setDT(x = dat)
  }

  res <- dat[get(outcome.name) == TRUE & get(t2.name) - get(t1.name) > max.length,]

  if(format.dt == FALSE){
    setDF(x = dat)
    setDF(x = res)
  }

  return(res[])
}
