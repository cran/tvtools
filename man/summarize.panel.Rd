% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tvtools.R
\name{summarize.panel}
\alias{summarize.panel}
\title{summarize.panel}
\usage{
summarize.panel(dat, id.name, t1.name, t2.name, grouping.variables = NULL)
}
\arguments{
\item{dat}{A data frame structured as panel data.}

\item{id.name}{The character name of the identifying variable within dat, used to
track subjects across multiple rows of data.}

\item{t1.name}{The character name of the time variable within dat representing the
start (left endpoint) of observation intervals.}

\item{t2.name}{The character name of the time variable within dat representing the
end (right endpoint) of observation intervals.}

\item{grouping.variables}{A character vector of variable names from dat for grouping
the resulting summary statistics. If NULL, summary statistics are computed for the
entire dataset.}
}
\value{
Returns a data.table with summary statistics for the panel data, grouped by the
specified grouping variables (if provided). The summary includes the total number of
records (total.records), the number of unique subjects (unique.ids), average number of
records per subject (mean.records.per.id), total follow-up time (total.followup), and
the maximum follow-up time (max.followup) for each group. This summary provides a
comprehensive overview of the data's coverage and depth, aiding in its interpretation
and the planning of subsequent analyses.
}
\description{
Provides summary statistics for panel data, including the total number of
records, the count of unique subjects, average records per subject, total and maximum
follow-up time. This summary is essential for understanding the dataset's structure
and the extent of data available for each subject.
}
