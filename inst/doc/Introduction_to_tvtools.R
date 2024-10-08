## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  tidy = TRUE
)

## ----include=FALSE------------------------------------------------------------
devtools::load_all(".")

## ----setup--------------------------------------------------------------------
library(tvtools)
library(data.table)
library(DTwrappers)

file_path <- system.file("extdata", "simulated_data.csv", package="tvtools")
simulated.chd <- fread(input = file_path)

orig.data <- copy(simulated.chd)


## -----------------------------------------------------------------------------
dim(simulated.chd)

## -----------------------------------------------------------------------------
simulated.chd[1:10,]

## -----------------------------------------------------------------------------
simulated.chd[58:70,]

## -----------------------------------------------------------------------------
simulated.chd[1:3,]

## -----------------------------------------------------------------------------
simulated.chd <- structure.panel(dat = simulated.chd, id.name = "id", t1.name = "t1")

simulated.chd[1:3,]

## -----------------------------------------------------------------------------
structure.panel(dat = simulated.chd[c(2,4,3,1),])

## -----------------------------------------------------------------------------
summarize.panel(dat = simulated.chd, id.name = "id", t1.name = "t1", t2.name = "t2")

## -----------------------------------------------------------------------------
summarize.panel(dat = simulated.chd, id.name = "id", t1.name = "t1", t2.name = "t2", grouping.variables = "sex")

summarize.panel(dat = simulated.chd, id.name = "id", t1.name = "t1", t2.name = "t2", grouping.variables = c("sex", "region"))

## -----------------------------------------------------------------------------
followup.time(dat = simulated.chd, id.name = "id", t2.name = "t2", calculate.as = "max")

## -----------------------------------------------------------------------------
followup.time(dat = simulated.chd, id.name = "id", t1.name = "t1", t2.name = "t2", calculate.as = "total")

## -----------------------------------------------------------------------------
followup.time(dat = simulated.chd[id == id[1],], id.name = "id", t1.name = "t1", t2.name = "t2", calculate.as = "total")
followup.time(dat = simulated.chd[id == id[1],][5:20,], id.name = "id", t1.name = "t1", t2.name = "t2", calculate.as = "total")
followup.time(dat = simulated.chd[id == id[1],][5:20,], id.name = "id", t1.name = "t1", t2.name = "t2", calculate.as = "max")

## -----------------------------------------------------------------------------
followup.time(dat = simulated.chd, id.name = "id", t2.name = "t2", calculate.as = "max", append.to.data = T, followup.name = "followup.time")
print(simulated.chd[1:5,])

## -----------------------------------------------------------------------------
first.event(dat = simulated.chd, id.name = "id", outcome.names = c("ace", "bb", "statin"), t1.name = "t1")

## -----------------------------------------------------------------------------
first.event(dat = simulated.chd, id.name = "id", outcome.names = c("hospital", "death"), t1.name = "t1")

## -----------------------------------------------------------------------------
##first.event(dat = simulated.chd, id.name = NULL, outcome.names = c("hospital", "death"), t1.name = "t1")

## -----------------------------------------------------------------------------
one.patient <- first.event(dat = simulated.chd[id == "01ZbYuUoYJeIyiVH",], id.name = "id", outcome.names = c("hospital", "death"), t1.name = "t1", append.to.table = TRUE, event.name = "time")
setorderv(x = one.patient, cols = c("id", "t1"))
print(one.patient)

## -----------------------------------------------------------------------------
last.event(dat = simulated.chd, id.name = "id", outcome.names = c("hospital", "death"), t1.name = "t1")[1:5,]

## -----------------------------------------------------------------------------
last.event(dat = simulated.chd, id.name = NULL, outcome.names = c("hospital", "death"), t1.name = "t2")[1:5,]

## -----------------------------------------------------------------------------
last.event(dat = simulated.chd, id.name = NULL, outcome.names = c("hospital", "death"), t1.name = "t1")

## -----------------------------------------------------------------------------
simulated.chd[, followup.time := NULL]

## -----------------------------------------------------------------------------
baseline <- cross.sectional.data(dat = simulated.chd, time.point = 0, id.name = "id", t1.name = "t1", t2.name = "t2", outcome.names = c("hospital", "death"))
baseline[1,]
baseline[hospital.first.event > 0,][1:2,]
baseline[death.first.event > 0,][1:2,]

## -----------------------------------------------------------------------------
baseline.2 <- create.baseline(dat = simulated.chd, id.name = "id", t1.name = "t1", t2.name = "t2", outcome.names = c("hospital", "death"))
baseline.2[hospital.first.event > 0,][1:2,]
baseline.2[death.first.event > 0,][1:2,]

## -----------------------------------------------------------------------------
cs.365 <- cross.sectional.data(dat = simulated.chd, time.point = 365, id.name = "id", t1.name = "t1", t2.name = "t2", outcome.names = c("hospital", "death"), relative.followup = FALSE)
cs.365[1:2,]

## -----------------------------------------------------------------------------
cs.365.relative <- cross.sectional.data(dat = simulated.chd, time.point = 365, id.name = "id", t1.name = "t1", t2.name = "t2", outcome.names = c("hospital", "death"), relative.followup = TRUE)
cs.365.relative[1:2,]

## -----------------------------------------------------------------------------
cs.no.outcomes <- create.baseline(dat = simulated.chd, id.name = "id", t1.name = "t1", t2.name = "t2", outcome.names = NULL)
cs.no.outcomes[1:2,]

## -----------------------------------------------------------------------------
baseline[, mean(age)]
simulated.chd[, mean(age)]

## -----------------------------------------------------------------------------
calculate.utilization(dat = simulated.chd, outcome.names = c("ace", "bb", "statin", "hospital"), begin = 0, end = 365, id.name = "id", t1.name = "t1", t2.name = "t2", type = "total", full.followup = F)

## -----------------------------------------------------------------------------
calculate.utilization(dat = simulated.chd, outcome.names = c("ace", "bb", "statin", "hospital"), begin = 0, end = 365, id.name = "id", t1.name = "t1", t2.name = "t2", type = "total", full.followup = T)

## -----------------------------------------------------------------------------
med.utilization.rates <- calculate.utilization(dat = simulated.chd, outcome.names = c("ace", "bb", "statin", "hospital"), begin = 0, end = 365, id.name = "id", t1.name = "t1", t2.name = "t2", type = "rate", full.followup = T)

med.utilization.rates

## -----------------------------------------------------------------------------
med.utilization.rates[, lapply(X = .SD, FUN = function(x){return(mean(x > 0.8))}), .SDcols = c("ace", "bb", "statin")]

## -----------------------------------------------------------------------------
count.events(dat = simulated.chd, outcome.names = c("hospital", "death"), type = "overall")

## -----------------------------------------------------------------------------
count.events(dat = simulated.chd, outcome.names = c("hospital", "death"), type = "distinct")

## -----------------------------------------------------------------------------
count.events(dat = simulated.chd, outcome.names = c("hospital", "death"), grouping.variables = "region", type = "distinct")

## -----------------------------------------------------------------------------
count.events(dat = simulated.chd, outcome.names = c("hospital", "death"), grouping.variables = "id", type = "distinct")

## -----------------------------------------------------------------------------
count.events(dat = simulated.chd, outcome.names = c("hospital", "death"), grouping.variables = c("ace", "bb"), type = "distinct")

## -----------------------------------------------------------------------------
crude.rates(dat = simulated.chd, outcome.names = c("hospital", "death"), type = "distinct")

## -----------------------------------------------------------------------------
crude.rates(dat = simulated.chd, outcome.names = c("hospital", "death"), time.multiplier = 100 * 365.25)

## -----------------------------------------------------------------------------
crude.rates(dat = simulated.chd, outcome.names = c("hospital", "death"), time.multiplier = 100 * 365.25, grouping.variables = "ace")

crude.rates(dat = simulated.chd, outcome.names = c("hospital", "death"), time.multiplier = 100 * 365.25, grouping.variables = "bb")

crude.rates(dat = simulated.chd, outcome.names = c("hospital", "death"), time.multiplier = 100 * 365.25, grouping.variables = "statin")

## -----------------------------------------------------------------------------
crude.rates(dat = simulated.chd, outcome.names = c("hospital", "death"), time.multiplier = 100 * 365.25, cut.points = c(90, 365/2))

## -----------------------------------------------------------------------------
crude.rates(dat = simulated.chd, outcome.names = c("hospital", "death"), time.multiplier = 100 * 365.25, grouping.variables = "bb", cut.points = c(90, 365/2))

## -----------------------------------------------------------------------------
simulated.chd[1:2, .SD, .SDcols = c("id", "t1", "t2")]

## -----------------------------------------------------------------------------
era.splits(dat = simulated.chd[1:2, .SD, .SDcols = c("id", "t1", "t2")], cut.points = c(3,5))

## -----------------------------------------------------------------------------
measurement.rate(dat = simulated.chd, id.name = "id", t1.name = "t1", t2.name = "t2", time.point = 365)

## -----------------------------------------------------------------------------
measurement.rate(dat = simulated.chd, id.name = "id", t1.name = "t1", t2.name = "t2", time.point = 365, grouping.variables = "region")

## -----------------------------------------------------------------------------
pg.check = panel.gaps(dat = orig.data, id.name = "id", t1.name = "t1", t2.name = "t2")
pg.check[, .N, gap_before]

## -----------------------------------------------------------------------------
gap.dat <- simulated.chd[c(1,3,5, 7, 58, 60, 64),]
pg.check.2 <- panel.gaps(dat = gap.dat, id.name = "id", t1.name = "t1", t2.name = "t2")
pg.check.2[, .SD, .SDcols = c("id", "t1", "t2", "gap_before")]

## -----------------------------------------------------------------------------
first.panel.gap(dat = gap.dat, id.name = "id", t1.name = "t1", t2.name = "t2")

## -----------------------------------------------------------------------------
last.panel.gap(dat = gap.dat, id.name = "id", t1.name = "t1", t2.name = "t2")

## -----------------------------------------------------------------------------
possible.overlaps <- panel.overlaps(dat = simulated.chd, id.name = "id", t1.name = "t1", t2.name = "t2")
#print(possible.overlaps)
possible.overlaps[, mean(overlapping_panels == F)]

## ----tidy = F-----------------------------------------------------------------
overlap.dat <- data.table(id = "ABC", t1 = c(0, 7, 14, 21), t2 = c(8, 15, 21, 30), ace = c(1,0,1,0))

## -----------------------------------------------------------------------------
panel.overlaps(dat = overlap.dat, id.name = "id", t1.name = "t1", t2.name = "t2")

## -----------------------------------------------------------------------------
long.hospitalizations <- unusual.duration(dat = simulated.chd, outcome.name = "hospital", max.length = 100, t1.name = "t1", t2.name = "t2")
long.hospitalizations[, .SD, .SDcols = c("id", "t1", "t2", "hospital")]

## -----------------------------------------------------------------------------
unusual.duration(dat = simulated.chd, outcome.name = "death", max.length = 1, t1.name = "t1", t2.name = "t2")

