library(testthat)
library(data.table)

file_path <- system.file("extdata", "simulated_data.csv", package="tvtools")
simulated.chd <- fread(input = file_path)

orig.data <- copy(simulated.chd)

test_that("structure.panel correctly sorts data", {

  structured_data <- structure.panel(dat = simulated.chd, id.name = "id", t1.name = "t1")

  # Check that the data is sorted by id and then by t1
  expect_true(is.data.table(structured_data)) # Check it's still a data.table
  expect_equal(structured_data$id, sort(simulated.chd$id)) # Assuming id sorting is relevant

  # Optionally, check the first few rows match expected output
  expect_equal(structured_data[1:3, .(id, t1, t2, age, sex, region)],
               data.table(id = c("01KTl0KSK88EFV8N", "01KTl0KSK88EFV8N", "01KTl0KSK88EFV8N"),
                          t1 = c(0, 8, 30),
                          t2 = c(8, 30, 38),
                          age = c(69, 69, 69),
                          sex = c("Male", "Male", "Male"),
                          region = c("West", "West", "West")))
})

test_that("summarize.panel computes summary statistics correctly", {

  summary_result <- summarize.panel(dat = simulated.chd, id.name = "id", t1.name = "t1", t2.name = "t2")

  # Check that the output is a data.table
  expect_true(is.data.table(summary_result))

  # Check the number of rows and columns in the output
  expect_equal(nrow(summary_result), 1)
  expect_equal(ncol(summary_result), 5)

  # Check the values of summary statistics
  expect_equal(summary_result$total.records, 33572)
  expect_equal(summary_result$unique.ids, 1000)
  expect_equal(summary_result$mean.records.per.id, 33.572)
  expect_equal(summary_result$total.followup, 722772)
  expect_equal(summary_result$max.followup, 2606)
})


test_that("followup.time computes total follow-up time correctly", {

  followup_result <- followup.time(dat = simulated.chd[id == id[1], ], id.name = "id", t1.name = "t1", t2.name = "t2", calculate.as = "total")

  # Check that the output is a data.table
  expect_true(is.data.table(followup_result))

  # Check the number of rows and columns in the output
  expect_equal(nrow(followup_result), 1)
  expect_equal(ncol(followup_result), 2)

  # Check the follow-up time value
  expect_equal(followup_result$followup.time, 1075)
})

test_that("first.event returns the first event correctly", {

  first_event_result <- first.event(dat = simulated.chd, id.name = "id", outcome.names = c("hospital", "death"), t1.name = "t1")

  # Check that the output is a data.table
  expect_true(is.data.table(first_event_result))

  # Check the number of rows and columns in the output
  expect_equal(nrow(first_event_result), 1000)
  expect_equal(ncol(first_event_result), 3)

  # Check some specific values from the output
  expect_equal(first_event_result[2, hospital.first.event], 43)
  expect_equal(first_event_result[2, death.first.event], 70)
  expect_equal(first_event_result[4, hospital.first.event], 100)
  expect_equal(first_event_result[997, hospital.first.event], 0)
  expect_equal(first_event_result[999, hospital.first.event], 255)

})


test_that("last.event returns the last event correctly", {

  last_event_result <- last.event(dat = simulated.chd, id.name = "id", outcome.names = c("hospital", "death"), t1.name = "t1")

  # Check that the output is a data.table
  expect_true(is.data.table(last_event_result))

  # Check the number of rows and columns in the output
  expect_equal(nrow(last_event_result), 1000)
  expect_equal(ncol(last_event_result), 3)

  # Check some specific values from the output
  expect_equal(last_event_result[2, hospital.last.event], 70)
  expect_equal(last_event_result[2, death.last.event], 70)
  expect_equal(last_event_result[4, hospital.last.event], 2039)

})

test_that("cross.sectional.data generates correct cross-sectional data", {

  cs_result <- cross.sectional.data(dat = simulated.chd, time.point = 365, id.name = "id",
                                    t1.name = "t1", t2.name = "t2", outcome.names = c("hospital", "death"),
                                    relative.followup = TRUE)

  # Check that the output is a data.table
  expect_true(is.data.table(cs_result))

  # Check the number of rows and columns in the output
  expect_equal(nrow(cs_result), 560)
  expect_equal(ncol(cs_result), 13)

  # Check some specific values from the output
  expect_equal(cs_result[2, hospital.first.event], 196)
  expect_equal(cs_result[1, followup.time], 710)
  expect_equal(cs_result[2, followup.time], 2024)
  expect_equal(cs_result[1, cross.sectional.time], 365)
  expect_equal(cs_result[2, cross.sectional.time], 365)
})

test_that("create.baseline generates correct baseline data", {

  cs_result <- create.baseline(dat = simulated.chd, id.name = "id", t1.name = "t1",
                               t2.name = "t2", outcome.names = NULL)

  # Check that the output is a data.table
  expect_true(is.data.table(cs_result))

  # Check the number of rows and columns in the output
  expect_equal(nrow(cs_result), 1000)
  expect_equal(ncol(cs_result), 12)

  # Check some specific values from the output
  expect_equal(cs_result[1, hospital], 0)
  expect_equal(cs_result[1, death], 0)
  expect_equal(cs_result[1, cross.sectional.time], 0)
  expect_equal(cs_result[2, hospital], 0)
  expect_equal(cs_result[2, death], 0)
  expect_equal(cs_result[2, cross.sectional.time], 0)
})
