library(testthat)

# each call to test_that() produces one test
# each test represents one point value
# you can have multiple tests for each question

library(dplyr)
library(ggplot2)
mvs <- read.csv("tmdb_5000_movies.csv")
movies <- mvs[, c("title","id", "status","release_date","budget","revenue", "runtime",
                  "overview", "tagline","genres", "original_language", "original_title", 
                  "popularity", "vote_average", "vote_count", "homepage")]
movies$release_date <- as.Date(movies$release_date, format = "%m/%d/%Y")
movies <- movies %>% 
  mutate(release_year = format(release_date, "%Y"),
         release_month = format(release_date, "%m"))
movies$release_year <- as.numeric(movies$release_year)

budghistkey <- ggplot(movies, aes(x = budget)) +
  geom_histogram()

budgboxkey <- ggplot(movies, aes(y = budget)) +
  geom_boxplot()


test_that("Q2 (visible)", {
  
  expect_true(dim(movies)[1] == 4803)
  expect_true(dim(movies)[2] > 15)
  expect_true(names(movies)[1] == "title")
  expect_true(names(movies)[4] == "release_date")
  expect_true(names(movies)[8] == "overview")

})

test_that("Q3 (visible)", {
  
  expect_true(class(movies$release_date) == "Date")
  expect_equal(movies$release_date[3], as.Date("2015-10-26"))
  expect_equal(movies$release_date[7], as.Date("2010-11-24"))
  expect_equal(movies$release_date[9], as.Date("2009-07-07"))

})

test_that("Q4 plots (visible)", {
  
  expect_equal(budghist$layers[[1]], budghistkey$layers[[1]])
  expect_equal(budghist$scales, budghistkey$scales)
  expect_equal(budghist$mapping, budghistkey$mapping)
  expect_equal(budghist$labels, budghistkey$labels)
  expect_equal(budgbox$layers[[1]], budgboxkey$layers[[1]])
  expect_equal(budgbox$scales, budgboxkey$scales)
  expect_equal(budgbox$mapping, budgboxkey$mapping)
  expect_equal(budgbox$labels, budgboxkey$labels)
  
})

test_that("Q5 (visible)", {
  
  expect_true(dim(highbudg)[1] == 1)
  expect_equal(highbudg$budget, 380000000, tolerance = 1)
  
})

test_that("Q6 (visible)", {
  
  expect_equal(highbudgprofit, 665713802, tolerance = 1)
  
})




test_that("Q4 (visible)", {
  
  expect_equal(sum(highimp$GOOGLE_PROSPECTING_IMPRESSIONS), 3187, tolerance = 1)
  expect_equal(sum(highimp$GOOGLE_RETARGETING_IMPRESSIONS), 4890, tolerance = 1)
  expect_equal(sum(highimp$FACEBOOK_PROSPECTING_IMPRESSIONS), 5826, tolerance = 1)

})

test_that("Q5 (visible)", {
  
  expect_true(names(fexp2)[2] == "DMA_NAME")
  expect_true(names(fexp2)[4] == "GP")
  expect_equal(sum(fexp2$GP), 20517.62, tolerance = 1)
  expect_equal(sum(fexp2$FR), 21337.17, tolerance = 1)
  expect_equal(sum(fexp2$WebSales), 1709670, tolerance = 1)
  expect_equal(dim(fexp2)[2], 11)
              
})
  
test_that("Q6 (visible)", {
  
  expect_true(names(fexp4)[12] == "GPTot")
  expect_true(names(fexp4)[13] == "FPTot")
  expect_equal(sum(fexp4$GPTot), 143623.3, tolerance = 1)
  expect_equal(sum(fexp4$FPTot), 268732.6, tolerance = 1)
  expect_equal(sum(fexp4$WebSales), 1709670, tolerance = 1)

})

test_that("Q7 (visible)", {
  
  expect_true(names(fexp4)[14] == "GPPercTot")
  expect_true(names(fexp4)[15] == "FPPercTot")
  expect_equal(sum(fexp4$GPPercTot), 630, tolerance = 1)
  expect_equal(sum(!is.na(fexp4$FPPercTot)), 4046, tolerance = 1)
  expect_equal(sum(fexp4$WebSales), 1709670, tolerance = 1)
  
})

test_that("Q8 data (visible)", {
  
  expect_equal(sum(gppercplot$data$GP), 3151.51, tolerance = 1)
  expect_equal(sum(gppercplot$data$GPPercTot), 9, tolerance = 1e-1)

})

test_that("Q8 plot (visible)", {
  
  expect_equal(gppercplot$layers[[1]], gppercplotkey$layers[[1]])
  expect_equal(gppercplot$scales, gppercplotkey$scales)
  expect_equal(gppercplot$mapping, gppercplotkey$mapping)
  expect_equal(gppercplot$labels, gppercplotkey$labels)
  
})

test_that("Q10 data (visible)", {
  
  expect_equal(sum(fppercplot$data$FP), 5644.59, tolerance = 1)
  expect_equal(sum(fppercplot$data$FPPercTot), 9, tolerance = 1e-1)
  
})

test_that("Q10 plot (visible)", {
  
  expect_equal(fppercplot$layers[[1]], fppercplotkey$layers[[1]])
  expect_equal(fppercplot$scales, fppercplotkey$scales)
  expect_equal(fppercplot$mapping, fppercplotkey$mapping)
  expect_equal(fppercplot$labels, fppercplotkey$labels)
  
})

test_that("Q11 summarize (visible)", {
  
  expect_equal(dim(gppercbyweek)[1], 60, tolerance = 1e-3) 
  expect_equal(sum(gppercbyweek$GPWeekly), 20517.62, tolerance = 1) 
  
})

test_that("Q11 mutate (visible)", {
  
  expect_equal(dim(gppercbyweek)[2], 5, tolerance = 1e-3) 
  expect_equal(sum(gppercbyweek$GPTot), 61552.86, tolerance = 1) 
  expect_equal(sum(gppercbyweek$GPPerc), 20, tolerance = 1e-2) 

})

test_that("Q12 (visible)", {
  
  expect_equal(dim(gppercclean)[2], 3, tolerance = 1e-3) 
  expect_equal(sum(gppercclean$GPPerc), 20, tolerance = 1e-2) 

})

test_that("Q13 (visible)", {
  
  expect_equal(dim(allchanperc)[2], 7, tolerance = 1e-3) 
  expect_equal(sum(allchanperc$GPPerc), 20, tolerance = 1e-2) 
  expect_equal(sum(allchanperc$GBPerc), 20, tolerance = 1e-2) 
  expect_equal(sum(allchanperc$FRPerc), 20, tolerance = 1e-2) 
  expect_equal(allchanperc$FPPerc[2], .3932765, tolerance = 1e-3)
  
})

test_that("Q15 Dimensions (visible)", {
  
  expect_equal(dim(wkchange)[1],20, tolerance = 1e-3) 
  expect_equal(dim(wkchange)[2], 6, tolerance = 1e-3) 

})

test_that("Q15 Values (visible)", {
  
  expect_equal(sum(wkchange$GPChange), 2.424282, tolerance = 1e-3) 
  expect_equal(sum(wkchange$GBChange), -.7228266, tolerance = 1e-3) 
  expect_equal(sum(wkchange$FRChange), 1.384375, tolerance = 1e-3) 
  expect_equal(wkchange$FPChange[2], .445777, tolerance = 1e-3)
  
})

test_that("Q16 (visible)", {
  
  expect_equal(ldgr, 16, tolerance = 1e-3) 
  
})

test_that("Q17 (visible)", {
  
  expect_equal(ldfp, 10, tolerance = 1e-3) 
  
})
