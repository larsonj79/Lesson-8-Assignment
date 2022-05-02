library(testthat)

# each call to test_that() produces one test
# each test represents one point value
# you can have multiple tests for each question

library(readxl)
library(dplyr)
library(ggplot2)
fexp <- read_excel("FieldExperiment.xlsx")
fexp <- data.frame(fexp)
fexp$DATE <- as.Date(fexp$DATE)
fexp$WEEK <- factor(fexp$WEEK)

fexp2 <- fexp %>% 
  select(DATE,
         DMA_NAME,
         DMA_CONDITION,
         GP = GOOGLE_PROSPECTING_SPEND,
         GR = GOOGLE_RETARGETING_SPEND,
         GB = GOOGLE_BRAND_SPEND,
         FP = FACEBOOK_PROSPECTING_SPEND,
         FR = FACEBOOK_RETARGETING_SPEND,
         WebSales = SHOPIFY_US_SALES,
         POPN,
         WEEK)

fexp4 <- fexp2 %>% 
  group_by(DMA_NAME, WEEK) %>% 
  mutate(GPTot = sum(GP),
         FPTot = sum(FP))

fexp4 <- fexp4 %>% 
  mutate(GPPercTot = GP / GPTot,
         FPPercTot = FP / FPTot)

gppercplotkey <- fexp4 %>% 
  filter(DMA_NAME %in% c("New York, NY", "Los Angeles, CA", "Chicago, IL")) %>% 
  ggplot(aes(x = DATE, y = GPPercTot, color = DMA_NAME)) +
  geom_line()

fppercplotkey <- fexp4 %>% 
  filter(DMA_NAME %in% c("New York, NY", "Los Angeles, CA", "Chicago, IL")) %>% 
  ggplot(aes(x = DATE, y = FPPercTot, color = DMA_NAME)) +
  geom_line()


test_that("Q2 (visible)", {
  
  expect_true(dim(fexp3)[1] == 4410)
  expect_true(dim(fexp3)[2] == 8)
  expect_true(names(fexp3)[1] == "DATE")
  expect_true(names(fexp3)[4] == "GOOGLE_RETARGETING_IMPRESSIONS")
  expect_true(names(fexp3)[8] == "FACEBOOK_OTHER_IMPRESSIONS")

})

test_that("Q3 (visible)", {
  
  expect_equal(maximps$maxGP, 13701, tolerance = 1)
  expect_equal(maximps$maxGB, 921, tolerance = 1)
  expect_equal(maximps$maxFP, 28955, tolerance = 1)

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
