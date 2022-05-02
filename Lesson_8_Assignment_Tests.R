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

rtboxkey <- ggplot(movies, aes(y = runtime)) +
  geom_boxplot()

votescatterkey <- ggplot(movies, aes(x = vote_count, y = vote_average)) +
  geom_point()

agescatterkey <- ggplot(movies, aes(x = release_date, y = vote_average)) +
  geom_point()

agescatterbkey <- movies %>% 
  filter(release_date < "2017-01-01") %>% 
  ggplot(aes(x = release_date, y = vote_average)) +
  geom_point()

histagekey <- movies %>% 
  filter(release_date < "2017-01-01") %>% 
  ggplot(aes(x = release_date)) +
  geom_histogram(bins = 49)

topmovieplotkey <- ggplot(topmovie, aes(x = release_year, y = toprev)) +
  geom_line()



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


test_that("Q7 plot (visible)", {
  
  expect_equal(rtbox$layers[[1]], rtboxkey$layers[[1]])
  expect_equal(rtbox$scales, rtboxkey$scales)
  expect_equal(rtbox$mapping, rtboxkey$mapping)
  expect_equal(rtbox$labels, rtboxkey$labels)
  
})

test_that("Q8 plot (visible)", {
  
  expect_equal(votescatter$layers[[1]], votescatterkey$layers[[1]])
  expect_equal(votescatter$scales, votescatterkey$scales)
  expect_equal(votescatter$mapping, votescatterkey$mapping)
  expect_equal(votescatter$labels, votescatterkey$labels)
  
})

test_that("Q9 plot (visible)", {
  
  expect_equal(agescatter$layers[[1]], agescatterkey$layers[[1]])
  expect_equal(agescatter$scales, agescatterkey$scales)
  expect_equal(agescatter$mapping, agescatterkey$mapping)
  expect_equal(agescatter$labels, agescatterkey$labels)
  
})

test_that("Q10 plot (visible)", {
  
  expect_equal(agescatterb$layers[[1]], agescatterbkey$layers[[1]])
  expect_equal(agescatterb$scales, agescatterbkey$scales)
  expect_equal(agescatterb$mapping, agescatterbkey$mapping)
  expect_equal(agescatterb$labels, agescatterbkey$labels)
  
})

test_that("Q11 plot (visible)", {
  
  expect_equal(histage$layers[[1]], histagekey$layers[[1]])
  expect_equal(histage$scales, histagekey$scales)
  expect_equal(histage$mapping, histagekey$mapping)
  expect_equal(histage$labels, histagekey$labels)
  
})

test_that("Q12 (visible)", {
  
  expect_true(dim(movies)[1] == 4803)
  expect_true(dim(movies)[2] == 18)
  expect_true(class(movies$release_year) == "numeric")
  expect_equal(movies$release_year[3], 2015, tolerance = 1e-2)
  expect_equal(movies$release_year[8], 2015, tolerance = 1e-2)
  expect_equal(movies$release_month[4], "07")
  
})

test_that("Q13 (visible)", {
  
  expect_true(dim(topmovie)[1] == 17)
  expect_equal(topmovie$toprev[3], 926475550, tolerance = 1e-2)
  expect_equal(topmovie$toprev[4], 1118888979, tolerance = 1e-2)
  expect_equal(topmovie$toprev[7], 1065659812, tolerance = 1e-2)

})

test_that("Q14 plot (visible)", {
  
  expect_equal(topmovieplot$layers[[1]], topmovieplotkey$layers[[1]])
  expect_equal(topmovieplot$scales, topmovieplotkey$scales)
  expect_equal(topmovieplot$mapping, topmovieplotkey$mapping)
  expect_equal(topmovieplot$labels, topmovieplotkey$labels)
  
})

test_that("Q15 (visible)", {
  
  expect_true(dim(revbymonth)[1] == 12)
  expect_equal(revbymonth$meanrev[3], 80728040, tolerance = 1e-2)
  expect_equal(revbymonth$meanrev[11], 132190472, tolerance = 1e-2)
  expect_equal(revbymonth$mdnrev[5], 21334266, tolerance = 1e-2)
  expect_equal(revbymonth$mdnrev[12], 43545364, tolerance = 1e-2)
  
})

test_that("Q16 (visible)", {
  
  expect_true(dim(percrevbyyear)[1] == 17)
  expect_equal(percrevbyyear$totrev[3], 14609857556, tolerance = 1e-2)
  expect_equal(percrevbyyear$totrev[11], 20348574768, tolerance = 1e-2)
  expect_equal(percrevbyyear$toprev[15], 1091405097, tolerance = 1e-2)
  expect_equal(percrevbyyear$toprev[17], 1153304495, tolerance = 1e-2)
  expect_equal(percrevbyyear$percrevtop[11], 0.05243462, tolerance = 1e-4)
  expect_equal(percrevbyyear$percrevtop[16], 0.06645564, tolerance = 1e-4)
  
})

test_that("Q17 (visible)", {
  
  expect_equal(murica, 35, tolerance = 1e-1)
  
})

test_that("Q18 (visible)", {
  
  expect_equal(titlech, 261, tolerance = 1e-1)
  
})

test_that("Q19 Part 1 (visible)", {
  
  expect_true(dim(movies2)[1] == 4802)
  expect_equal(movies2$underperformed[3], FALSE)
  expect_equal(movies2$underperformed[434], TRUE)
  expect_equal(movies2$underperformed[1241], TRUE)
  expect_equal(movies2$underperformed[1628], TRUE)
  expect_equal(movies2$underperformed[4639], TRUE)
  
})

test_that("Q19 Part 2 (visible)", {
  
  expect_equal(movies2$underperformed[454], TRUE)
  expect_equal(movies2$underperformed[2639], TRUE)
  expect_equal(movies2$underperformed[3367], TRUE)
  expect_equal(movies2$underperformed[4700], TRUE)

})

test_that("Q20 (visible)", {
  
  expect_equal(numunder, 156, tolerance = 1e-1)
  
})


test_that("Q21 Extra Credit 1a (visible)", {
  
  expect_equal(movies2$underperfmonth[736], TRUE)
  expect_equal(movies2$underperfmonth[747], TRUE)
  expect_equal(movies2$underperfmonth[863], TRUE)
  expect_equal(movies2$underperfmonth[954], TRUE)
  
})

test_that("Q21 Extra Credit 1b (visible)", {
  
  expect_equal(movies2$underperfmonth[980], TRUE)
  expect_equal(movies2$underperfmonth[1029], TRUE)
  expect_equal(movies2$underperfmonth[1112], TRUE)
  expect_equal(movies2$underperfmonth[1398], TRUE)
  
})

test_that("Q21 Extra Credit 1c (visible)", {
  
  expect_equal(movies2$underperfmonth[1609], TRUE)
  expect_equal(movies2$underperfmonth[1643], TRUE)
  expect_equal(movies2$underperfmonth[2213], TRUE)
  expect_equal(movies2$underperfmonth[2308], TRUE)
  
})

test_that("Q21 Extra Credit 1d (visible)", {
  
  expect_equal(movies2$underperfmonth[2628], TRUE)
  expect_equal(movies2$underperfmonth[2782], TRUE)
  expect_equal(movies2$underperfmonth[4078], TRUE)
  expect_equal(movies2$underperfmonth[4758], TRUE)
  
})

test_that("Q22 Extra Credit 2a (visible)", {
  
  expect_true(grepl("Action", movies2$genres[1]) == TRUE)
  expect_true(grepl("Adventure", movies2$genres[1]) == TRUE)
  expect_true(grepl("Fantasy", movies2$genres[1]) == TRUE)
  expect_true(grepl("Science Fiction", movies2$genres[1]) == TRUE)
  expect_true(grepl("}", movies2$genres[1]) == FALSE)
  
})

test_that("Q22 Extra Credit 2b (visible)", {
  
  expect_true(grepl("Animation", movies2$genres[7]) == TRUE)
  expect_true(grepl("Family", movies2$genres[7]) == TRUE)
  expect_true(grepl("}", movies2$genres[1]) == FALSE)
  
})

test_that("Q22 Extra Credit 2c (visible)", {
  
  expect_true(grepl("Action", movies2$genres[13]) == TRUE)
  expect_true(grepl("Adventure", movies2$genres[13]) == TRUE)
  expect_true(grepl("Fantasy", movies2$genres[13]) == TRUE)
  expect_true(grepl("}", movies2$genres[13]) == FALSE)
  
})

test_that("Q22 Extra Credit 2d (visible)", {
  
  expect_true(grepl("Action", movies2$genres[14]) == TRUE)
  expect_true(grepl("Adventure", movies2$genres[14]) == TRUE)
  expect_true(grepl("Western", movies2$genres[14]) == TRUE)
  expect_true(grepl("28", movies2$genres[14]) == FALSE)
  expect_true(grepl("}", movies2$genres[14]) == FALSE)
  
})


