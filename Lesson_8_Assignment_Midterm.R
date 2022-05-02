# Lesson 8 Assignment - Midterm

# Your assignment is to write the commands instructed in the comments below. To run your
# commands, simply hit Ctrl+Enter (command+return on a MAC) when the cursor is on that 
# command line. You can also type commands directly into the Console below, but you must
# save them in this file for your assignment.

# Do not change these four lines or GradeScope will not work
library(stringr)
library(dplyr)
library(ggplot2)
mvs <- read.csv("tmdb_5000_movies.csv")


## Welcome to the Midterm

#This assignment is a shout-out to all the movie aficionados out there. We are going to be 
# cleaning and analyzing data from 5000 movies. The data is contained in mvs.

#1. Take a look at the first 6 rows of columns 1, 3, 9, 13, and 18. 
# (https://campus.datacamp.com/courses/cleaning-data-in-r/chapter-1-introduction-and-exploring-raw-data?ex=9)
mvs[1:6, c(1, 3, 9, 13, 18)]

#2. Let's start cleaning the data by creating a new data frame that is a subset of the original 
#data. Call the new data frame "movies" and include the following columns: "title","id",
# "status","release_date","budget","revenue", "runtime","overview", "tagline","genres", 
# "original_language", "original_title", "popularity", "vote_average", "vote_count", "homepage". 

# (You may complete this task with or without the dplyr package. If you do not use dplyr, 
# you can copy the variable list above directly into your code.)
movies <- mvs[, c("title","id", "status","release_date","budget","revenue", "runtime",
                  "overview", "tagline","genres", "original_language", "original_title", 
                  "popularity", "vote_average", "vote_count", "homepage")]

#3. Next, let's correct the release_date variable, which was read in as a character variable.
# Print out the first ten release dates to confirm it worked. 
# (https://campus.datacamp.com/courses/intermediate-r/chapter-5-utilities?ex=14)
movies$release_date <- as.Date(movies$release_date, format = "%m/%d/%Y")
movies$release_date[1:10]


#4. Let's continue exploring the data by examining a few distributions. Using ggplot2, 
# create both a histogram and a boxplot of the budget variable. Save the histogram as
# budghist and the boxplot as budgbox. (The bars on the histogram should be vertical,
# and the boxplot should be oriented vertically.)
budghist <- ggplot(movies, aes(x = budget)) +
  geom_histogram()

budgbox <- ggplot(movies, aes(y = budget)) +
  geom_boxplot()

#5. The distribution of movie budgets shows a high degree of right-skew. What movie had 
# the highest budget? Save the entire row of this movie to highbudg.
highbudg <- movies[movies$budget == max(movies$budget),]

#6. What was the gross profit from this movie? (Calculate gross profit as revenue minus 
# the budget. This is not the real gross profit earned by the studio, but we will overlook 
# that.) Save this amount as highbudgprofit.
highbudgprofit <- highbudg$revenue - highbudg$budget

#7. Create a boxplot of the movie run times (using ggplot2). Save it as rtbox.
rtbox <- ggplot(movies, aes(y = runtime)) +
  geom_boxplot()

#8. Using ggplot2, create a scatter plot with vote_count on the x-axis and vote_average 
# on the y-axis. Save it as votescatter.
votescatter <- ggplot(movies, aes(x = vote_count, y = vote_average)) +
  geom_point()

# It looks like there is a positive correlation between the number of votes that a movie 
# gets and its average rating. In other words, it looks like the more votes received by a 
# movie, the higher the average rating the movie receives. At this point in the course, we 
# do not yet know how to confirm this correlation but you can look forward to that in a 
# few weeks!

#9. Often people complain that movies are not as good as they used to be. Create another 
# scatter plot (using ggplot) with release_date on the x axis and vote_average on the y 
# axis. Save it as agescatter.
agescatter <- ggplot(movies, aes(x = release_date, y = vote_average)) +
  geom_point()

#10. The database includes some movies with anticipated release dates far in the future. 
# Re-do the plot above without movies released after Dec 31, 2016. Save it as agescatterb.
agescatterb <- movies %>% 
  filter(release_date < "2017-01-01") %>% 
  ggplot(aes(x = release_date, y = vote_average)) +
  geom_point()

# It DOES appear that more recent movies have an average rating that is lower than earlier
# movies, which can be interpreted to mean that the adage is true: "They don't make 'em like 
# they used to." In reality, there's a better explanation for this trend. (Hint: it has 
# something to do with the next plot.)

#11. We think the data might have a recency bias and have many more films that are newer 
# than are older. Test this theory by creating a histogram of the release_date (use ggplot). 
# Make the histogram use one bar per year (use *bins = * within the geom_histogram command).
# Call the histogram histage.
# (Hint: the oldest movie was made in 1968, the newest in 2016. Remove movies with release 
# dates after 2016.)
histage <- movies %>% 
  filter(release_date < "2017-01-01") %>% 
  ggplot(aes(x = release_date)) +
  geom_histogram(bins = 49)

#12. Next we will be creating reports grouped by year and/or month. To do this, create two 
# new variables, *release_year* and *release_month*, in the movies data frame. Format 
# release_year as a four-digit numeric variable (e.g., *2009*). Leave month as a character 
# variable, but the entries should be numbers (e.g., "01", "12"). 
# (https://campus.datacamp.com/courses/intermediate-r/chapter-5-utilities?ex=14)
movies <- movies %>% 
  mutate(release_year = format(release_date, "%Y"),
         release_month = format(release_date, "%m"))
movies$release_year <- as.numeric(movies$release_year)

#13. Using dplyr, create a report that shows the highest revenue earned by a single movie
# each year from 2000 to 2016. Call the report topmovie. The revenue variable should be
# named toprev.
topmovie <- movies %>% 
  filter(release_date < "2017-01-01",
         release_date > "1999-12-31") %>% 
  group_by(release_year) %>% 
  top_n(1, revenue) %>% 
  rename(toprev = revenue) %>% 
  arrange(release_year)

topmovie <- movies %>% 
  filter(release_date < "2017-01-01",
         release_date > "1999-12-31") %>% 
  group_by(release_year) %>% 
  summarize(toprev = max(revenue))
  

#14. Using gpplot2, create a line plot of this data. Save the plot as topmovieplot.
topmovieplot <- ggplot(topmovie, aes(x = release_year, y = toprev)) +
  geom_line()

#15. Hollywood tends to release their biggest movies in summer or in December. Create a 
# report that shows the mean and median revenues of movies by month of the year, but limit 
# the report to movies released from 2000 to 2016. Name the variables meanrev and mdnrev, 
# respectively. (The report should be by month, not by year and month, so it should only 
# have 12 rows.) Call the report revbymonth.
revbymonth <- movies %>%
  filter(release_date < "2017-01-01",
         release_date > "1999-12-31") %>% 
  group_by(release_month) %>% 
  summarize(meanrev = mean(revenue),
            mdnrev = median(revenue)) %>% 
  filter(!is.na(release_month))

# Notice that the mean is MUCH higher than the median. This is characteristic of highly 
# right-skewed data.

#16. One last report using dplyr. Create a report that shows for the movies from 2000 to 
# 2016: (1) the total movie revenue for the year (name it totrev), (2) the revenue of the 
# highest-revenue movie of that year (name it toprev), and (3) the percentage of the total 
# revenues consisting of the highest-revenue movie (name it percrevtop). Call the report 
# percrevbyyear.
percrevbyyear <- movies %>% 
  filter(release_date < "2017-01-01",
         release_date > "1999-12-31") %>% 
  group_by(release_year) %>% 
  summarize(totrev = sum(revenue),
            toprev = max(revenue),
            percrevtop = toprev / totrev)

#17. How many movies use the word "America" in its overview? Save the answer as murica.
# Note that "American" and "America's" should not count as using the word "America".
# (https://campus.datacamp.com/courses/intermediate-r/chapter-5-utilities?ex=8)
americamovie <- movies %>% 
  filter(grepl("America ", overview))

murica <- nrow(americamovie)

#18. How many movies had a title change (i.e., the title is different from the original 
# title)? Save the answer as titlech.
titlech <- sum(as.character(movies$original_title) != as.character(movies$title))

#19. Create a data frame movies2 that is a copy of the movies data frame but removes 
# the movie that has a missing release_month. The studio wants to compile a list of 
# movies that underperformed in the theaters. That is, it wants to find movies that 
# were above the mean in popularity but below the mean in revenue. Create a new logical 
# variable, *underperformed*, in the movies2 data frame that is TRUE for 
# underperforming movies and FALSE otherwise. Below is pseudo-code for this task 
# (pseudo-code provides an explanation of what the code does but is not executable). 

# Loop across the movies in the movies2 dataset.
# If the movie's release year is after 2016, return FALSE.
# Check whether the movie's popularity is above the mean popularity. If it is not, 
#   return FALSE.
# For a movie that IS above the mean popularity, check whether its revenue is below 
#   the median.
# If it is, return TRUE. If it is not, return FALSE.

# (https://campus.datacamp.com/courses/intermediate-r/chapter-2-loops?ex=7)

movies2 <- movies %>% 
  filter(!is.na(release_month))

movies2$underperformed <- rep(F, nrow(movies2))
for(i in 1:nrow(movies2)) {
  if(movies2$popularity[i] > mean(movies2$popularity)) {
    if(movies2$revenue[i] < median(movies2$revenue)) {
      movies2$underperformed[i] <- TRUE
    }
  }
}

#20. How many movies qualified as underperforming? Save the answer as numunder.
numunder <- sum(movies2$underperformed)

# *EXTRA CREDIT* *OPTIONAL*
#21. The studio wants to compile a list of movies that underperformed in the theaters, 
# as in question 19. But this time, the studio wants to control for release month, 
# meaning it doesn't want to check whether the movie is below  the overall revenue 
# mean, rather it wants to check whether the movie is below the mean revenue *of 
# movies released in the same month of the year*. Create a new logical variable, 
# *underperfmonth*, in the movies data frame that is TRUE for underperforming movies
# and FALSE otherwise. Below is pseudo-code for this task.

# Loop across the movies in the movies2 dataset.
# If the movie's release year is after 2016, return FALSE.
# Check whether the movie's popularity is above the mean popularity. If it is not, 
#   return FALSE.
# For a movie that IS above the mean popularity, check whether its revenue is below 
#   the mean of movies released in the same month.
# If it is, return TRUE. If it is not, return FALSE.
movies2$underperfmonth <- rep(F, nrow(movies2))
for(i in 1:nrow(movies2)) {
  if(movies2$popularity[i] > mean(movies2$popularity)) {
    if(movies2$revenue[i] < median(movies2$revenue[movies2$release_month == movies2$release_month[i]])) {
      movies2$underperfmonth[i] <- TRUE
    }
  }
}


#22. Edit the genres variable of movies2 so that it contains only a list of the 
# genres without all the other symbols and numbers. It may contain commas and 
# spaces. The first line of code has been provided for you.
movies2$genres <-  gsub(pattern = '\\[\\{"id":' , replacement = '', movies2$genres)
movies2$genres <-  gsub('\\[','', movies2$genres)
movies2$genres <-  gsub('\\]','', movies2$genres)
movies2$genres <-  gsub('\\{"id": ','', movies2$genres)
movies2$genres <-  gsub('"name": "','', movies2$genres)
movies2$genres <-  gsub("[0-9],",'', movies2$genres)
movies2$genres <-  gsub("[0-9]",'', movies2$genres)
movies2$genres <-  gsub('"\\}','', movies2$genres)
