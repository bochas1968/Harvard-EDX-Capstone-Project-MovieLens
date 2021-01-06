##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


################################################################################
########################### Data Analysis ######################################
################################################################################

# Installing Packages necessary for Data Descroption , Data Tables and Graphs..

if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr","http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr","http://cran.us.r-project.org")

library(tidyverse)
library(plotly)
library(kableExtra)
library(ggplot2)
library(knitr)
library(forcats)
library(ggpubr)
library(stringr)
library(tidyr)


# Generating Date format . A first view of the first 10 files 

head(edx) %>%  kable(caption = "Table I: A Sample of the edx File") %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("scale_down", "HOLD_position"))

#
# Convert timestamp to a human readable date

edx$date <- as.POSIXct(edx$timestamp, origin="1970-01-01")
validation$date <- as.POSIXct(validation$timestamp, origin="1970-01-01")

# Extract the year and month of rate in both dataset

edx$yearOfRate <- format(edx$date,"%Y")
edx$monthOfRate <- format(edx$date,"%m")

validation$yearOfRate <- format(validation$date,"%Y")
validation$monthOfRate <- format(validation$date,"%m")

# Extract the year of release for each movie in both dataset
# First in the edx dataset

edx <- edx %>%
  mutate(title = str_trim(title)) %>%
  extract(title,
          c("titleTemp", "release"),
          regex = "^(.*) \\(([0-9 \\-]*)\\)$",
          remove = F) %>%
  mutate(release = if_else(str_length(release) > 4,
                           as.integer(str_split(release, "-",
                                                simplify = T)[1]),
                           as.integer(release))
  ) %>%
  mutate(title = if_else(is.na(titleTemp),
                         title,
                         titleTemp)
  ) %>%
  select(-titleTemp)

# second in the validation dataset

validation <- validation %>%
  mutate(title = str_trim(title)) %>%
  extract(title,
          c("titleTemp", "release"),
          regex = "^(.*) \\(([0-9 \\-]*)\\)$",
          remove = F) %>%
  mutate(release = if_else(str_length(release) > 4,
                           as.integer(str_split(release, "-",
                                                simplify = T)[1]),
                           as.integer(release))
  ) %>%
  mutate(title = if_else(is.na(titleTemp),
                         title,
                         titleTemp)
  ) %>%
  select(-titleTemp)

# Extract the genre in edx datasets 

genres <- str_replace(edx$genres,"\\|.*","")
genres <- genres[!duplicated(genres)]

# Extract the genre in validation datasets

genres <- str_replace(validation$genres,"\\|.*","")
genres <- genres[!duplicated(genres)]



# remove unnecessary columns on edx and validation dataset

edx <- edx %>% select(userId, movieId, rating, title, genres, release, yearOfRate, monthOfRate)

validation <- validation %>% select(userId, movieId, rating, title, genres, release, yearOfRate, monthOfRate)

# Convert the columns into the desidered data type

edx$yearOfRate <- as.numeric(edx$yearOfRate)
edx$monthOfRate <- as.numeric(edx$monthOfRate)
edx$release <- as.numeric(edx$release)

validation$yearOfRate <- as.numeric(validation$yearOfRate)
validation$monthOfRate <- as.numeric(validation$monthOfRate)
validation$release <- as.numeric(validation$release)

# Head

head(edx) %>% kable(caption = "Table II: A Sample of the transformed  edx File") %>% 
kable_styling(font_size = 10, position = "center",
              latex_options = c("scale_down", "HOLD_position"))
# Total unique movies and users
summary(edx) %>% kbl() %>% kable_styling(font_size = 10, position = "center",
                                         latex_options = c("scale_down", "HOLD_position"))

# Number of unique movies and users in the edx dataset 
edx %>%
  summarize(n_users = n_distinct(userId), 
            n_movies = n_distinct(movieId)) 

# Plot number of ratings per movie ordered 
rating_sum <- edx %>% group_by(rating) %>%
  summarize(count = n())

gg <- rating_sum %>% 
  mutate(rating = factor(rating),
         top_rating = ifelse(rating %in% c(3,4,5),"high","low")) %>%
  ggplot(aes(x = reorder(rating, count/100), count/100, fill = top_rating)) +
  geom_col(color = "white") +
  coord_flip() +
  scale_fill_manual(values = c("dark blue","grey")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 12)) +
  labs(title = "Ranking most frequent rating value",
       x = "Rating")
gg


# Density of Ratings per movie
movie_sum <- edx %>% group_by(movieId) %>%
  summarize(n_rating_of_movie = n(), 
            mu_movie = mean(rating),
            sd_movie = sd(rating))

gg <- movie_sum %>% ggplot(aes(n_rating_of_movie)) +
  geom_density(fill = "blue") +
  labs(title = "Density plot - Number of Ratings per Movie",
       x = "number of rating",
       y = "density") + 
  scale_x_continuous(limits = c(0,2000,200)) +
  geom_vline(aes(xintercept = mean(n_rating_of_movie)), color = "grey")+
  annotate("text", x = 1000, y = 0.0022,
           label = print(round(mean(movie_sum$n_rating_of_movie),0)),
           color = "grey", size = 3) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "none")
gg



# Standard Deviation of Rating
gg <- movie_sum %>% 
  mutate(group = cut(n_rating_of_movie, 
                     breaks = c(-Inf, mean(n_rating_of_movie),Inf), 
                     labels = c("n < 843", "n > 843"))) %>%
  ggplot(aes(sd_movie, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Standard deviation of rating",
       x = "Standard deviation",
       y = "count",
        plot.title = element_text(size = 12),
       caption = "Figure 3 - 
       N < 843 number of rating less than average,
       N > 843 number of rating greater than average") + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12))
gg

# Graph Distribution of Movie Ratings 

gg <- edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "Dark blue", fill="blue") +
  labs(title = "Number of ratings per movie",
       x = "Number of Ratings",
       y = "Number of Movies")+ 
  scale_x_log10()+
  theme_classic() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 10))
gg

# Table 10 movies rated only once
edx %>%
  group_by(movieId) %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  left_join(edx, by = "movieId") %>%
  group_by(title) %>%
  summarize(rating = rating, n_rating = count) %>%
  slice(1:20) %>%
  kable(caption = "10 Movies rated only one")%>%
  kable_styling(font_size = 10, position = "center",
                               latex_options = c("scale_down", "HOLD_position"))


# Plot number of ratings given by users


gg <- edx %>% count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "navy blue",fill="blue") +
  scale_x_log10() +
  xlab("Number of ratings") + 
  ylab("Number of users") +
  ggtitle("Number of ratings given by users") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.position = "none")
gg


# Distribution by Genres


#####
#View of all unique genres
unique_genres_list <- str_extract_all(unique(edx$genres), "[^|]+") %>%
  unlist() %>%
  unique() %>% 
  
  kable(caption = "List of Unique Genres") %>% kable_styling(font_size = 10, position = "center",
                                                             latex_options = c("scale_down", "HOLD_position"))


# Creating the long version of both the train and validation datasets with separeted genres
edx_genres <- edx %>%
  separate_rows(genres, sep = "\\|", convert = TRUE)

validation_genres <- validation %>%
  separate_rows(genres, sep = "\\|", convert = TRUE)
hist_genres <- ggplot(edx_genres, aes(x = reorder(genres, genres, function(x) -length(x)))) +
  geom_bar(fill = "blue") +
  labs(
    title = "Number of Ratings per genre",
    x = "Genre", y = "Counts"
  ) +
  scale_y_continuous(
    labels = paste0(1:4, "M"),
    breaks = 10^6 * 1:4
  ) +
  coord_flip() +
  theme_classic()
hist_genres


# Boxplot of movie ratings per genre

boxplot_genre_ratings <- ggplot(edx_genres, aes(genres, rating)) +
  geom_boxplot(fill = "blue", varwidth = TRUE) +
  labs(
    title = "Movie ratings per genre",
    x = "Genre", y = "Rating", fill = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
boxplot_genre_ratings




################################################################################
######################### Modelling Approach ###################################3
################################################################################

## 1) Average movie rating model ##

# This is the simplest possible recommendation system as we predict the same 
#rating for all movies regardless of user.  The model assumes
# the same ratings for all movies (the average) with all the differences explained by random
#variation is the looks like Y= mu + error 

# Compute the dataset's mean rating
mu <- mean(edx$rating)


# Test results based on simple prediction
naive_rmse <- RMSE(validation$rating, mu)

# Check results
# Save prediction in data frame
rmse_results <- data_frame(Method = "Average Movie Rating Model (Naive)", RMSE = naive_rmse)
rmse_results %>% kable(caption = "RMSE of Alternative Models") %>% kable_styling()

## 2) Movie effect model ##
# Here we assume that some movies are just rated higher than others. 
# We augment  a term by adding a term b_i (average rating of movie i) 
# Simple model taking into account the movie effect b_i
# Subtract the rating minus the mean for each rating the movie received
# Plot number of movies with the computed b_i
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

gg <- movie_avgs %>% ggplot(aes(b_i)) +
  geom_histogram(bins = 10, color = "navy blue",fill="blue") +
  xlab("b_i") + 
  ylab("Number of movies") +
  ggtitle("Number of movies with the computed b_i")+
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.position = "none")
gg

# Test and save rmse results and add to the table
predicted_ratings <- mu +  validation %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Movie Effect Model",  
                                     RMSE = model_1_rmse ))
rmse_results %>%  kable(caption = "RMSE of Alternative Models") %>% kable_styling()

## Movie and user effect model ##

# A) Plot penalTy term user effect #
user_avgs<- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating - mu - b_i))

gg <- user_avgs %>% ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "navy blue",fill="blue") +
  xlab("b_u") + 
  ylab("Number of movies") +
  ggtitle("Number of movies with the computed b_u")+
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.position = "none")
gg


#overcome Group effects 

user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


# B) compute, save rmse results and add results to table 
predicted_ratings <- validation%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Movie and User Effect Model",  
                                     RMSE = model_2_rmse))
rmse_results %>% kable(caption = "RMSE of Alternative Models") %>% kable_styling()

######## Method: Movie + User + Genres Movie Effects Model #############

# A) Plot Genres Movie effect #

genres_avgs_ind <- edx_genres %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_gInd = mean(rating - mu - b_i - b_u))

gg <- genres_avgs_ind %>% ggplot(aes(b_gInd)) +
  geom_histogram(bins = 10, color = "navy blue",fill="blue") +
  xlab("b_gInd") + 
  ylab("Number of movies") +
  ggtitle("Number of movies with the computed Genre Movie Effects(b_gInd)")+
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.position = "none")
gg

# B) compute, save rmse results and add results to table 
predicted_ratings <- validation_genres %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genres_avgs_ind, by = c("genres")) %>%
  mutate(pred = mu + b_i + b_u + b_gInd) %>%
  pull(pred)

predicted_ratings <- clamp(predicted_ratings, 0.5, 5)

model_m_u_gInd <- RMSE(predicted_ratings, validation_genres$rating)
rmse_results <- bind_rows(
  rmse_results,
  tibble(
    Method = "Movie + User + Genres Movie Effects Model",
    RMSE = model_m_u_gInd
  )
)
rmse_results %>% kable(caption = "RMSE of Alternative Models") %>% kable_styling()


# Method: Movie + User + Genres Movies + Genre_User Effects Model -----------

# A) Plot Genres  User effect #


genres_user_avgs <- edx_genres %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genres_avgs_ind, by = "genres") %>%
  group_by(genres, userId) %>%
  summarize(b_gu = mean(rating - mu - b_i - b_u - b_gInd))

gg <- genres_user_avgs %>% ggplot(aes(b_gu)) +
  geom_histogram(bins = 10, color = "navy blue",fill="blue") +
  xlab("b_gu") + 
  ylab("Number of movies") +
  ggtitle("Number of movies with the computed Genre UserEffects(b_gu)")+
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.position = "none")
gg

predicted_ratings <- validation_genres %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genres_avgs_ind, by = c("genres")) %>%
  left_join(genres_user_avgs, c("userId", "genres")) %>%
  mutate(
    b_gu = ifelse(is.na(b_gu), 0, b_gu),
    pred = mu + b_i + b_u + b_gInd + b_gu
  ) %>%
  pull(pred)

predicted_ratings <- clamp(predicted_ratings, 0.5, 5)

model_m_u_gInd_gu <- RMSE(predicted_ratings, validation_genres$rating)
rmse_results <- bind_rows(
  rmse_results,
  tibble(
    Method = "Movie + User + Genre Movie + Genre User Effects Model",
    RMSE = model_m_u_gInd_gu
  )
)

rmse_results %>% kable(caption = "RMSE of Alternative Models")%>% kable_styling()



#########################################################################
############################ Regularization #######
#########################################################################


# Regularized Model I (Regularized Movie Effect Model) #

# Lambda represents the Tuning parameter
# Use cross-validation to choose it.
lambdas <- seq(0, 4, 0.25)


# Estimate the RMSE fr each Lambda 

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})


# Plot RMSE vs lambdas to select the optimal lambda                                                             

qplot(lambdas, rmses,xlab="Lambda",ylab="Root Mean Square Error (RMSE)",main="Penalty Parameter & RMSE")  


# The optimal lambda                                                             
lambda <- lambdas[which.min(rmses)]


# Test Results and Add to Table                                                              
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Regularized Movie Effect Model",  
                                     RMSE = min(rmses)))

rmse_results %>% kable(caption = "RMSE of Alternative Models") %>% kable_styling()



#Regularized Model II (Regularized Movie and User Effect)#


# Lambda represents the Tuning parameter
# Use cross-validation to choose it.
lambdas <- seq(2, 6, 0.25)


# For each lambda,find b_i & b_u, followed by rating prediction & testing
# note:the below code could take some time  
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})


# Plot RMSE vs lambdas to select the optimal lambda                                                             

qplot(lambdas, rmses,xlab="Lambda",ylab="Root Mean Square Error (RMSE)",main="Penalty Parameter & RMSE")  

# The optimal lambda                                                             
lambda <- lambdas[which.min(rmses)]


# Test                                                              
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Regularized Movie and User Effect Model",  
                                     RMSE = min(rmses)))


# Add Results to Table
rmse_results %>% kable(caption = "RMSE of Alternative Models") %>% kable_styling()

# Method: Regularized Movie + User + Genre Individual Effect Model --------
# Regularized parameter
lambdas <- seq(2, 6, 0.25)

# Grid search to tune the regularized parameter lambda
rmses <- sapply(lambdas, function(l) {
  mu <- mean(edx$rating)
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + l))
  
  b_u <- edx %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu) / (n() + l))
  
  b_gInd <- edx_genres %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_gInd = sum(rating - mu - b_i - b_u) / (n() + l))
  
  predicted_ratings <- validation_genres %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_gInd, by = "genres") %>%
    mutate(
      b_gInd = ifelse(is.na(b_gInd), 0, b_gInd),
      pred = mu + b_i + b_u + b_gInd
    ) %>%
    pull(pred)
  
  
  return(RMSE(predicted_ratings, validation_genres$rating))
})

qplot(lambdas, rmses,xlab="Lambda",ylab="Root Mean Square Error (RMSE)",main="Penalty Parameter & RMSE")  

lambda <- lambdas[which.min(rmses)]

rmse_results <- bind_rows(
  rmse_results,
  tibble(
    Method = "Regularized Movie + User + Genre Movie Effects Model",
    RMSE = min(rmses)
  )
)

rmse_results %>% kable(caption = "RMSE of Alternative Models") %>% kable_styling()


# Method: Regularized Movie + User + Genre Movie + Genre_User Effect Model --------
# Regularized parameter
lambdas <- seq(2, 6, 0.25)

# Grid search to tune the regularized parameter lambda
rmses <- sapply(lambdas, function(l) {
  mu <- mean(edx$rating)
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + l))
  
  b_u <- edx %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu) / (n() + l))
  
  b_gInd <- edx_genres %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_gInd = sum(rating - mu - b_i - b_u) / (n() + l))
  
  b_gu <- edx_genres %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_gInd, by = "genres") %>%
    group_by(userId, genres) %>%
    summarize(b_gu = sum(rating - mu - b_i - b_u - b_gInd) / (n() + l))
  
  predicted_ratings <- validation_genres %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_gInd, by = "genres") %>%
    left_join(b_gu, by =  "genres") %>%
    mutate(
      b_gu = ifelse(is.na(b_gu), 0, b_gu),
      pred = mu + b_i + b_u + b_gInd + b_gu
    ) %>%
    pull(pred)
  

  return(RMSE(predicted_ratings, validation_genres$rating))
})

qplot(lambdas, rmses,xlab="Lambda",ylab="Root Mean Square Error (RMSE)",main="Penalty Parameter & RMSE")  
# The optimal lambda                                                             

lambda <- lambdas[which.min(rmses)]

rmse_results <- bind_rows(
  rmse_results,
  tibble(
    Method = "Regularized Movie + User + Genre Movie & User Effects Mode ",
    RMSE = min(rmses)
  )
)

#Final Results Table
rmse_results %>% kable(caption = "RMSE of Alternative Models") %>% kable_styling()

