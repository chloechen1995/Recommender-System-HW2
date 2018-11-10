get_genre = function(user_id){
  # load the data and change the colnames
  load('~/ML100K')
  names(udata) = c('user id', 'item id', 'rating', 'timestamp')
  names(ugenre) = c('genre')
  names(uitem) = c('movie id', 'movie title', 'release date', 'video release date',
                   'IMDb URL', 'unknown', 'Action', 'Adventure', 'mAnimation', 
                   "Children's", "Comedy ", "Crime", "Documentary", "Drama", "Fantasy", 
                   'Film-Noir', 'Horror', 'Musical', 'Mystery', 'Romance', 'Sci-Fi', 
                   'Thriller', 'War', 'Western')
  names(uuser) = c('user id', 'age', 'gender', 'occupation', 'zip code')
  
  library(dplyr)
  # select movie id and genere columns
  genre_movie = dplyr::select(uitem, 'movie id', 'unknown': 'Western')
  # select data from udata where its user id matches the input user id
  udata_match = udata[which(udata$`user id` == user_id), ]
  # merge the udata_match with the genre data
  user_data = merge(udata_match, genre_movie, by.x = c('item id'), by.y = 'movie id')
  # subset the genre information
  subset_col = dplyr::select(user_data, 'unknown': 'Western')
  
  # find out which genre that user rates the most
  fav_movie_list = lapply(apply(subset_col, 1, function(x) which(x == 1)), names)
  # construct the dataframe with the number of genre that the user has rated
  movie_df = as.data.frame(table(unlist(fav_movie_list)))
  # merge the movie df and the genre df so that we can get the genre id
  movie_genre = merge(movie_df, ugenre, by.x = 'Var1', by.y = 'genre')
  colnames(movie_genre)[3] = 'genre'
  # if there is a tie for most frequently-rated genre, take the lowest ID
  movie_genre[order(movie_genre$Freq, -movie_genre$genre, decreasing = TRUE), ][1, 3]
}


load('~/ML100K')
names(udata) = c('user id', 'item id', 'rating', 'timestamp')
get_genre(186)
top_10_user = udata$`user id`[0:10]
sapply(top_10_user, get_genre)

# Conclusion: The most popular genres are Action, Drama, and Thriller. However, Drama is the most dominant. 
