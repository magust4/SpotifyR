#Pulls data from spotify and creates visual representations
#Mickael Agustin
#05/08/2024

library(spotifyr)
library(knitr)

Sys.setenv(SPOTIFY_CLIENT_ID = "39e68d74ea964fe8b12e4d03f28c817c")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "2e4bd1398d6e4e0aa43ed25e0116b4bf")

#Creates a line graph of the danceability, enegy, and valence of the given album
#Params: string of the name of the album
#Return: table of the track number, title, danceability, energy, and valance
graph_audio_features_of_album <- function(album_name) {
  album_data <- search_spotify(album_name, type = "album", limit = 1)
  album <- get_album(album_data$id)

  track_titles <- album$tracks$items$name
  
  track_danceability <- c()
  track_energy <- c()
  track_valence <- c()

  for (track in album$tracks$items$id) {
    audio_features <- get_track_audio_features(track)
    track_danceability <- c(track_danceability, audio_features$danceability)
    track_energy <- c(track_energy, audio_features$energy)
    track_valence <- c(track_valence, audio_features$valence)
  }

  num_of_tracks <- album$tracks$total
  
  plot(1:num_of_tracks, track_danceability,
       main = paste("Audio Features of", album_data$name, "by", album_data$artists[[1]]$name), 
       type = "l", lwd = 2, ylim = c(0,1.25), 
       col = "red", xlab = "Track Number", ylab = "Feature Value")
  lines(1:num_of_tracks, track_energy, lwd = 2, col = "green")
  lines(1:num_of_tracks, track_valence, lwd = 2, col = "blue")
  legend('topright', legend = c("Danceability", "Energy", "Valence"), col = c("red", "green", "blue"), lty = 1)


  audio_features_table <- data.frame(Track_Num = 1:num_of_tracks, Title = track_titles, 
        Danceability = track_danceability, Energy = track_energy, Valence = track_valence)
  kable(audio_features_table, format = "markdown")
}

#Creates a bar graph of the average danceability of the top 50 songs in the countries provided
#Params: vector/array of strings
#Return: none
compare_country_danceability <- function(country_list) {
    danceability <- c()
    for(country in country_list){
      danceability <- append(danceability, get_avg_country_danceability(country))
    }
    barplot(danceability, names.arg = country_list, ylim = c(0,1), 
      col = c('blue', 'yellow', 'red', 'green'), main = "Danceability of the Top 50 Songs in Each Country")
}

#searches for the top 50 playlist of the country and calculates its average danceability
#params: string - name of the country
#return: numerical value of the average danceability
get_avg_country_danceability <- function(country_name){
    playlist_name <- paste("top 50 -", country_name)
    playlist_data <- search_spotify(playlist_name, type = "playlist", limit = 1)
    playlist <- get_playlist_tracks(playlist_data$id, limit = 50)
    return(get_avg_danceability(playlist))
}

#calculates the average danceability of a given playlist
#params: playlist object
#return: numerical value of the average danceability
get_avg_danceability <- function(playlist_object) {
  danceability_list <- c()
  for (track_id in playlist_object$track.id){
    track_info <- get_track_audio_features(track_id)
    danceability_list <- append(danceability_list, track_info$danceability)
  }
  return(mean(danceability_list))
}
