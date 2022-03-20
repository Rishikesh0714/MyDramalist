setwd("D:/Science/MyDramalist/WebScraping")

library(stringr)
library(rvest)
library(dplyr)

# function to get feature from nested pages
get_feature = function(series_link, feature) {
             series_page = read_html(series_link)
             
             if(feature == "genre"){
             series_genre = series_page %>% html_nodes(".show-genres .text-primary") %>%
                         html_text() %>% paste(collapse = ",")
             return(series_genre)
             }
             else if (feature == "screenwriter"){
                     series_screenwriter = series_page %>% html_nodes(".p-a-0:nth-child(3) .text-primary") %>%
                             html_text() %>% paste(collapse = ",")
                     return(series_screenwriter)    
             }
             else if (feature == "director"){
                     series_director = series_page %>% html_nodes(".p-a-0:nth-child(4) .text-primary") %>%
                             html_text() %>% paste(collapse = ",")
                     return(series_director)
             }
             else if (feature == "duration"){
                     series_duration = series_page %>% html_nodes(".m-b-0 :nth-child(7)") %>%
                             html_text() %>% paste(collapse = ",")
                     return(series_duration)
             }
             else if (feature == "cast"){
                     series_cast = series_page %>% html_nodes(".text-ellipsis b") %>%
                             html_text() %>% paste(collapse = ",")
                     return(series_cast)
             }
             else if (feature == "network"){
                     series_network = series_page %>% html_nodes(".p-l b") %>%
                             html_text() %>% paste(collapse = ",")
                     return(series_network)
             }
             
         }


# dataframe of 500 top shows on MyDramaList first 25 pages

Drama <- data.frame()


#setTimeLimit(0)

for (page_result in seq(1, 25)) {
        link = paste0("https://mydramalist.com/shows/top?page=",page_result)
        
        # facing connection time out
        download.file(link, destfile = 'temp.html')
        page = read_html("temp.html")
        
        
        series_links = page %>% html_nodes(".title a:nth-child(1)") %>%
                      html_attr("href") %>% paste("https://mydramalist.com/", ., sep="")
        
        Name <-
                page %>% html_nodes(".title a:nth-child(1)") %>% html_text()
        Ratings <-  page %>% html_nodes(".score") %>% html_text()
        Language_Year_Episodes <-
                page %>% html_nodes(".content .text-muted") %>% html_text()
        
        # making different features of language, year and number of episodes.
        lang_split <- strsplit(Language_Year_Episodes, "-")
        Language <- lapply(lang_split, "[[", 1)
        Language <- word(Language, 1)
        
        
        Year_episode <- lapply(lang_split, "[[", 2)
        Year <-  as.numeric(lapply(strsplit(trimws(Year_episode), ","), "[[", 1))
        
        Num_of_episodes <-
                lapply(strsplit(trimws(Year_episode), ","), "[[", 2)
        Num_of_episodes <- trimws(Num_of_episodes)
        Num_of_episodes <-
                as.numeric(gsub(" episodes", "", Num_of_episodes))
        
        
        Genre <- sapply(series_links, FUN = get_feature, "genre")
        Director <- sapply(series_links, FUN = get_feature, "director")
        Screenwriter <- sapply(series_links, FUN = get_feature, "screenwriter")
        Top_cast <- sapply(series_links, FUN = get_feature, "cast")
        Duration <- sapply(series_links, FUN = get_feature, "duration")
        Network <- sapply(series_links, FUN = get_feature, "network")
        
        
        
        Drama <-
                rbind(
                        Drama,
                        data.frame(
                                Name,
                                Ratings,
                                Language,
                                Year,
                                Num_of_episodes,
                                Genre,
                                Duration, 
                                Network,
                                Director, 
                                Screenwriter,
                                Top_cast,
                                stringsAsFactors = FALSE
                        )
                )
        
        
        print(paste("Page:", page_result))
        
}


write.csv(Drama, "Drama.csv")
