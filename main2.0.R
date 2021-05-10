library(tidyverse)

questvec <- NULL #unlist(str_split(rawquest, ", "))
ratingvec <- c('', 'No', 'A little', 'Moderately', 'Significantly', 'Very significantly', 'Unsure')
filelist <- list.files('./Inputs', pattern = "*.csv")
filesnum <- length(filelist)
#questvec <- c('Create better networks for your funding', 'Discover funding opportunities', 'Bleep blorp')
envector <- NULL
mastervec <- c('Unsure', 'No', 'A little', 'Moderately', 'Significantly', 'Very significantly')

# Read in all the files

dataframes <- map(paste0('./Inputs/', filelist), ~read.csv(.x, stringsAsFactors = FALSE)) %>%
  # Set names to second row
  map(~set_names(.x, .x[1, ])) %>%
  map(~subset(.x, select = which(!duplicated(names(.x)) & 
                                   names(.x) != "" &
                                   names(.x) != "NA" & 
                                   names(.x) != "Open-Ended Response" )) %>% 
        slice(-1) %>%
        map_df(as.factor))
# Select only columns with impact scores

rating_questions <- dataframes %>%
  map(~.x %>%
        select_if(~all(levels(.) %in% ratingvec)) %>%
        gather(key = "area", value = "response") %>%
        group_by_all() %>%
        summarise(n = n())
  )

# Get totals across all events

all_events <- rating_questions %>%
  bind_rows %>%
  group_by(area, response) %>%
  summarise(n = sum(n))

# Get summary for events
by_event <- bind_rows(rating_questions, .id = 'event') %>%
  mutate(event_name = filelist[as.numeric(event)])

write.csv(all_events, 'all_events.csv')
write.csv(by_event, 'by_event.csv')


