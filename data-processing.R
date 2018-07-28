library(tidyverse)

# load the original data
athlete_events <- read_csv("athlete_events.csv")

# filter to just gymnastics
gym <- filter(athlete_events, Sport=="Gymnastics")

# grab desired columns and convert to imperial units
gym <- gym[, c("ID", "Name", "Sex", "Age", "Height", "Weight", "Team", "Year", "Event", "Medal")] %>% 
  mutate(Height=round(Height*(0.0328084),1),
         Weight = round(Weight*(2.20462)))

# distinct athletes
gym_distinct <- gym %>% distinct(ID, .keep_all = TRUE)

# medals
medalists <- gym %>% filter(!is.na(Medal))
top_teams <- medalists %>% 
  group_by(Team) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  .$Team %>% 
  rev()
top_medals <- medalists %>% filter(Team %in% top_teams)
top_medals$Team <- factor(top_medals$Team, levels=top_teams)
top_medals$Medal <- factor(top_medals$Medal, levels=c("Gold", "Silver", "Bronze"))

# export data
write_csv(gym, "./data/olympic_gymnasts.csv")
write_csv(gym_distinct, "./data/distinct_gymnasts.csv")
write_csv(medalists, "./data/medals.csv")
write_csv(top_medals, "./data/team_medals.csv")
