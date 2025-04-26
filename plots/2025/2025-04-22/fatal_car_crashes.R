## Load libraries --------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(showtext)

# devtools::install_github("gaba-tope/socialcap")
library(socialcap)


## Load data  ------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2025-04-22')
daily_accidents <- tuesdata$daily_accidents


## Data wrangling --------------------------------------------------------------
# Add year and weekday metadata
daily_accidents <- daily_accidents %>%
  mutate(year = year(date)) %>%
  mutate(wday = wday(date, label=TRUE, abbr=TRUE))
daily_accidents$wday <- factor(daily_accidents$wday, 
                               levels=c("Mon", "Tue", "Wed", "Thu",
                                        "Fri", "Sat", "Sun"))
  
# Calculate averages and standard deviation
df <- daily_accidents %>%
  group_by(wday, year) %>%
  summarise(wday_avg = mean(fatalities_count),
            wday_std = sd(fatalities_count))
  

## Begin plotting --------------------------------------------------------------
social_caption <- socialcap(gitname = "nsohail19")
# Caption for the Plot
data_caption <- "Data : osf.io/qnrg6"
plot_caption <- paste(data_caption, "<br>Graphic:", social_caption)

# # Apply to Plot
# font_add_google(name = "Oswald", family = "oswald")
# showtext_auto()

ggplot(df, 
       aes(x = year, 
           y = wday_avg, 
           color = wday, 
           fill = wday)
  ) +
  geom_line(color = "black", linewidth=rel(1)) +
  geom_ribbon(aes(
    ymax = wday_avg+wday_std, 
    ymin = wday_avg-wday_std),
    alpha = 0.1
  ) +
  labs(x = "Year",
       y = "Fatal Car Crashes",
       title = "Fatal Car Crash Trends in the US",
       caption = plot_caption
  ) +
  facet_wrap(~wday, ncol=4) +
  theme_bw(base_size=10) +
  theme(
    plot.title = element_text(face="bold", size=rel(2.5)),
    axis.title.x = element_text(size=rel(2)),
    axis.title.y = element_text(size=rel(2)),
    axis.text.x = element_text(size=rel(1.5), angle=90, vjust=1, hjust=1),
    axis.text.y = element_text(size=rel(1.5)),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_line(),
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line(),
    legend.position = "none", 
    strip.text = element_text(size = rel(1.5), face="bold"),
    plot.caption = element_textbox(size=15,
                                   family = "oswald",
                                   hjust = 0)
  ) +
  scale_x_continuous(breaks = seq(1995, max(df$year), by = 5))


## Save ------------------------------------------------------------------------
ggsave(
  filename = "fatal_car_crashes.png",
  height = 600,
  width = 1050,
  units = "px",
)