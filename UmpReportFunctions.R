# Ump Report Functions
library(tidyverse)
library(kableExtra)
library(gridExtra)
library(gtsummary)
library(pak)
library(kableExtra)
library(knitr)
library(pandoc)
library(scales)
library(ggrepel)
library(ggplot2)

pcolors = c('#d22d49','#00d1ed')
pcolors = setNames(pcolors, c('Strike', 'Ball'))

Overall_Performance_Plot = function(game_data) {

  ggplot(data = game_data, aes(x = -PlateLocSide, y = PlateLocHeight, fill = PitchCall)) +
  xlim(-2,2) + ylim(0,4.5) + labs(title = paste("Overall"), x = paste("Feet"), y = paste("Feet") ) +
  geom_rect(aes(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
  # Home Plate Outline Below
  geom_segment(aes(x = -0.708, y = 0.3, xend = 0.708, yend = 0.3), size = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.15, xend = 0, yend = 0), size = 1, color = "black") +
  geom_segment(aes(x = 0, y = 0.0, xend = 0.708, yend = 0.15), size = 1, color = "black") +
  geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
  geom_point(size = 4, alpha = .75, pch = 21) +
  scale_fill_manual(values = pcolors) +
  theme_bw() + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5), axis.title = element_blank()) +
  theme(legend.position = "none")  +
  theme(aspect.ratio = 1)

}

Missed_Calls_Plot = function(game_data) {
  
  ggplot(data = game_data[game_data$MissedCall == 1,], 
         aes(x = -PlateLocSide, y = PlateLocHeight, fill = PitchCall)) +
    xlim(-2,2) + ylim(0,4.5) + labs(title = paste("Missed Calls")) +
    geom_rect(aes(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
    geom_rect(aes(xmin = -0.8288, xmax = 0.8288, ymin = 1.379, ymax = 3.6208), alpha = 0, size = .75, color = "red") +
    # Home Plate Outline Below
    geom_segment(aes(x = -0.708, y = 0.3, xend = 0.708, yend = 0.3), size = 1, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.15, xend = 0, yend = 0), size = 1, color = "black") +
    geom_segment(aes(x = 0, y = 0.0, xend = 0.708, yend = 0.15), size = 1, color = "black") +
    geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
    geom_point(size = 4, alpha = .75, pch = 21) +
    scale_fill_manual(values = pcolors) +
    theme_bw() + 
    theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5), axis.title = element_blank()) +
    theme(legend.position = "none")  +
    theme(aspect.ratio = 1)
  
}

Missed_Strikes_Plot = function(game_data) {
  
  ggplot(data = game_data[game_data$MissedCall == 1 & game_data$PitchCall == "Ball",], 
         aes(x = -PlateLocSide, y = PlateLocHeight, fill = PitchCall)) +
    xlim(-2,2) + ylim(0,4.5) + labs(title = paste("Missed Strikes")) +
    geom_rect(aes(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
    geom_rect(aes(xmin = -0.8288, xmax = 0.8288, ymin = 1.379, ymax = 3.6208), alpha = 0, size = .75, color = "red") +
    # Home Plate Outline Below
    geom_segment(aes(x = -0.708, y = 0.3, xend = 0.708, yend = 0.3), size = 1, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.15, xend = 0, yend = 0), size = 1, color = "black") +
    geom_segment(aes(x = 0, y = 0.0, xend = 0.708, yend = 0.15), size = 1, color = "black") +
    geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
    geom_point(size = 4, alpha = .75, pch = 21) +
    scale_fill_manual(values = pcolors) +
    theme_bw() + 
    theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5), axis.title = element_blank()) +
    theme(legend.position = "none")  +
    theme(aspect.ratio = 1)
  
}

Missed_Balls_Plot = function(game_data) {
  
  ggplot(data = game_data[game_data$MissedCall == 1 & game_data$PitchCall == "Strike",], 
         aes(x = -PlateLocSide, y = PlateLocHeight, fill = PitchCall)) +
    xlim(-2,2) + ylim(0,4.5) + labs(title = paste("Missed Balls")) +
    geom_rect(aes(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
    geom_rect(aes(xmin = -0.8288, xmax = 0.8288, ymin = 1.379, ymax = 3.6208), alpha = 0, size = .75, color = "red") +
    # Home Plate Outline Below
    geom_segment(aes(x = -0.708, y = 0.3, xend = 0.708, yend = 0.3), size = 1, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.15, xend = 0, yend = 0), size = 1, color = "black") +
    geom_segment(aes(x = 0, y = 0.0, xend = 0.708, yend = 0.15), size = 1, color = "black") +
    geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
    geom_point(size = 4, alpha = .75, pch = 21) +
    scale_fill_manual(values = pcolors) +
    theme_bw() + 
    theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5), axis.title = element_blank()) +
    theme(legend.position = "none")  +
    theme(aspect.ratio = 1)
  
}

Missed_Call_Situations_Plot = function(game_data) {
  
  ggplot(data = game_data[game_data$MissedCall == 1,], 
         aes(x = -PlateLocSide, y = PlateLocHeight, fill = PitchCall)) +
    xlim(-2,2) + ylim(0,4.5) + labs(title = paste("Missed Call Situations")) +
    geom_rect(aes(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
    geom_rect(aes(xmin = -0.8288, xmax = 0.8288, ymin = 1.379, ymax = 3.6208), alpha = 0, size = .75, color = "red") +
    # Home Plate Outline Below
    geom_segment(aes(x = -0.708, y = 0.3, xend = 0.708, yend = 0.3), size = 1, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.15, xend = 0, yend = 0), size = 1, color = "black") +
    geom_segment(aes(x = 0, y = 0.0, xend = 0.708, yend = 0.15), size = 1, color = "black") +
    geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
    geom_point(size = 4, alpha = .85, pch = 21) +
    scale_fill_manual(values = pcolors) +
    geom_text(aes(x = -PlateLocSide, y = PlateLocHeight, label = PitchNo), size = 2, color = 'white') +
    theme_bw() + 
    theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5), axis.title = element_blank()) +
    theme(legend.position = "none")  +
    theme(aspect.ratio = 1)
  
}

Missed_Call_Distance_Plot = function(game_data) {
  
  ggplot(data = game_data[game_data$MissedCall == 1,], 
         aes(x = -PlateLocSide, y = PlateLocHeight, fill = PitchCall)) +
    xlim(-2,2) + ylim(0,4.5) + labs(title = paste("Missed Call Distance")) +
    geom_rect(aes(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
    geom_rect(aes(xmin = -0.8288, xmax = 0.8288, ymin = 1.379, ymax = 3.6208), alpha = 0, size = .75, color = "red") +
    # Home Plate Outline Below
    geom_segment(aes(x = -0.708, y = 0.3, xend = 0.708, yend = 0.3), size = 1, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.15, xend = 0, yend = 0), size = 1, color = "black") +
    geom_segment(aes(x = 0, y = 0.0, xend = 0.708, yend = 0.15), size = 1, color = "black") +
    geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
    geom_point(size = 4, alpha = .85, pch = 21) +
    scale_fill_manual(values = pcolors) +
    geom_text(aes(x = -PlateLocSide, y = PlateLocHeight, label = ZoneDistance), size = 2, color = 'white') +
    theme_bw() + 
    theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5), axis.title = element_blank()) +
    theme(legend.position = "none")  +
    theme(aspect.ratio = 1)
  
}

UmpireReport = function(gamefile){
  
  #### Data Pull ####
  game_data = gamefile %>% filter(PitchCall %in% c('Strike', 'Ball'))
  
  date = unique(game_data$Date)
  
  accuracy = percent((nrow(game_data) - sum(game_data$MissedCall))/nrow(game_data))
  
  overall_performance_plot = Overall_Performance_Plot(game_data = game_data)
  
  missed_calls_plot = Missed_Calls_Plot(game_data = game_data)
  
  missed_balls_plot = Missed_Balls_Plot(game_data = game_data)
  
  missed_strikes_plot = Missed_Strikes_Plot(game_data = game_data)
  
  missed_call_situations_plot = Missed_Call_Situations_Plot(game_data = game_data)
  
  missed_call_distance_plot = Missed_Call_Distance_Plot(game_data = game_data)
  
  #### Game Stats ####
  
  game_stats = 
  game_data %>% 
    group_by(PitchCall) %>% 
    summarise(
      'Calls' = n(),
      'In Zone' = sum(InZone),
      'Out of Zone' = sum(OutZone),
      'Missed Calls' = sum(MissedCall),
      'Accuracy' = percent((Calls - `Missed Calls`)/n())
      )

  missed_call_list = 
    game_data %>% 
    filter(MissedCall == 1) %>% select(PitchNo, Inn, Pitcher, Batter, Count, 
                                       Outs, PitchCall, PAOutcome, Likely)
  
  #### Parameters ####
  params = list(
    date = date,
    opponent = opponent,
    accuracy = accuracy,
    game_data = game_data,
    overall_performance_plot = overall_performance_plot,
    missed_calls_plot = missed_calls_plot,
    missed_balls_plot = missed_balls_plot,
    missed_strikes_plot = missed_strikes_plot,
    missed_call_situations_plot = missed_call_situations_plot,
    missed_call_distance_plot = missed_call_distance_plot
  )
  
  rmarkdown::render(input = "UmpireReport.Rmd",
                    params = params,
                    quiet = FALSE) 
  
}




