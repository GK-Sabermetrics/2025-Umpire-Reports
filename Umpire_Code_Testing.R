# Code Testing
ggplot(data = game_data, 
       aes(x = -PlateLocSide, y = PlateLocHeight, fill = PitchCall)) +
  xlim(-1.5,1.5) + ylim(0,4) + labs(title = paste("Overall"), x = paste("Feet"), y = paste("Feet") ) +
  geom_rect(aes(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
  # Home Plate Outline Below
  geom_segment(aes(x = -0.708, y = 0.3, xend = 0.708, yend = 0.3), size = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.15, xend = 0, yend = 0), size = 1, color = "black") +
  geom_segment(aes(x = 0, y = 0.0, xend = 0.708, yend = 0.15), size = 1, color = "black") +
  geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
  geom_point(size = 5, alpha = .75, pch = 21) +
  scale_fill_manual(values = pcolors) +
  geom_text(aes(x = -PlateLocSide, y = PlateLocHeight, label = ZoneDistance), size = 3, color = 'white') +
  theme_bw() + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
  theme(legend.position = "none")  +
  #facet_wrap(~Count) +
  theme(aspect.ratio = 1)


ggplot(data = game_data, aes(x = -PlateLocSide, y = PlateLocHeight, fill = PitchCall)) +
  xlim(-2.5,2.5) + ylim(0,5) + labs(title = paste("Overall"), x = paste("test"), y = "test" ) +
  geom_rect(aes(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
  # Home Plate Outline Below
  geom_segment(aes(x = -0.708, y = 0.3, xend = 0.708, yend = 0.3), size = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.15, xend = 0, yend = 0), size = 1, color = "black") +
  geom_segment(aes(x = 0, y = 0.0, xend = 0.708, yend = 0.15), size = 1, color = "black") +
  geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
  geom_point(size = 5, alpha = .75, pch = 21) +
  scale_fill_manual(values = pcolors)

ggplot(game_data, aes(x = PlateLocSide, y = PlateLocHeight, fill = PitchCall)) +
  xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Location vs LHH" ) )+
  geom_rect(aes(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
  geom_rect(aes(xmin = -0.8288, xmax = 0.8288, ymin = 1.379, ymax = 3.6208), alpha = 0, size = .75, color = "black") +
  # Home Plate Outline Below
  geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") +
  geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") +
  geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
  geom_point(size = 7.75, alpha = .75, pch = 21) +
  scale_fill_manual(values = pcolors) +
  theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
  theme(legend.position = "none", legend.text = element_text(size = 10), axis.title = element_blank())  +
  theme(strip.text = element_text(size = 7, face = 'bold'),
        axis.text.x=element_blank(), #remove x axis labels
        axis.text.y=element_blank(),  #remove y axis labels
  )

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
  geom_point(size = 6, alpha = .85, pch = 21) +
  scale_fill_manual(values = pcolors) +
  geom_text(aes(x = -PlateLocSide, y = PlateLocHeight, label = ZoneDistance), size = 3, color = 'white') +
  theme_bw() + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5), axis.title = element_blank()) +
  theme(legend.position = "none")  +
  theme(aspect.ratio = 1)



filter(game_data, MissedCall == 1) %>% select(PitchNo, Inn, Pitcher, Batter, Count, Outs, PitchCall, PAOutcome, Likely, 
                                              PlateLocSide, PlateLocHeight, ZoneDistance)


xmin = -0.708
xmax = 0.708
ymin = 1.5
ymax = 3.5

x_p = 1.01602
y_p = 3.65735

dx <- max(xmin - x_p, 0, x_p - xmax)
dy <- max(ymin - y_p, 0, y_p - ymax)

distance <- sqrt(dx^2 + dy^2)

max(-0.708 - gamefile$PlateLocSide, 0, gamefile$PlateLocSide - 0.708)

point_to_rectangle_distance <- function(PlateLocSide, PlateLocHeight, xmin, xmax, ymin, ymax) {
  # Horizontal distance to the rectangle
  dx <- max(xmin - PlateLocSide, 0, PlateLocSide - xmax)
  # Vertical distance to the rectangle
  dy <- max(ymin - PlateLocHeight, 0, PlateLocHeight - ymax)
  # Distance (0 if inside, Pythagorean otherwise)
  distance <- sqrt(dx^2 + dy^2)
  return(distance)
}

point_to_rectangle_distance(1.01, 3.65, -0.708, 0.708, 1.5, 3.5)


