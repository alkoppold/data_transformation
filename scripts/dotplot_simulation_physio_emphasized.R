## simulate a potential plot
# Koppold, 30.06.2023

# Load the required library
library(data.table)
library(ggplot2)
library(tidyverse)

# Set the seed for reproducibility
rm(list = ls())
set.seed(123)

# Define the variables and their possible values
transformation_procedure <- c("not_reported", "t_transformation", "raw",
                              "square_root", "log10", "range_corr", 
                              "log_range_corr", "z_transformation", "inverse")
SCR <- sample(0, size = 200, replace = TRUE)
EMG <- sample(0:1, size = 200, replace = TRUE)
HR <- sample(0, size = 200, replace = TRUE)
HRV <- sample(0, size = 200, replace = TRUE)
eye_tracking <- sample(0, size = 200, replace = TRUE)
pupil_dilation <- sample(0, size = 200, replace = TRUE)
saliva_cortisol <- sample(0, size = 200, replace = TRUE)
corrugator <- sample(0:1, size = 200, replace = TRUE)
respiration <- sample(0:1, size = 200, replace = TRUE)
SCL <- sample(0:1, size = 200, replace = TRUE)
hair_cortisol <- sample(0, size = 200, replace = TRUE)
body_sway <- sample(0, size = 200, replace = TRUE)

# Set three instances with all variables as 1
HRV[1:7] <- 1
eye_tracking[c(1,5,8)] <- 1
pupil_dilation[1:2] <- 1
SCR[c(50:58)] <- 1
SCL[c(101,51,81)] <- 1
saliva_cortisol [100:106] <- 1
body_sway [1] = 1


# Create the data table
d <- data.table(
  transformation_procedure,
  SCR,
  EMG,
  HR,
  HRV,
  eye_tracking,
  pupil_dilation,
  saliva_cortisol,
  corrugator,
  respiration,
  SCL,
  hair_cortisol,
  body_sway
)



d_long <- melt(
  d,
  id.vars = "transformation_procedure",
  variable.name = "measure",
  value.name = "n"
)


# save content as tables
Content = d_long %>% 
  group_by(transformation_procedure, measure) %>% 
  count(transformation_procedure, measure) %>%
  ungroup()
pander::pander(Content)

df.long = reshape2::dcast(d_long, transformation_procedure ~ measure, 
                          value.var = "n",
                          fun.aggregate = sum)

df <- df.long %>% 
  mutate_at(c(2:13), as.numeric)
tet = apply(df[,c(2:13)], 2, function(x) ifelse(x >= 2, 2, x))
df = df %>% select(transformation_procedure)
d = cbind(df, tet)
#######


# Print the data table
print(d)
d = as.data.table(d)
class(d)
#### create plot
d[, S := factor(paste0("T",1:nrow(d)))] #Create symptom variable
d = reshape2::melt(d ,variable.name="measure", value.name="Type") #Transform to long format

#d = d %>% rename(S = content_agreement)
d = setDT(d)
d = d[Type >=1] #Keep the scales in which the symptoms are 1 (present) or 2 (included)
range(d$Type)
d[, Type := factor(Type, labels=c("Measure was transformed in one publication with this procedure", 
                                  "Measure was transformed in more than one publication with this procedure"))]
d = setDT(d)
d[, count := .N, by=S]

# Symptom order
sympt.order = d[, .N, by=S][order(N)][, S] #Replace by order
d[, S := factor(S, levels = sympt.order)]


# Scale order by frequency
scale.order = d[, .N, by=measure][order(N)][, measure]
d[, measure := factor(measure, levels = scale.order)]
d[, measure2 := as.numeric(measure)]


# for color blindeness
pal_small = c("#332288", "#CC6677" , "#AA4499", "#882255", "black", "#648FFF", "#C0C5C1")
pal_big = rev(c( "black","#332288", "#648FFF",  "#003f5c",  "#2f4b7c", "#665191", "#882255",
          "#CC6677" , "#a05195", "#AA4499", "#d45087", "#f95d6a", "#ff7c43", "#ffa600", 
          "yellow", "#C0C5C1"))
# https://projects.susielu.com/viz-palette?colors=[%22#ffd700%22,%22#ffb14e%22,%22#fa8775%22,%22#ea5f94%22,%22#cd34b5%22,%22#9d02d7%22,%22#0000ff%22,%22#000000%22]&backgroundColor=%22white%22&fontColor=%22black%22&mode=%22protanomaly%22
pal1 = rev(c("yellow","#ffd700",
        "#ffb14e",
        "#fa8775",
        "#ea5f94",
        "#cd34b5",
        "#9d02d7",
        "#665191",
        "#0000ff",
        "#648FFF", 
        "#2f4b7c",
        "#000000"))


arrN = 1:9 # how many transf. procedures

# Plot
a<- ggplot(d, aes(x=measure2, y=S, group=S, color=measure, shape=Type, rev=F)) +
  geom_line() + #keep this here, otherwise there is an error 
  xlab("") +
  ylab("") +
  # Generate the grid lines
  geom_hline(yintercept = 1:12, colour = "grey80", size = .2) + # 8 different measures
  geom_vline(xintercept = 1:9, colour = "grey80", size = .2) + # 16 different transformation_procedures
  # Points and lines
  geom_line(colour="grey60") +
  geom_point(size=3, fill="white") +
  # Fill the middle space with a white blank rectangle
  geom_rect(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=.6,fill="white", color=NA) +
  # Polar coordinates
  # coord_polar() +
  # geom_label(aes(label = count))+ 
  scale_shape_manual(values=c(124, 15), guide=guide_legend(override.aes=list(size=4))) +
  # The angle for the symptoms and remove the default grid lines
  theme(#axis.text.x = element_text(angle = 360/(2*pi)*rev( pi/2 + seq( pi/43, 2*pi-pi/43, len=43)) + c(rep(0, 21.5), rep(180,24))),
    #axis.text.x = element_markdown(colour = color_parent_item), 
    panel.border = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position="right",
    plot.margin = unit(rep(.5,4), "lines")) +
  labs(shape = "", color = "Outcome measure") + # remove legend title
  scale_x_continuous(limits=c(0,12), # how many measures
                     expand=c(0,0), 
                     breaks=1:10, # how many measures-1
                     labels=d[, levels(measure)]) +
  scale_y_discrete(labels = rev(arrN)) +
  scale_color_manual(values=pal1); a


t = d %>% select(S, transformation_procedure) %>% group_by(S, transformation_procedure)%>% unique()%>%
  ungroup() %>%
  mutate(index = row_number()) %>%
  select(2:3)

# Content = t %>% 
#   arrange(desc(S)) %>%
#   select(transformation_procedure) 

t = as.data.frame(t)
labels = ggplot() +
  # Your existing plot layers here
  geom_text(data = t, aes(x = 1, y = index, label = transformation_procedure),
            color = "black", size = 5, vjust = 0, position = position_dodge(width = 1))+
  theme(
    panel.background = element_rect(fill = "white"),
    axis.line.x = element_blank(),    # Hide x-axis line
    axis.line.y = element_blank(),    # Hide y-axis line
    axis.text.x = element_blank(),    # Hide x-axis text
    axis.ticks.x = element_blank(),   # Hide x-axis ticks
    axis.title.x = element_blank(),   # Hide x-axis title
    #axis.line.y = element_line(),     # Show y-axis line
    axis.text.y = element_text(margin = margin(r = 0, l = 50), 
                               size = 15, face = "bold"),  # Adjust margin to bring y-axis closer
    axis.ticks.y = element_blank(),    # hide y-axis ticks
    axis.title.y = element_blank()     # hide y-axis title
  )+ theme_void()+
  scale_y_reverse(breaks =c(1:7))
labels


library(patchwork)
labels + a + plot_layout(heights = c(7, 12), ncol = 2)
ggsave(plot = last_plot(), file = "./figures/dot_plot_simulation.jpeg",
       height= 10, width = 12)

