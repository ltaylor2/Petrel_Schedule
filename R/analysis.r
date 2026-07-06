############################################################
### Logistics
#########################################################

# Required packages
library(tidyverse)
library(patchwork)
library(gghalves)

# Read in processed data
read_processed_dat <- function(f) {
  read_csv(f, show_col_types=FALSE) |>
  mutate(Strategy_F = paste(Min_Energy_Thresh_F, Max_Energy_Thresh_F, sep="-"),
         Strategy_M = paste(Min_Energy_Thresh_M, Max_Energy_Thresh_M, sep="-"),
         Strategy_Combination = paste(Strategy_F, Strategy_M, sep=" : "),
         Is_Empirical_Strategy = (Min_Energy_Thresh_F >= 400 & Min_Energy_Thresh_F <= 700) &
                                 (Max_Energy_Thresh_F >= 700 & Max_Energy_Thresh_F <= 900) &
                                 (Min_Energy_Thresh_M >= 400 & Min_Energy_Thresh_M <= 700) &
                                 (Max_Energy_Thresh_M >= 700 & Max_Energy_Thresh_M <= 900))
}

dat_regular <- read_processed_dat("Output/processed_regular.csv")

# Get numerical order for factoring strategies
order_strategy_f <- dat_regular |>
                 arrange(Min_Energy_Thresh_F, Max_Energy_Thresh_F) |>
                 pull(Strategy_F) |>
                 unique()
order_strategy_m <- dat_regular |>
                 arrange(Min_Energy_Thresh_M, Max_Energy_Thresh_M) |>
                 pull(Strategy_M) |>
                 unique()
dat_regular <- dat_regular |>
            mutate(Strategy_F = factor(Strategy_F, levels=order_strategy_f),
                   Strategy_M = factor(Strategy_M, levels=order_strategy_m))

# Custom plotting theme
theme_lt <- theme_bw() +
         theme(plot.title = element_text(size=12, hjust=0.5),
               axis.title = element_text(size=11),
               axis.text = element_text(size=8),
               legend.title = element_text(size=11),
               legend.text = element_text(size=8),
               panel.grid = element_blank())

EMPIRICAL_COLOR <- "#f275ee"

OF <- "Output/results_log.txt"

###############################################################
### Model fidelity
############################################################

# Empirical parameter set only, including empirical parameters for the environment, egg,
# and the range of empirical parent strategies
emp <- dat_regular |>
    filter(Is_Empirical_Strategy, Foraging_Condition_Mean == 162, Foraging_Condition_SD == 47)

# Function to quickly summarize a vector of numeric values for report printing
summaryValues <- function(v, sfs=4) {
    return(paste0("Mean=", round(mean(v, na.rm=TRUE),sfs), 
                  " SD=", round(sd(v, na.rm=TRUE),sfs), 
                  " Min=", round(min(v, na.rm=TRUE),sfs), 
                  " Max=", round(max(v, na.rm=TRUE), sfs))) 
}

# Print key empirical results to report for model fidelity comparisons
cat("Results log from", as.character(now()), "\n", file=OF, append=FALSE)
cat("\nModel fidelity for empirical strategies in empirical environment\n", file=OF, append=TRUE)
cat("Foraging conditions ", unique(emp$Foraging_Condition_Mean), "+-", unique(emp$Foraging_Condition_SD), "\n", file=OF, append=TRUE)
cat("Departure thresholds", unique(c(emp$Min_Energy_Thresh_F, emp$Min_Energy_Thresh_M)), "\n", file=OF, append=TRUE)
cat("Return thresholds", unique(c(emp$Max_Energy_Thresh_F, emp$Max_Energy_Thresh_M)), "\n", file=OF, append=TRUE)
cat("Success rate", summaryValues(emp$Rate_Success), "\n", file=OF, append=TRUE)
cat("Success hatch date", summaryValues(emp$Successful_Hatch_Date), "\n", file=OF, append=TRUE)
cat("Success proportion neglect", summaryValues(emp$Successful_Prop_Neglect), "\n", file=OF, append=TRUE)
cat("Mean success incubation bout (F)", summaryValues(emp$Mean_Incubation_Bout_F), "\n", file=OF, append=TRUE)
cat("Mean success incubation bout (F Trimmed)", summaryValues(emp$Mean_Incubation_Bout_F_Trimmed), "\n", file=OF, append=TRUE)
cat("Mean success incubation bout (M)", summaryValues(emp$Mean_Incubation_Bout_M), "\n", file=OF, append=TRUE)
cat("Mean success incubation bout (M Trimmed)", summaryValues(emp$Mean_Incubation_Bout_M_Trimmed), "\n", file=OF, append=TRUE)
cat("Mean success foraging bout (F)", summaryValues(emp$Mean_Foraging_Bout_F), "\n", file=OF, append=TRUE)
cat("Mean success foraging bout (F Trimmed)", summaryValues(emp$Mean_Foraging_Bout_F_Trimmed), "\n", file=OF, append=TRUE)
cat("Mean success foraging bout (M)", summaryValues(emp$Mean_Foraging_Bout_M), "\n", file=OF, append=TRUE)
cat("Mean success foraging bout (M Trimmed)", summaryValues(emp$Mean_Foraging_Bout_M_Trimmed), "\n", file=OF, append=TRUE)
cat("Mean success incubation bout (Both)", summaryValues(emp$Mean_Incubation_Bout_Both), "\n", file=OF, append=TRUE)
cat("Mean success incubation bout (Both Trimmed)", summaryValues(emp$Mean_Incubation_Bout_Both_Trimmed), "\n", file=OF, append=TRUE)
cat("Mean success foraging bout (Both)", summaryValues(emp$Mean_Foraging_Bout_Both), "\n", file=OF, append=TRUE)
cat("Mean success foraging bout (Both Trimmed)", summaryValues(emp$Mean_Foraging_Bout_Both_Trimmed), "\n", file=OF, append=TRUE)

###############################################################
### Example schedule visualizations under empirical parameters
############################################################

# Schedules saved from successful simulations and empirical parameters only
example_schedules <- read_csv("Output/processed_schedules_empirical.csv")

# Pivot compressed schedule strings to one-state-per-day-per-row for plotting
scheds_for_plot <- select(example_schedules, Season_History) |>
                separate(Season_History, sep="", into=as.character(c(1:max(nchar(example_schedules$Season_History)+1)))) |>
                mutate(I = row_number()) |>
                pivot_longer(cols=-I, names_to="Day", values_to="State") |>
                mutate(Day = as.numeric(Day)) |>
                filter(!is.na(State) & State != "")
  
# Tile plot of incubation attendance schedules for successful seasons
plot_schedule_examples <- ggplot(scheds_for_plot) +
                       geom_raster(aes(x=Day, y=I, fill=State)) +
                       scale_fill_manual(values=c("F"="black", "M"="lightgray", "N"="pink"),
                                         labels=c("F"="Female", "M"="Male", "N"="Unattended")) +
                       theme_lt +
                       xlab("Day of incubation") +
                       ylab("Simulation") +
                       theme(panel.background=element_blank(),
                             panel.grid=element_blank(),
                             axis.text.y=element_blank(),
                             axis.ticks.y=element_blank(),
                             legend.position="top")
ggsave(filename="Plots/FIGURE_S_SCHEDULE_EXAMPLES.png", plot=plot_schedule_examples,
       width=3, height=8, unit="in")

###############################################################
### Effects of starting energy
############################################################

# Data from starting energy runs, which includes only empirical parameters
# across a range of season starting energy values
dat_startEnergy <- read_processed_dat("Output/processed_startEnergy.csv")

# Print starting energy effects on mean incubation bout length  to report
cat("\nSeason starting energy effects:", file=OF, append=TRUE)
sink(OF, append=TRUE)
dat_startEnergy |>
  group_by(Start_Energy) |>
  summarize(Mean_Incubation_Bout_Both_Trimmed = mean(Mean_Incubation_Bout_Both_Trimmed)) |>
  print()
sink()

# Line plot of changing success rate as starting energy increases
plot_startEnergy_hatch <- ggplot(dat_startEnergy) +
                       geom_line(aes(x=Start_Energy, y=Rate_Success, 
                                     group=Strategy_Combination),
                                 colour=EMPIRICAL_COLOR, alpha=0.5) +
                       geom_smooth(aes(x=Start_Energy, y=Rate_Success),
                                   method="loess", formula=y~x,
                                   colour="#b404a2", se=FALSE) +  
                       geom_vline(xintercept=766, colour="black", linetype="dashed") +
                       scale_x_continuous(breaks=seq(300, 1300, by=100)) +
                       scale_y_continuous(breaks=seq(0, 1, by=0.25)) +
                       xlab("Season starting energy (kJ)") +  
                       ylab("Success rate") +
                       theme_lt

# Line plot of changing mean incubation bout length as starting energy increases
plot_startEnergy_bouts <- ggplot(dat_startEnergy) +
                       geom_line(aes(x=Start_Energy, y=Mean_Incubation_Bout_Both_Trimmed, 
                                     group=Strategy_Combination),
                                 colour=EMPIRICAL_COLOR, alpha=0.5) +
                       geom_smooth(aes(x=Start_Energy, y=Mean_Incubation_Bout_Both_Trimmed),
                                   method="loess", formula=y~x,
                                   colour="#b404a2", se=FALSE) +
                       geom_hline(yintercept=2.5, colour="black", linetype="dashed") +
                       geom_hline(yintercept=3.5, colour="black", linetype="dashed") +
                       annotate(geom="rect", xmin=300, xmax=1300, ymin=2.5, ymax=3.5, fill="black", alpha=0.2) +
                       geom_vline(xintercept=766, colour="black", linetype="dashed") +  
                       scale_x_continuous(limits=c(300, 1300), breaks=seq(300, 1300, by=100)) +
                       scale_y_continuous(limits=c(1.9, 10), breaks=seq(2, 10, by=2)) +
                       xlab("Season starting energy (kJ)") +  
                       ylab("Incubation bout length (days)") +
                       theme_lt

# Combine starting energy plots and save
plots_startEnergy <- plot_startEnergy_hatch / plot_startEnergy_bouts +
                  plot_annotation(tag_levels="A", tag_prefix="(", tag_suffix=")") + 
                  plot_layout(ncol=1, nrow=2, axes="collect")
ggsave(filename="Plots/FIGURE_S_STARTENERGY.png", plot=plots_startEnergy, 
       width=6, height=5, unit="in")

###############################################################
### Sex bias 
############################################################

# Data from egg cost runs, which includes only empirical parameters
# across a range of energetic costs that the female pays for laying the egg
dat_eggCost <- read_processed_dat("Output/processed_eggCost.csv")

# Line plot of changing success rate as egg cost increases 
plot_eggCost_hatch <- ggplot(dat_eggCost) +
                   geom_line(aes(x=Egg_Cost, y=Rate_Success, 
                                 group=Strategy_Combination),
                             colour=EMPIRICAL_COLOR, alpha=0.5) +
                   geom_smooth(aes(x=Egg_Cost, y=Rate_Success),
                               method="loess", formula=y~x,
                               colour="#b404a2", se=FALSE) +
                   scale_x_continuous(breaks=seq(0, 500, by=100)) +
                   xlab("Egg cost to female (kJ)") +
                   ylab("Success rate") +
                   theme_lt

# Calculating mean incubation bout lengths across empirical parent strategy combinations 
dat_eggCost_long <- dat_eggCost |>
                select(Strategy_Combination, Egg_Cost, Mean_Incubation_Bout_F_Trimmed, Mean_Incubation_Bout_M_Trimmed) |>
                pivot_longer(cols=contains("Bout"), names_to="Sex", values_to="Mean_Incubation_Bout") |>
                mutate(Sex = str_split_i(Sex, "_", 4))
eggCost_means <- dat_eggCost_long |>
              group_by(Egg_Cost, Sex) |>
              summarize(Mean_Incubation_Bout = mean(Mean_Incubation_Bout), .groups="drop_last")

# Split violin plot showing mean incubation bout lengths for males vs. females, distributed across
# parent strategy combinations
plot_eggCost_bias <- ggplot() +
                  geom_half_violin(data=filter(dat_eggCost_long, Sex == "F"), 
                                    aes(x=Egg_Cost, y=Mean_Incubation_Bout, group=Egg_Cost, fill=Sex),
                                    colour="black", side="l") +
                  geom_half_violin(data=filter(dat_eggCost_long, Sex == "M"), 
                                    aes(x=Egg_Cost, y=Mean_Incubation_Bout, group=Egg_Cost, fill=Sex),
                                    colour="black", side="r") +
                  geom_point(data=filter(eggCost_means, Sex == "F"),
                             aes(x=Egg_Cost, y=Mean_Incubation_Bout),
                             colour="black", size=0.5, 
                             position=position_nudge(x=-11)) +
                  geom_point(data=filter(eggCost_means, Sex == "M"),
                             aes(x=Egg_Cost, y=Mean_Incubation_Bout),
                             colour="black", size=0.5,
                             position=position_nudge(x=11)) +
                  scale_x_continuous(breaks=seq(0, 500, by=100)) +
                  scale_y_continuous(breaks=seq(1, 9, by=2)) +
                  scale_fill_manual(values=c("F"="white", "M"="gray"),
                                    labels=c("F"="Female", "M"="Male")) +
                  xlab("Egg cost to female (kJ)") +
                  ylab("Mean incubation bout (days)") +
                  theme_lt +
                  theme(legend.position="right", 
                        legend.title=element_text(size=10),
                        legend.text=element_text(size=10))

# Combine egg cost plots and save
plots_eggCost <- plot_eggCost_hatch / plot_eggCost_bias +
              plot_annotation(tag_levels="A", tag_prefix="(", tag_suffix=")") + 
              plot_layout(ncol=1, nrow=2, axes="collect")
ggsave(filename="Plots/FIGURE_S_EGGCOST.png", plot=plots_eggCost, 
       width=6, height=5, unit="in")

# Data from sex order run, where we flip males to start incubating first under
# otherwise empirical parameters for the environment, egg, and range of empirical
# parent strategies
dat_swappedSexOrder <- read_processed_dat("Output/processed_swapSexOrder.csv")

# Pivot long to get seperate male and female incubation bout averages for each strategy combination
dat_swappedSexOrder_long <- dat_swappedSexOrder |>
                         select(Strategy_Combination, Mean_Incubation_Bout_F_Trimmed, Mean_Incubation_Bout_M_Trimmed) |>
                         pivot_longer(cols=contains("Bout"), names_to="Sex", values_to="Mean_Incubation_Bout") |>
                         mutate(Sex = str_split_i(Sex, "_", 4))

# Do the same pivot (including filtering to just those empirical parameters) for the regular data
# which has that initial starting order of females incubating first
dat_regular_comparison_long <- dat_regular |>
                            filter(Is_Empirical_Strategy, Foraging_Condition_Mean == 162, Foraging_Condition_SD == 47) |>
                            select(Strategy_Combination, Mean_Incubation_Bout_F_Trimmed, Mean_Incubation_Bout_M_Trimmed) |>
                                              pivot_longer(cols=contains("Bout"), names_to="Sex", values_to="Mean_Incubation_Bout") |>
                            mutate(Sex = str_split_i(Sex, "_", 4))

# Print averages for regular and swapped orders to report
cat("\n\nRegular order incubation bout F ", summaryValues(filter(dat_regular_comparison_long, Sex=="F")$Mean_Incubation_Bout), "\n", file=OF, append=TRUE)
cat("Regular order incubation bout M ", summaryValues(filter(dat_regular_comparison_long, Sex=="M")$Mean_Incubation_Bout), "\n", file=OF, append=TRUE)
cat("\nSwapped order incubation bout F ", summaryValues(filter(dat_swappedSexOrder_long, Sex=="F")$Mean_Incubation_Bout), "\n", file=OF, append=TRUE)
cat("Swapped order incubation bout M ", summaryValues(filter(dat_swappedSexOrder_long, Sex=="M")$Mean_Incubation_Bout), "\n", file=OF, append=TRUE)

# Boxplot of mean incubation bout lengths for males vs. females in the normal starting order 
plot_sexBias_regular <- ggplot(dat_regular_comparison_long) +
                     geom_boxplot(aes(x=Sex, y=Mean_Incubation_Bout, fill=Sex)) +
                     scale_x_discrete(labels=c("F"="Female", "M"="Male")) +   
                     scale_fill_manual(values=c("F"="white", "M"="gray"),
                                       labels=c("F"="Female", "M"="Male"),
                                       guide="none") +
                     xlab("Sex") +
                     ylab("Mean incubation bout (days)") +
                     ggtitle("Regular starting order\n(females incubate first)") +
                     theme_lt

# Boxplot of mean incubation bout lengths for males vs. females in the swapped starting order 
plot_sexBias_swapped <- ggplot(dat_swappedSexOrder_long) +
                     geom_boxplot(aes(x=Sex, y=Mean_Incubation_Bout, fill=Sex)) +
                     scale_x_discrete(labels=c("F"="Female", "M"="Male")) +
                     scale_fill_manual(values=c("F"="white", "M"="gray"),
                                       labels=c("F"="Female", "M"="Male"),
                                       guide="none") +
                     xlab("Sex") +
                     ylab("Mean incubation bout (days)") +
                     ggtitle("Swapped starting order\n(males incubate first)") +
                     theme_lt 

# Combine regular vs. swapped sex order boxplots and save
plots_sexBias_swapped <- plot_sexBias_regular + plot_sexBias_swapped +
                      plot_annotation(tag_levels="A", tag_prefix="(", tag_suffix=")") + 
                      plot_layout(ncol=2, nrow=1, axes="collect") &
                      theme(plot.title=element_text(size=10))
ggsave(filename="Plots/FIGURE_S_SEXBIAS.png", plot=plots_sexBias_swapped, 
       width=4.5, height=3, unit="in")

###############################################################
### Parent strategies
############################################################

# Data from empirical environment only, but across the range of empirical strategies
emp_environment <- filter(dat_regular, Foraging_Condition_Mean == 162, Foraging_Condition_SD == 47)

# Print parent strategy success range to report
cat("\nParent strategy combinations:", file=OF, append=TRUE)
cat("\nMin = ", min(emp_environment$Rate_Success), "Max = ", max(emp_environment$Rate_Success), file=OF, append=TRUE)
cat("\nQuantiles:", quantile(emp_environment$Rate_Success, c(0.05, 0.95)), file=OF, append=TRUE)

# Histogram of overall success rates under empirical environment and egg conditions
plot_success_distribution <- ggplot(emp_environment) +
                          geom_histogram(aes(x=Rate_Success, fill=Is_Empirical_Strategy), 
                                         binwidth=0.025, colour="gray40") +
                          scale_x_continuous(limits=c(0,1), breaks=seq(0, 1, by=0.1), oob=scales::oob_keep) +
                          scale_fill_manual(values=c("FALSE"="gray70", "TRUE"=EMPIRICAL_COLOR)) +
                          xlab("Success rate") +
                          ylab("No. parent strategy combinations") +
                          guides(fill="none") +
                          theme_lt

# Save plot
ggsave(filename="Plots/FIGURE_S_SUCCESS_DISTRIBUTION.png", plot=plot_success_distribution,
       width=5, height=3)

# Empirical strategy boxes to highlight those places in the tile
# We factor to align with axes in the tile plot and shift boundaries to not run through center of boxes 
emp_strategy_boxes <- tibble(Strategy_F_Start = c(rep("400-700", times=4), rep("500-700", times=4), rep("600-700", times=4), rep("700-800", times=4)),
                             Strategy_F_End = c(rep("400-900", times=4), rep("500-900", times=4), rep("600-900", times=4), rep("700-900", times=4)),
                             Strategy_M_Start = c(rep(c("400-700", "500-700", "600-700", "700-800"), times=4)),
                             Strategy_M_End = c(rep(c("400-900", "500-900", "600-900", "700-900"), times=4))) |>
                   mutate(Strategy_F_Start = as.numeric(factor(Strategy_F_Start, order_strategy_f))-0.5,
                          Strategy_F_End = as.numeric(factor(Strategy_F_End, order_strategy_f))+0.5,
                          Strategy_M_Start = as.numeric(factor(Strategy_M_Start, order_strategy_m))-0.5,
                          Strategy_M_End = as.numeric(factor(Strategy_M_End, order_strategy_m))+0.5)

# Tile plot from empirical environment
plot_main_tile <- ggplot(emp_environment) +
               geom_tile(aes(x=Strategy_F, y=Strategy_M, fill=Rate_Success)) +
               geom_rect(data=emp_strategy_boxes,
                         aes(xmin=Strategy_F_Start, xmax=Strategy_F_End, 
                             ymin=Strategy_M_Start, ymax=Strategy_M_End),
                         fill="transparent", color=alpha(EMPIRICAL_COLOR,0.5)) +
               scale_fill_continuous(low="white", high="gray10", name="Success rate",
                                     limits=c(0, 1)) +
               xlab("Female strategy") +
               ylab("Male strategy") +
               theme_lt +
               theme(panel.background = element_blank(),
                     axis.text.y = element_text(size=4),
                     axis.text.x = element_text(size=4, angle=-90, hjust=0, vjust=1))

# Summarize across minimum thresholds
min_threshes <- emp_environment |>
             select(Min_Energy_Thresh_F, Min_Energy_Thresh_M, Rate_Success) |>
             pivot_longer(cols=contains("Thresh"), names_to="Sex", values_to="Min_Energy_Thresh") |>
             group_by(Min_Energy_Thresh) |>
             mutate(Mean_Rate_Success = mean(Rate_Success))

# Violin plot of distributions of success rates for different min (departure) thresholds
# Shows the distribution across all possible combinations of a min (departure) threshold with
# the max (return) threshold for the same bird and both parameters for the partner
plot_min_threshes <- ggplot(min_threshes,
                            aes(x=Min_Energy_Thresh, y=Rate_Success)) +
                  geom_violin(aes(group=Min_Energy_Thresh, fill=Mean_Rate_Success),
                              scale="area",
                              colour="black", linewidth=0.05) +
                  stat_summary(fun=mean, geom="line", colour="black", linewidth=0.5) +
                  scale_x_continuous(breaks=seq(200, 11000, by=200)) +
                  scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, by=0.25)) +
                  scale_fill_continuous(low="white", high="gray10", name="Success rate",
                                        limits=c(0, 1)) +    
                  xlab("Departure threshold (kJ)") +
                  ylab("Success rate") +
                  theme_lt

# Summarize across maximum thresholds
max_threshes <- emp_environment |>
             select(Max_Energy_Thresh_F, Max_Energy_Thresh_M, Rate_Success) |>
             pivot_longer(cols=contains("Thresh"), names_to="Sex", values_to="Max_Energy_Thresh") |>
             group_by(Max_Energy_Thresh) |>
             mutate(Mean_Rate_Success = mean(Rate_Success))

# Violin plot of distributions of success rates for different max (return) thresholds
# Shows the distribution across all possible combinations of a max (return) threshold with
# the min (departure) threshold for the same bird and both parameters for the partner
plot_max_threshes <- ggplot(max_threshes,
                            aes(x=Max_Energy_Thresh, y=Rate_Success)) +
                  geom_violin(aes(group=Max_Energy_Thresh, fill=Mean_Rate_Success),
                              scale="area",
                              colour="black", linewidth=0.05) +
                  stat_summary(fun=mean, geom="line", colour="black", linewidth=0.5) +
                  scale_x_continuous(breaks=seq(400, 1200, by=200)) +
                  scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, by=0.25)) +  
                  scale_fill_continuous(low="white", high="gray10", name="Success rate",
                                        limits=c(0, 1)) +    
                  xlab("Return threshold (kJ)") +
                  ylab("Success rate") +
                  theme_lt

# Assemble full with tile plots and save
design <- "12
           13"
plot_tiles <- (plot_main_tile + labs(tag="(A)")) + 
              guide_area() + 
              ((plot_min_threshes + labs(tag="(B)")) / plot_max_threshes + 
               plot_layout(axes="collect", heights=c(1,1))) + 
              plot_layout(guides="collect", design=design, 
                          widths=c(1, 0.5), heights=c(1, 4)) &
              theme(legend.position = "bottom", legend.title.position = "top",
                    legend.title = element_text(size=11, hjust=0.5),
                    plot.tag.position = "topleft")
ggsave(filename="Plots/FIGURE_2.png", plot=plot_tiles,
       width=6.5, height=4, unit="in")

###############################################################
### Tradeoffs
############################################################
 
# Parent Energy ~ Hatch Rate

# Convex hull for empirical strategies
emp_hull_energies <- emp_environment |>
                  filter(Is_Empirical_Strategy) |>
                  slice(chull(Rate_Success, Successful_Mean_Energy_F))

# Calculate success rate bins to compare to plot with secondary metrics
success_bins <- emp_environment |>
              mutate(Success_Bin = round(Rate_Success, 1)) |>
              group_by(Success_Bin) |>
              summarize(Mean_Parent_Energy = mean(Successful_Mean_Energy_F),
                        SD_Parent_Energy = sd(Successful_Mean_Energy_F),
                        Mean_Hatch_Date = mean(Successful_Hatch_Date),
                        SD_Hatch_Date = sd(Successful_Hatch_Date)) 

# Point plot of tradeoff between success rate and parent energy
# including parent energy averages across success rate bins 
plot_tradeoff_energy <- ggplot() +
                     geom_point(data=filter(emp_environment, !Is_Empirical_Strategy),
                                aes(x=Rate_Success, y=Successful_Mean_Energy_F),
                                colour = "lightgray", size=0.8, alpha=0.1) +
                     geom_point(data=filter(emp_environment, Is_Empirical_Strategy),
                                aes(x=Rate_Success, y=Successful_Mean_Energy_F),
                                colour = EMPIRICAL_COLOR, size=0.8, alpha=0.3) +
                     geom_polygon(data=emp_hull_energies,
                                  aes(x=Rate_Success, y=Successful_Mean_Energy_F),
                                  colour = EMPIRICAL_COLOR, fill="transparent",
                                  linewidth=0.3) +
                     geom_errorbar(data=success_bins,
                                   aes(x=Success_Bin, ymin=Mean_Parent_Energy-SD_Parent_Energy, ymax=Mean_Parent_Energy+SD_Parent_Energy),
                                   colour="black",
                                   linewidth=0.5, width=0) +  
                     geom_point(data=success_bins,
                                aes(x=Success_Bin, y=Mean_Parent_Energy), colour="black",
                                size=1) +
                     scale_x_continuous(limits=c(0, 1), breaks=seq(0, 1.0, by=0.25)) +
                     scale_y_continuous(breaks=seq(400, 1200, by=200)) +
                     guides(colour="none") +
                     xlab("Success rate") +
                     ylab("Female energy (kJ)") +
                     theme_lt

# Hatch Date ~ Hatch Rate

# Convex hull for empirical strategies
emp_hull_hatchdate <- emp_environment |>
                   filter(Is_Empirical_Strategy) |>
                   slice(chull(Rate_Success, Successful_Hatch_Date))

# Point plot of tradeoff between success rate and hatch date, which scales with egg neglect
# including hatch date averages across success rate bins 
plot_tradeoff_date <- ggplot() +
                   geom_point(data=filter(emp_environment, !Is_Empirical_Strategy),
                              aes(x=Rate_Success, y=Successful_Hatch_Date),
                              colour = "lightgray", size=0.8, alpha=0.1) +
                   geom_point(data=filter(emp_environment, Is_Empirical_Strategy),
                              aes(x=Rate_Success, y=Successful_Hatch_Date),
                              colour = EMPIRICAL_COLOR, size=0.8, alpha=0.3) +
                   geom_polygon(data=emp_hull_hatchdate,
                                aes(x=Rate_Success, y=Successful_Hatch_Date),
                                colour = EMPIRICAL_COLOR, fill="transparent",
                                linewidth=0.3) +
                   geom_errorbar(data=success_bins,
                                 aes(x=Success_Bin, ymin=Mean_Hatch_Date-SD_Hatch_Date, ymax=Mean_Hatch_Date+SD_Hatch_Date),
                                 colour="black",
                                 linewidth=0.5, width=0) +  
                   geom_point(data=success_bins,
                              aes(x=Success_Bin, y=Mean_Hatch_Date), colour="black",
                              size=1) +  
                   scale_x_continuous(limits=c(0, 1), breaks=seq(0, 1.0, by=0.25)) +
                   scale_y_continuous(limits=c(36, 58), breaks=seq(36, 58, by=4)) +
                   guides(colour="none") +
                   xlab("Success rate") +
                   ylab("Hatch date (days)") +
                   theme_lt

# Parent Energy ~ Hatch Date

# Convex hull for empirical strategies
emp_hull_energydate <- emp_environment |>
                     filter(Is_Empirical_Strategy) |>
                     slice(chull(Successful_Mean_Energy_F, Successful_Hatch_Date))

# Now we are calculating bins by parent energy, rather than by hatch date
energy_hatch_bins <- emp_environment |>
                  mutate(Energy_Bin = round(Successful_Mean_Energy_F / 100) * 100) |>
                  group_by(Energy_Bin) |>
                  summarize(Mean_Hatch_Date = mean(Successful_Hatch_Date),
                            SD_Hatch_Date = sd(Successful_Hatch_Date))

# Point plot of tradeoff between parent energy and hatch date
# including hatch date averages across parent energy bins 
plot_tradeoff_energydate <- ggplot() +
                         geom_point(data=filter(emp_environment, !Is_Empirical_Strategy),
                                    aes(x=Successful_Mean_Energy_F, y=Successful_Hatch_Date),
                                    colour = "lightgray", size=0.8, alpha=0.1) +
                         geom_point(data=filter(emp_environment, Is_Empirical_Strategy),
                                    aes(x=Successful_Mean_Energy_F, y=Successful_Hatch_Date),
                                    colour=EMPIRICAL_COLOR, size=0.8, alpha=0.3) +
                         geom_polygon(data=emp_hull_energydate,
                                      aes(x=Successful_Mean_Energy_F, y=Successful_Hatch_Date),
                                      colour=EMPIRICAL_COLOR, fill="transparent",
                                      linewidth=0.5) +
                         geom_errorbar(data=energy_hatch_bins,
                                       aes(x=Energy_Bin, ymin=Mean_Hatch_Date-SD_Hatch_Date, ymax=Mean_Hatch_Date+SD_Hatch_Date),
                                       colour="black",
                                       linewidth=0.5, width=0) +  
                         geom_point(data=energy_hatch_bins,
                                    aes(x=Energy_Bin, y=Mean_Hatch_Date), colour="black",
                                    size=1) +
                         scale_x_continuous(breaks=seq(400, 1200, by=200)) +
                         scale_y_continuous(limits=c(36, 58), breaks=seq(36, 58, by=4)) +
                         guides(colour="none") +
                         xlab("Female energy (kJ)") +
                         ylab("Hatch date (days)") +  
                         theme_lt

# Assemble and save full tradeoffs plot 
plot_tradeoffs <- plot_tradeoff_energy + plot_tradeoff_date + plot_tradeoff_energydate +
               plot_annotation(tag_levels="A", tag_prefix="(", tag_suffix=")") &
               theme(legend.position = "bottom", legend.title.position = "top",
                     legend.title = element_text(size=11, hjust=0.5),
                     plot.tag.position="topleft",
                     plot.tag=element_text(vjust=2),
                     plot.margin = margin(t=14, r=0, b=0, l=0))
ggsave(filename="Plots/FIGURE_3.png", plot=plot_tradeoffs, 
       width=6.5, height=2.3)

# Print representative range of hatch dates for binned average parent energies for report
cat("\n\nAverage hatch date for 500 kJ parent energy strategy combinations. Mean = ", 
    round(filter(energy_hatch_bins, Energy_Bin == 500)$Mean_Hatch_Date),
    "  SD = ",
    round(filter(energy_hatch_bins, Energy_Bin == 500)$SD_Hatch_Date),
    file=OF, append=TRUE)
cat("\nAverage hatch date for 1000 kJ parent energy strategy combinations. Mean = ", 
    round(filter(energy_hatch_bins, Energy_Bin == 1000)$Mean_Hatch_Date),
    "  SD = ",
    round(filter(energy_hatch_bins, Energy_Bin == 1000)$SD_Hatch_Date),
    file=OF, append=TRUE)


###############################################################
### How empirical strategies relatively manage the tradeoff between parent energy and attendance 
############################################################

# Calculate the average success across all empirical strategies under otherwise empirical parameters
average_empirical_success <- emp_environment |>
                          filter(Is_Empirical_Strategy) |>
                          pull(Rate_Success) |>
                          mean()

# Split the NON-EMPIRICAL parent strategies (under otherwise empirical parameters) into two groups:
# The first group is parent strategy combinations with lower success rates than the mean empirical strategy success rate
# The second group is parent strategy combinations with higher success rates than the mean empirical strategy success rate 
# We can now use those three groups -- Empirical, Worse than Empirical, and Better than Empirical, to show where
# empirical strategies fall in secondary metrics (i.e., parent energy, hatch date) 
comp_to_emp <- emp_environment |>
            mutate(Compared_to_Emp = case_when(Is_Empirical_Strategy ~ "Empirical",
                                               !Is_Empirical_Strategy & Rate_Success < average_empirical_success ~ "Worse",
                                               !Is_Empirical_Strategy & Rate_Success > average_empirical_success ~ "Better")) |>
            filter(!is.na(Compared_to_Emp))

# Boxplot of parent energy for empirical, worse-than-empirical, and better-than-empirical strategies
plot_comp_emp_energy <- ggplot(comp_to_emp) +
                    geom_boxplot(aes(x=Compared_to_Emp, y=Successful_Mean_Energy_F, fill=Compared_to_Emp)) +
                    xlab("Success compared to empirical average") +
                    ylab("Female energy (kJ)") +
                    scale_x_discrete(limits=c("Worse", "Empirical", "Better")) +
                    scale_fill_manual(values=c("Worse"="lightgray", "Empirical" = EMPIRICAL_COLOR, "Better"="white"),
                                      guide="none") +
                    theme_lt

# Boxplot of hatch date for empirical, worse-than-empirical, and better-than-empirical strategies
plot_comp_emp_date <- ggplot(comp_to_emp) +
                    geom_boxplot(aes(x=Compared_to_Emp, y=Successful_Hatch_Date, fill=Compared_to_Emp)) +
                    xlab("Success compared to empirical average") +
                    ylab("Hatch date (days)") +
                    scale_x_discrete(limits=c("Worse", "Empirical", "Better")) +
                    scale_fill_manual(values=c("Worse"="lightgray", "Empirical" = EMPIRICAL_COLOR, "Better"="white"),
                                      guide="none") +
                    theme_lt

# Assemble empirical vs. worse vs. better comparisons and save
plots_comp_emp <- plot_comp_emp_energy + plot_comp_emp_date +
               plot_layout(nrow=1, ncol=2, axe="collect") +
               plot_annotation(tag_levels="A", tag_prefix="(", tag_suffix=")")
ggsave(filename="Plots/FIGURE_S_COMP_EMP.png", plot=plots_comp_emp,
       width=5, height=2.5, unit="in")

###############################################################
### Decline in hatch success in the environment
############################################################

# Exclude data from empirical environment, so we have smooth sample of 
#   130 - 170 by 10 (including 160)
not_emp_forgmean <- filter(dat_regular, Foraging_Condition_Mean != 162, Foraging_Condition_SD == 47)

# Fit logistic curves to find fail point
mlog_all <- glm(Rate_Success ~ Foraging_Condition_Mean, data=not_emp_forgmean, family="quasibinomial")
mlog_all_failpoint <- -coef(mlog_all)[1] / coef(mlog_all)[2]
mlog_emp <- glm(Rate_Success ~ Foraging_Condition_Mean, data=filter(not_emp_forgmean, Is_Empirical_Strategy), family="quasibinomial")
mlog_emp_failpoint <- -coef(mlog_emp)[1] / coef(mlog_emp)[2]

# Line plot of hatch rate as environment degrades
plot_decline_hatch_mean <- ggplot() +
                        geom_line(data=filter(not_emp_forgmean, !Is_Empirical_Strategy),
                                  aes(x=Foraging_Condition_Mean, y=Rate_Success, group=Strategy_Combination),
                                  colour="lightgray", alpha=0.5, linewidth=0.35) +
                        geom_line(data=filter(not_emp_forgmean, Is_Empirical_Strategy),
                                  aes(x=Foraging_Condition_Mean, y=Rate_Success, group=Strategy_Combination),
                                  colour=EMPIRICAL_COLOR, alpha=0.15, linewidth=0.1) +
                        geom_vline(xintercept = mlog_all_failpoint, colour="black", linewidth=0.25) +
                        geom_vline(xintercept = mlog_emp_failpoint, colour="#b404a2", linewidth=0.25) +
                        stat_smooth(data=filter(not_emp_forgmean, Is_Empirical_Strategy),
                                    aes(x=Foraging_Condition_Mean, y=Rate_Success), 
                                    method = "glm", method.args = list(family="quasibinomial"), formula=y~x,
                                    se=FALSE, colour="#b404a2") +
                        stat_smooth(data=not_emp_forgmean,
                                    aes(x=Foraging_Condition_Mean, y=Rate_Success), 
                                    method = "glm", method.args = list(family="quasibinomial"), formula=y~x, 
                                    se=FALSE, colour="black") +
                        xlab("Foraging mean (kJ/day)") +
                        ylab("Success rate") +
                        theme_lt

# Summarize mean and variance in success rates across not empirical conditions
# for smooth sampling of foraging condition mean and SD sampling
not_emp_both_summaries <- filter(dat_regular, 
                                 Foraging_Condition_Mean != 162,    
                                 Foraging_Condition_SD != 47) |>
                       group_by(Foraging_Condition_Mean, Foraging_Condition_SD) |>
                       summarize(Mean_Rate_Success = mean(Rate_Success),
                                 Var_Rate_Success = var(Rate_Success), .groups="keep")

# Tile plot of mean success rate as both the mean and SD foraging condition changes
plot_env_success <- ggplot(not_emp_both_summaries) +
                 geom_tile(aes(x=Foraging_Condition_Mean, 
                               y=Foraging_Condition_SD, 
                               fill=Mean_Rate_Success)) +
                 scale_y_continuous(limits=c(0, 110), breaks=seq(10, 100, by=20)) +
                 scale_fill_continuous(low="white", high="gray10", name="Success rate",
                                       limits=c(0, 1)) +
                 xlab("Foraging mean (kJ/day)") +
                 ylab("Foraging S.D. (kJ/day)") +
                 theme_lt +
                 theme(legend.title.position="right",
                       legend.title=element_text(size=8, angle=-90, hjust=0.5, vjust=0),
                       legend.text=element_text(size=6))

# Tile plot of variance in success rate as both the mean and SD foraging condition changes
plot_env_var <- ggplot(not_emp_both_summaries) +
             geom_tile(aes(x=Foraging_Condition_Mean, 
                           y=Foraging_Condition_SD, 
                           fill=Var_Rate_Success)) +
             scale_y_continuous(limits=c(0, 110), breaks=seq(10, 100, by=20)) +
             scale_fill_continuous(low="white", high="firebrick3", 
                                   limits=c(0, 0.16),
                                   breaks=seq(0, 0.16, by=0.04), name="Variance success\nbetween parent strategies") +
             xlab("Foraging mean (kJ/day)") +
             ylab("Foraging S.D. (kJ/day)") +
             theme_lt +
             theme(legend.title.position="right",
                   legend.title=element_text(size=6, angle=-90, hjust=0.5, vjust=0),
                   legend.text=element_text(size=6))

# Assemble and print full environmental effects plot
design <- "12"
plot_declines <- plot_decline_hatch_mean + 
              (plot_env_success / 
                plot_env_var +
                plot_layout(axes="collect")) +
              plot_annotation(tag_levels="A", tag_prefix="(", tag_suffix=")") + 
              plot_layout(design=design,
                          widths=c(1, 0.7)) &
              theme(legend.key.width = unit(0.1, "in"),
                    legend.key.height = unit(0.1, "in"),
                    legend.margin = margin(t=0, r=0, b=0, l=0),
                    legend.box.spacing = unit(0.06, "in"),                  
                    plot.tag.position="topleft",
                    plot.tag=element_text(vjust=-2, hjust=-1),
                    plot.margin = margin(t=2, r=4, b=2, l=4))
ggsave(filename="Plots/FIGURE_4.png", plot=plot_declines, 
       width=6.5, height=3, unit="in")

# Print hatching success averages as the environment declines
cat("\n\nSuccess rate at 170 kJ/day, all strategies: Mean = ",
    mean(filter(dat_regular, Foraging_Condition_SD==47, Foraging_Condition_Mean==170)$Rate_Success),
    "  SD = ",
    sd(filter(dat_regular, Foraging_Condition_SD==47, Foraging_Condition_Mean==170)$Rate_Success),
    file=OF, append=TRUE)
cat("\nSuccess rate at 130 kJ/day, all strategies: Mean = ",
    mean(filter(dat_regular, Foraging_Condition_SD==47, Foraging_Condition_Mean==130)$Rate_Success),
    "  SD = ",
    sd(filter(dat_regular, Foraging_Condition_SD==47, Foraging_Condition_Mean==130)$Rate_Success),
    file=OF, append=TRUE)
cat("\nLogistic switch-point, all strategies: ", mlog_all_failpoint, file=OF, append=TRUE)
cat("\nSuccess rate at 170 kJ/day, empirical strategies: Mean = ",
    mean(filter(dat_regular, Is_Empirical_Strategy, Foraging_Condition_SD==47, Foraging_Condition_Mean==170)$Rate_Success),
    "  SD = ",
    sd(filter(dat_regular, Is_Empirical_Strategy, Foraging_Condition_SD==47, Foraging_Condition_Mean==170)$Rate_Success),
    file=OF, append=TRUE)
cat("\nSuccess rate at 130 kJ/day, empirical strategies: Mean = ",
    mean(filter(dat_regular, Is_Empirical_Strategy, Foraging_Condition_SD==47, Foraging_Condition_Mean==130)$Rate_Success),
    "  SD = ",
    sd(filter(dat_regular, Is_Empirical_Strategy, Foraging_Condition_SD==47, Foraging_Condition_Mean==130)$Rate_Success),
    file=OF, append=TRUE)
cat("\nLogistic switch-point, empirical strategies: ", mlog_emp_failpoint, file=OF, append=TRUE)
cat("\nSuccess rate at 170 kJ/day, 10 kJ/day uncertainty: Mean = ",
    mean(filter(dat_regular, Foraging_Condition_SD==10, Foraging_Condition_Mean==170)$Rate_Success),
    "  SD = ",
    sd(filter(dat_regular, Foraging_Condition_SD==10, Foraging_Condition_Mean==170)$Rate_Success),
    file=OF, append=TRUE)
cat("\nSuccess rate at 170 kJ/day, 100 kJ/day uncertainty: Mean = ",
    mean(filter(dat_regular, Foraging_Condition_SD==100, Foraging_Condition_Mean==170)$Rate_Success),
    "  SD = ",
    sd(filter(dat_regular, Foraging_Condition_SD==100, Foraging_Condition_Mean==170)$Rate_Success),
    file=OF, append=TRUE)
cat("\nSuccess rate at 130 kJ/day, 10 kJ/day uncertainty: Mean = ",
    mean(filter(dat_regular, Foraging_Condition_SD==10, Foraging_Condition_Mean==130)$Rate_Success),
    "  SD = ",
    sd(filter(dat_regular, Foraging_Condition_SD==10, Foraging_Condition_Mean==130)$Rate_Success),
    file=OF, append=TRUE)
cat("\nSuccess rate at 130 kJ/day, 100 kJ/day uncertainty: Mean = ",
    mean(filter(dat_regular, Foraging_Condition_SD==100, Foraging_Condition_Mean==130)$Rate_Success),
    "  SD = ",
    sd(filter(dat_regular, Foraging_Condition_SD==100, Foraging_Condition_Mean==130)$Rate_Success),
    file=OF, append=TRUE)
cat("\nSuccess rate for empirical strategies 162 kJ/day, 10 kJ/day uncertainty: Mean = ",
    mean(filter(dat_regular, Is_Empirical_Strategy, Foraging_Condition_SD==10, Foraging_Condition_Mean==162)$Rate_Success),
    "  SD = ",
    sd(filter(dat_regular, Is_Empirical_Strategy, Foraging_Condition_SD==10, Foraging_Condition_Mean==162)$Rate_Success),
    file=OF, append=TRUE)
cat("\nSuccess rate for empirical strategies 162 kJ/day, 100 kJ/day uncertainty: Mean = ",
    mean(filter(dat_regular, Is_Empirical_Strategy, Foraging_Condition_SD==100, Foraging_Condition_Mean==162)$Rate_Success),
    "  SD = ",
    sd(filter(dat_regular, Is_Empirical_Strategy, Foraging_Condition_SD==100, Foraging_Condition_Mean==162)$Rate_Success),
    file=OF, append=TRUE)

# Printing details about variance in different environments
cat("\n\nVariance in success across all strategies at 170 kJ/day: ",
    var(filter(dat_regular, Foraging_Condition_Mean==170, Foraging_Condition_SD==47)$Rate_Success),
    file=OF, append=TRUE)
cat("\nVariance in success across all strategies at 130 kJ/day: ",
    var(filter(dat_regular, Foraging_Condition_Mean==130, Foraging_Condition_SD==47)$Rate_Success),
    file=OF, append=TRUE)
cat("\nVariance in success across all strategies at 160 kJ/day when SD = 10 kJ/day: ",
    var(filter(dat_regular, Foraging_Condition_Mean==160, Foraging_Condition_SD==10)$Rate_Success),
    file=OF, append=TRUE)
cat("\nVariance in success across all strategies at 160 kJ/day when SD = 100 kJ/day: ",
    var(filter(dat_regular, Foraging_Condition_Mean==160, Foraging_Condition_SD==100)$Rate_Success),
    file=OF, append=TRUE)

###############################################################
### Change in outcomes across environments
############################################################

# Get rates of four different outcomes 
# (successful hatch, slow development, cold shock, dead parent)
# excluding the empirical environment, so we have a smooth sample
# of 130 - 170 by 10 (including 160)
outcomes <- dat_regular |> 
         filter(Foraging_Condition_Mean != 162, Foraging_Condition_SD == 47) |>
         select(Strategy_Combination, Foraging_Condition_Mean, contains("Rate_")) |>
         pivot_longer(cols=contains("Rate_"), names_to="Outcome", values_to="Rate") |>
         mutate(Outcome = str_replace(Outcome, "Rate_", ""))     

# Summary line (smoothing) plot of changing outcome rates as mean foraging condition declines
plot_outcomes_smooth <- ggplot(outcomes) +
                     geom_smooth(aes(x=Foraging_Condition_Mean, y=Rate, colour=Outcome),
                                 method="loess", formula=y~x) +
                     annotate(geom="text", label="Successful", hjust=0, vjust=1, x=157, y=0.94, size=3, lineheight=1, colour="black") +
                     annotate(geom="text", label="(Fail)\nCold shock", hjust=0, vjust=1, x=131, y=0.89, size=3, lineheight=1, colour="#7570b3") + 
                     annotate(geom="text", label="(Fail)\nSlow dev.", hjust=0, vjust=1, x=155, y=0.22, size=3, lineheight=1, colour="#1b9e77") +
                     annotate(geom="text", label="(Fail)\nParent dead", hjust=0, vjust=1, x=130.5, y=0.23, size=3, lineheight=1, colour="#d95f02") +
                     scale_colour_manual(values=c("Success"="black",
                                                  "Fail_Egg_Cold"="#7570b3",
                                                  "Fail_Egg_Time"="#1b9e77",
                                                  "Fail_Parent_Dead"="#d95f02")) +
                     scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, by=0.25)) +
                     xlab("Foraging mean (kJ/day)") +
                     ylab("Outcome rate") +
                     guides(colour="none") +
                     theme_lt

# Smaller plots showing the changing outcome rates individual parent strategy combinations
# separate for each outcome
plot_outcome_success <- ggplot(filter(outcomes, Outcome=="Success")) +
                     geom_line(aes(x=Foraging_Condition_Mean, y=Rate, 
                                   group=Strategy_Combination),
                               colour="black", alpha=0.25, linewidth=0.1) +
                     annotate(geom="text", label="Successful", hjust=0, vjust=1, x=130, y=1.0, size=3, lineheight=1, colour="black") +
                     scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, by=0.25)) +
                     xlab("Foraging mean (kJ/day)") +
                     ylab("Outcome rate") +
                     guides(colour="none") +
                     theme_lt
plot_outcome_cold <- ggplot(filter(outcomes, Outcome=="Fail_Egg_Cold")) +
                  geom_line(aes(x=Foraging_Condition_Mean, y=Rate, 
                                group=Strategy_Combination),
                            colour="#7570b3", alpha=0.25, linewidth=0.1)  +
                  annotate(geom="text", label="Cold\nshock", hjust=0, vjust=0, x=130, y=0, size=3, lineheight=1, colour="#7570b3") +
                  scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, by=0.25)) +
                  xlab("Foraging mean (kJ/day)") +
                  ylab("Outcome rate") +
                  guides(colour="none") +
                  theme_lt
plot_outcome_time <- ggplot(filter(outcomes, Outcome=="Fail_Egg_Time")) +
                  geom_line(aes(x=Foraging_Condition_Mean, y=Rate, 
                                group=Strategy_Combination),
                            colour="#1b9e77", alpha=0.25, linewidth=0.1)  +
                  annotate(geom="text", label="Slow\ndevelopment", hjust=0, vjust=1, x=130, y=1.0, size=3, lineheight=1, colour="#1b9e77") +
                  scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, by=0.25)) +
                  xlab("Foraging mean (kJ/day)") +
                  ylab("Outcome rate") +
                  guides(colour="none") +
                  theme_lt
plot_outcome_dead <- ggplot(filter(outcomes, Outcome=="Fail_Parent_Dead")) +
                  geom_line(aes(x=Foraging_Condition_Mean, y=Rate, 
                                group=Strategy_Combination),
                            colour="#d95f02", alpha=0.25, linewidth=0.25)  +
                  annotate(geom="text", label="Parent\ndead", hjust=0, vjust=1, x=130, y=1.0, size=3, lineheight=1, colour="#d95f02") +
                  scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, by=0.25)) +
                  xlab("Foraging mean (kJ/day)") +
                  ylab("Outcome rate") +
                  guides(colour="none") +
                  theme_lt

# Report average outcome rates as foraging conditions change
cat("\n\nOutcome rate (%) across all strategy combinations as environment changes:\n", file=OF, append=TRUE)
sink(OF, append=TRUE)
outcomes |> 
  group_by(Foraging_Condition_Mean, Outcome) |>
  summarize(Mean = round(mean(Rate)*100, 1), 
            SD = round(sd(Rate)*100, 1)) |>
  mutate(Rate = paste0(Mean, "+-", SD)) |>
  pivot_wider(id_cols = "Foraging_Condition_Mean", names_from=Outcome, values_from=Rate) |>
  select(Foraging_Condition_Mean, Success, Fail_Egg_Cold, Fail_Egg_Time, Fail_Parent_Dead) |>
  print()
sink()

###############################################################
### Egg tolerance
############################################################

# Data for egg tolerance runs, showing the effects of egg resilience
# across the range of empirical parent strategy combinations
# Filtered only to narrower range of foraging condition means, which captures full effect
dat_eggTolerance  <- read_processed_dat("Output/processed_eggTolerance.csv") |>
                  filter(Is_Empirical_Strategy,
                         Foraging_Condition_Mean %in% c(150, 160, 170),
                         Foraging_Condition_SD == 47)

egg_strip_labeller <- function(value) {
    ifelse(value == min(as.numeric(value)),
           paste0("Foraging mean: ", value, " (kJ/day)"),
           paste0(value, " (kJ/day)"))
}

# Line plot of changing success rate as egg cold tolerance declines
plot_egg_tolerance <- ggplot(dat_eggTolerance) +
                   geom_line(aes(x=Egg_Tolerance, y=Rate_Success, 
                                 group=Strategy_Combination),
                             colour=EMPIRICAL_COLOR, alpha=0.5) +
                   geom_smooth(aes(x=Egg_Tolerance, y=Rate_Success),
                               method="loess", formula=y~x,
                               colour="#b404a2", se=FALSE) +
                   facet_wrap(facets=vars(Foraging_Condition_Mean), nrow=1, ncol=3,
                              labeller=labeller(Foraging_Condition_Mean=egg_strip_labeller)) +
                   scale_x_continuous(breaks=1:7) +
                   xlab("Egg cold tolerance (days)") +
                   ylab("Success rate") +
                   theme_lt +
                   theme(strip.background=element_rect(colour="transparent", fill="transparent"),
                         strip.text=element_text(size=8, hjust=1))

# Print changing success rates as egg cold tolerance declines
cat("\n\nOutcome rate (%) across empirical strategy combinations as egg tolerance changes :\n", file=OF, append=TRUE)
sink(OF, append=TRUE)
dat_eggTolerance |>
  group_by(Foraging_Condition_Mean, Egg_Tolerance) |>
  summarize(Mean = round(mean(Rate_Success)*100, 2),
            SD = round(sd(Rate_Success)*100, 2)) |>
  mutate(Rate_Success = paste0(Mean, "+-", SD)) |>
  pivot_wider(id_cols = Foraging_Condition_Mean, names_from = Egg_Tolerance, values_from = Rate_Success) |>
  print()
sink()

###############################################################
### Prepare joined Figure 5
############################################################

design <- "123
           145
           666"

plot_outcomes <- plot_outcomes_smooth + 
              plot_outcome_success + plot_outcome_cold + 
              plot_outcome_time + plot_outcome_dead +
              plot_egg_tolerance +
              plot_annotation(tag_levels = list(c("(A)", "", "", "", "", "(B)"))) +
              plot_layout(widths=c(1, 0.5, 0.5), heights=c(1, 1, 1.4),
                          design = design,
                          axes="collect")
  
            #   plot_layout(design=design,
            #               widths=c(1, 0.7)) &
            #   theme(legend.key.width = unit(0.1, "in"),
            #         legend.key.height = unit(0.1, "in"),
            #         legend.margin = margin(t=0, r=0, b=0, l=0),
            #         legend.box.spacing = unit(0.06, "in"),                  
            #         plot.tag.position="topleft",
            #         plot.tag=element_text(vjust=-2, hjust=-1),
            #         plot.margin = margin(t=2, r=4, b=2, l=4))
  
ggsave(filename="Plots/FIGURE_5.png", plot=plot_outcomes, 
       width=6.5, height=5, unit="in")

###############################################################
### One parent energy requirements
############################################################

# Data for single-parent runs, which models how one parent could incubate
# under the range of empirical parent strategies as the environment gets way better
dat_oneParent  <- read_processed_dat("Output/processed_oneParent.csv")

# Fit logistic curves to find fail point (really, success point) for one parent
mlog_oneParent <- glm(Rate_Success ~ Foraging_Condition_Mean, data=dat_oneParent, family="quasibinomial")
mlog_oneParent_failpoint <- -coef(mlog_oneParent)[1] / coef(mlog_oneParent)[2]

# Line plot showing changing success rates as environment gets crazily better for one parent
plot_success_oneParent <- ggplot(dat_oneParent) +
                       geom_line(aes(x=Foraging_Condition_Mean, y=Rate_Success, group=Strategy_Combination),
                                 colour=EMPIRICAL_COLOR, alpha=0.5, linewidth=0.5) +
                       geom_vline(xintercept = mlog_oneParent_failpoint, colour="#b404a2", linewidth=0.25) +
                       stat_smooth(aes(x=Foraging_Condition_Mean, y=Rate_Success), 
                                   method = "glm", method.args = list(family="quasibinomial"), formula=y~x,
                                   se=FALSE, colour="#b404a2", linewidth=0.8) +
                       scale_x_continuous(breaks=seq(130, 400, by=40)) +
                       xlab("Foraging mean (kJ/day)") +
                       ylab("Success rate\n(one parent)") +
                       theme_lt

# Save plot
ggsave(filename="Plots/FIGURE_S_ONEPARENT.png", plot=plot_success_oneParent,
       width=4, height=2, units="in")

# Print the key switch point, when success is at 50%, for a single parent 
cat("\n\nLogistic switch-point, one parent: ", mlog_oneParent_failpoint, file=OF, append=TRUE)