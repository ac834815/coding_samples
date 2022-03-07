library(tidyverse)

block <- function(trials, groups) {
  responses <- floor(runif(trials, 0, groups))
  correct <- responses == 1
  return(sum(correct)/trials)
}

nsims <- 40000
trialsPerBlock <- 128
ncats <- 2
threshold <- .999

sims <- c()
runningMean <- c()
for (i in 1:nsims) {
  sims <- append(sims, block(trialsPerBlock, ncats))
  runningMean <- append(runningMean, mean(sims))
}
upperLimit <- quantile(sims, threshold)
maxDensity <- max(density(sims)$y)

ggplot() +
  geom_density(aes(x = sims), fill = "gray", color="black", alpha = .6) +
  geom_vline(xintercept = upperLimit, color = "red", linetype = "dashed", size = 2, alpha = .8) +
  geom_label(aes(label = paste0(threshold*100, "th Percentile:\n", round(upperLimit*100,2), "%")), x = upperLimit, y = maxDensity -1, hjust = 0.5) +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Accuracy",
       y = "Density",
       title = "Accuracy of Random Responders",
       subtitle = paste0(nsims, " simulations, ", ncats, " categories, ", trialsPerBlock, " trials per block of training")
  ) +
  theme(
    panel.background = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.x.bottom = element_line(),
    axis.line.y.left = element_line()
  )

ggplot() +
  geom_hline(yintercept = 1/ncats, color = "red", linetype = "dashed", size = 2, alpha = .8) +
  geom_line(aes(x = 1:nsims, y = runningMean), size = 1.2, alpha = .6) +
  geom_label(aes(label = paste0("Chance: ", round(100*(1/ncats), 2), "%")), x = (9/10)*nsims, y = 1/ncats + .01, hjust = 0.5) +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(breaks = 1) +
  labs(
    x = "",
    y = "Running Mean",
    title = paste0("Running Mean accuracy across ", nsims, " simulations")
  ) +
  theme(
    panel.background = element_blank(),
    # axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.line.x.bottom = element_line(),
    axis.line.y.left = element_line()
  )
