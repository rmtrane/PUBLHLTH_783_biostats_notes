library(tidyverse)

death_penalty <- tibble(victim = factor(rep(c("white", "black"), each = 2),
                                        levels = c("white", "black")),
                        defendant = factor(rep(c("white", "black"), times = 2),
                                           levels = c("white", "black")),
                        death_penalty = c(53, 11, 0, 4),
                        no_death_penalty = c(414, 37, 16, 139)) %>%
  gather(key = "verdict", value = "n", death_penalty, no_death_penalty)

without_victim <- death_penalty %>%
  count(verdict, defendant, wt = n) %>%
  group_by(defendant) %>%
  mutate(prop = n/sum(n)) %>%
  filter(verdict == "death_penalty") %>%
  ggplot(aes(x = defendant,
             y = prop,
             color = "overall")) +
    geom_point() +
    geom_line(aes(group = 1)) +
    scale_y_continuous("Proportion getting the death penalty", limits = c(0, 0.25)) +
    scale_color_manual("", values = "black", labels = "") +
    theme_bw() +
    theme(legend.position = "top")

with_victim <- death_penalty %>%
  count(verdict, defendant, wt = n) %>%
  group_by(defendant) %>%
  mutate(total = sum(n),
         prop = n/total) %>%
  filter(verdict == "death_penalty") %>%
  mutate(victim = factor("overall",
                         levels = c("overall", "white", "black"))) %>%
  ggplot(aes(x = defendant,
             y = prop,
             color = victim)) +
    geom_point() +
    geom_line(aes(group = 1)) +
    geom_point(data = death_penalty %>%
                 group_by(defendant, victim) %>%
                 mutate(total = sum(n),
                        prop = n/total) %>%
                 filter(verdict == "death_penalty"),
               aes(x = defendant,
                   y = prop,
                   size = total)) +
    geom_line(data = death_penalty %>%
                group_by(defendant, victim) %>%
                mutate(total = sum(n),
                       prop = n/total) %>%
                filter(verdict == "death_penalty"),
              aes(x = defendant,
                  y = prop,
                  group = victim)) +
    scale_y_continuous("Proportion getting the death penalty", limits = c(0, 0.25)) +
    scale_color_manual("Victim", values = c("red", "black", "blue")) +
    guides(size = "none") +
    theme_bw() +
    theme(legend.position = "top")

without_victim
with_victim


logistic <- glm(data = death_penalty %>%
      mutate(verdict = as.numeric(verdict == "death_penalty")),
    verdict ~ defendant, weights = n,
    family = "binomial")

logistic2 <- glm(data = death_penalty %>%
                  mutate(verdict = as.numeric(verdict == "death_penalty")),
                verdict ~ defendant + victim, weights = n,
                family = "binomial")

broom::tidy(logistic)
broom::tidy(logistic2)


BibOptions(check.entries = FALSE, bib.style = "authoryear", style = "text")
file.name <- system.file("Bib", "biblatexExamples.bib", package = "RefManageR")
bib <- ReadBib(file.name)

