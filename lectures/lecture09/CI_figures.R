full_show_data <- haven::read_sas(paste0(here::here(), "/data/SHOW/SHOW_PHS.sas7bdat")) %>%
  transmute(depressed = if_else(PHQ_SEVERITY < 2, 0, 1),
            bmi = ANT_BMI,
            smoker = if_else(SMQ_DER_FORMER_NEVER_CURRENT == 1, 1, 0))

sas_files <- list.files("~/SASUniversityEdition/SAS/Lab10/data/", pattern = ".sas7bdat",
                        full.names = T)

datasets <- tibble(sas_files = sas_files,
                   student = str_split(sas_files, pattern = "//", simplify = T)[,2] %>%
                     str_remove(pattern = "\\.sas7bdat"),
                   datasets = map(sas_files,
                                  function(x){
                                    tmp <- haven::read_sas(x)
                                    tmp %>%
                                      transmute(depressed = if_else(PHQ_SEVERITY < 2, 0, 1),
                                                bmi = ANT_BMI,
                                                smoker = if_else(SMQ_DER_FORMER_NEVER_CURRENT == 1, 1, 0)) %>%
                                      return()
                                  }))

RR <- function(x, exposure, outcome){
  exposure <- enexpr(exposure)
  outcome <- enexpr(outcome)

  x <- filter(x, !is.na(!!exposure), !is.na(!!outcome))

  x1 <- nrow(filter(x, !!exposure == 0, !!outcome == 1))
  x2 <- nrow(filter(x, !!exposure == 1, !!outcome == 1))
  n1 <- nrow(filter(x, !!exposure == 0))
  n2 <- nrow(filter(x, !!exposure == 1))

  RR_est <- (x2/n2)/(x1/n1)

  SD <- sqrt(((n1-x1)/x1)/n1 + ((n2-x2)/x2)/(n2))
  lower_CI <- log(RR_est) - 1.96*SD
  upper_CI <- log(RR_est) + 1.96*SD

  tibble(RR_est = RR_est,
         logRR_est = log(RR_est),
         lower_CI_log = lower_CI,
         upper_CI_log = upper_CI,
         lower_CI = exp(lower_CI),
         upper_CI = exp(upper_CI)) %>%
    return()
}

true_mean_diff <- full_show_data %>%
  group_by(depressed) %>%
  summarize(mean = mean(bmi, na.rm = T)) %>%
  filter(!is.na(depressed)) %>%
  pull(mean) %>% diff

true_RR <- RR(x = full_show_data, exposure = smoker, outcome = depressed)

tests <- datasets %>%
  mutate(ttests = map(datasets, function(x) broom::tidy(t.test(data = x, bmi ~ smoker))),
         rr = map(datasets, function(tmp){
           tmp %>% RR(exposure = smoker, outcome = depressed)
         }),
         ) %>%
  unnest_wider(col = c(ttests,rr)) %>%
  mutate(contains_truth = if_else(conf.low < true_mean_diff & conf.high > true_mean_diff, 'Contains truth', 'Does not'),
         contains_true_RR = if_else(lower_CI < true_RR[['RR_est']] & upper_CI > true_RR[['RR_est']], 'Contains truth', 'Does not'),
         contains_true_logRR = if_else(lower_CI_log < true_RR[['logRR_est']] & upper_CI_log > true_RR[['logRR_est']], 'Contains truth', 'Does not'),
         reject_null_means = if_else(conf.high < 0 | conf.low > 0, 'Reject', 'Not reject'),
         reject_null_RR = if_else(upper_CI < 1 | lower_CI > 1, 'Reject', 'Not reject'))

diff_mean_plot <- tests %>%
  ggplot(aes(x = student, y = estimate, ymax = conf.high, ymin = conf.low,
             color = contains_truth)) +
    geom_point() +
    geom_hline(yintercept = true_mean_diff, linetype = 'dashed', color = 'red') +
    geom_errorbar() +
    theme_bw() +
    scale_color_viridis_d("") +
    ggtitle("95% CIs for difference in means",
            subtitle = paste0(sum(tests$contains_truth == "Contains truth"), " (",
                              round(mean(tests$contains_truth == "Contains truth"), digits = 3)*100,
                              "%) contain the true value")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")


RR_plot <- tests %>%
  ggplot(aes(x = student, y = RR_est, ymax = upper_CI, ymin = lower_CI,
             color = contains_true_RR)) +
  geom_point() +
  geom_hline(yintercept = true_RR[['RR_est']], linetype = 'dashed', color = 'red') +
  geom_errorbar() +
  theme_bw() +
  scale_color_viridis_d("") +
  ggtitle("95% CIs for RR",
          subtitle = paste0(sum(tests$contains_true_RR == "Contains truth"), " (",
                            round(mean(tests$contains_true_RR == "Contains truth")*100, digits = 3),
                            "%) contain the true value")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

log_RR_plot <- tests %>%
  ggplot(aes(x = student, y = logRR_est, ymax = upper_CI_log, ymin = lower_CI_log,
             color = contains_true_logRR)) +
  geom_point() +
  geom_hline(yintercept = true_RR[['logRR_est']], linetype = 'dashed', color = 'red') +
  geom_errorbar() +
  theme_bw() +
  scale_color_viridis_d("") +
  ggtitle("95% CIs for log(RR)",
          subtitle = paste0(sum(tests$contains_true_logRR == "Contains truth"), " (",
                            round(mean(tests$contains_true_logRR == "Contains truth")*100, digits = 3),
                            "%) contain the true value")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")


means_tests <- tests %>%
  ggplot(aes(x = student, y = estimate, ymax = conf.high, ymin = conf.low,
             color = reject_null_means)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
  geom_errorbar() +
  theme_bw() +
  scale_color_viridis_d("") +
  ggtitle("95% CIs for difference in means",
          subtitle = paste0(sum(tests$reject_null_means == "Reject"), " (",
                            round(mean(tests$reject_null_means == "Reject")*100, digits = 2),
                            "%) reject the null.")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

RR_tests <- tests %>%
  ggplot(aes(x = student, y = RR_est, ymax = upper_CI, ymin = lower_CI,
             color = reject_null_RR)) +
  geom_point() +
  geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
  geom_errorbar() +
  theme_bw() +
  scale_color_viridis_d("") +
  ggtitle("95% CIs for RR",
          subtitle = paste0(sum(tests$reject_null_RR == "Reject"), " (",
                            round(mean(tests$reject_null_RR == "Reject")*100, digits = 2),
                            "%) reject the null.")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

ggsave(RR_plot, filename = "lectures/lecture09/RR_plot.png", width = 9, height = 7)
ggsave(log_RR_plot, filename = "lectures/lecture09/log_RR_plot.png", width = 9, height = 7)
ggsave(diff_mean_plot, filename = "lectures/lecture09/diff_mean_plot.png", width = 9, height = 7)
ggsave(RR_tests, filename = "lectures/lecture09/RR_tests.png", width = 9, height = 7)
ggsave(means_tests, filename = "lectures/lecture09/means_tests.png", width = 9, height = 7)


write_csv(tests %>% select(-sas_files, -datasets),
          path = "lectures/lecture09/tests.csv")
