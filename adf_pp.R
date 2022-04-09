data_diff

adf.test(join_data$m_number_dated_brent)[c("statistic", "p.value")]

adf_results <- data.frame(crude = rep(NA_character_, 15),
                          stat = rep(NA_real_, 15),
                          pval = rep(NA_real_, 15))
for (i in 3:17) {
  x <- adf.test(join_data[,i])
  adf_results[i-2,1] <- colnames(join_data)[i]
  adf_results[i-2,2] <- x$statistic
  adf_results[i-2,3] <- x$p.value
}
adf_results
write.csv(adf_results, file = "adf_results_notdiff.csv")



adf_results_diff <- data.frame(crude = rep(NA_character_, 15),
                               stat = rep(NA_real_, 15),
                               pval = rep(NA_real_, 15))
for (i in 3:17) {
  x <- adf.test(data_diff[,i])
  adf_results_diff[i-2,1] <- colnames(data_diff)[i]
  adf_results_diff[i-2,2] <- x$statistic
  adf_results_diff[i-2,3] <- x$p.value
}
adf_results_diff
write.csv(adf_results_diff, file = "adf_results_diff.csv")








pp_results <- data.frame(crude = rep(NA_character_, 15),
                         stat = rep(NA_real_, 15),
                         pval = rep(NA_real_, 15))
for (i in 3:17) {
  x <- pp.test(join_data[,i], alternative = "stationary")
  pp_results[i-2,1] <- colnames(join_data)[i]
  pp_results[i-2,2] <- x$statistic
  pp_results[i-2,3] <- x$p.value
}
pp_results
write.csv(pp_results, file = "pp_results_notdiff.csv")



pp_results_diff <- data.frame(crude = rep(NA_character_, 15),
                              stat = rep(NA_real_, 15),
                              pval = rep(NA_real_, 15))
for (i in 3:17) {
  x <- pp.test(data_diff[,i], alternative = "stationary")
  pp_results_diff[i-2,1] <- colnames(data_diff)[i]
  pp_results_diff[i-2,2] <- x$statistic
  pp_results_diff[i-2,3] <- x$p.value
}
pp_results_diff
write.csv(pp_results_diff, file = "pp_results_diff.csv")
