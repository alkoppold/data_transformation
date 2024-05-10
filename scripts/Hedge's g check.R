library(tidyverse)

#apa package
iris %>% group_by(Species) %>% summarise(
  cohen_d = apa::cohens_d(Sepal.Length),
  n = Sepal.Length %>% na.omit() %>% length(), #should not use n() because it cannot handle NAs accordingly
  hedges_g = apa::cohens_d(Sepal.Length, corr="hedges_g"),
  hedges_g_df1 = cohen_d * (1 - (3 / (4 * (n-1) - 1))), #for one-sample & within: df = N - 1
  hedges_g_df2 = cohen_d * (1 - (3 / (4 * (n-2) - 1))), #for two independent samples
  check_df1 = hedges_g == hedges_g_df1,
  check_df2 = hedges_g == hedges_g_df2,
  check_noChange = cohen_d == hedges_g
)

#effsize package
iris %>% group_by(Species) %>% summarise(
  cohen_d = effsize::cohen.d(Sepal.Length, f=NA)$estimate,
  n = Sepal.Length %>% na.omit() %>% length(), #should not use n() because it cannot handle NAs accordingly
  hedges_g = effsize::cohen.d(Sepal.Length, NA, hedges.correction=T)$estimate,
  hedges_g_df1 = cohen_d * (1 - (3 / (4 * (n-1) - 1))), #for one-sample & within: df = N - 1
  hedges_g_df2 = cohen_d * (1 - (3 / (4 * (n-2) - 1))), #for two independent samples
  check_df1 = hedges_g == hedges_g_df1,
  check_df2 = hedges_g == hedges_g_df2
)
