title_factor =
  c(
    "Standard Normal", "Wide Normal",  "Bimodal Persons", "Bimodal Items",
    "Small Mismatch", "Large Mismatch", "Extreme Mismatch"
  )

all_mad_estimates %>%
  dplyr::select(contains("Mean_")) %>%
  distinct() %>%
  mutate(
    Title = fct(Title, levels = title_factor)
    , N_Person = fct(N_Person, levels = c('25', '50', '100', '200', '500', '1000')) 
    ) %>%
  pivot_longer(
    cols = contains("_MAD_")
    , values_to = "MAD"
    , names_to = "Estimation"
  ) %>%
  ggplot() +
  geom_bar(
    aes(y = MAD, x = Title, group = N_Person, fill = Estimation)
    , stat = 'identity'
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  facet_wrap(~ Estimation + N_Person, nrow = 5)



all_mse_estimates %>%
  dplyr::select(contains("Mean_")) %>%
  distinct() %>%
  mutate(
    Title = fct(Title, levels = title_factor)
    , N_Person = fct(N_Person, levels = c('25', '50', '100', '200', '500', '1000'))
    ) %>%
  pivot_longer(
    cols = contains("_MSE_")
    , values_to = "MSE"
    , names_to = "Estimation"
  ) %>%
  ggplot() +
  geom_bar(
    aes(y = MSE, x = Title, group = N_Person, fill = Estimation)
    , stat = 'identity'
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  facet_wrap(~ Estimation + N_Person, nrow = 5)


mean_bias_estimates %>%
  dplyr::select(contains("Mean_")) %>%
  distinct() %>%
  mutate(
    Title = fct(Title, levels = title_factor)
    , N_Person = fct(N_Person, levels = c('25', '50', '100', '200', '500', '1000'))
  ) %>%
  pivot_longer(
    cols = contains("_Bias")
    , values_to = "Bias"
    , names_to = "Estimation"
  ) %>%
  ggplot() +
  geom_bar(
    aes(y = Bias, x = Title, group = N_Person, fill = Estimation)
    , stat = 'identity'
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  facet_wrap(~ Estimation + N_Person, nrow = 5)

