library(tidyverse)
library(yardstick)
library(glue)
library(fs)
library(ggstance)
library(patchwork)
library(ggtext)

recall_vec(factor(c(rep(1, 51), rep(0, 49))), factor(c(rep(1, 71), rep(0, 29))))

# annotated <- read_csv("~/Downloads/permissed_sampled_annotated_with_pipeline_predictions.csv") %>%
# annotated <- read_csv("~/Downloads/permissed_sampled_annotated_with_predictions.csv") %>%
annotated <- read_csv("data/permissed_sampled_annotated_old1k.csv") %>%
  mutate(
    aann = case_when(is.na(aann) ~ 0, TRUE~aann),
    aann = case_when(span == "a season eight years" ~ 0, TRUE ~ aann),
    aann = case_when(str_detect(span, "an estimated 182 persons") ~ 1, TRUE ~ aann),
    aann = case_when(str_detect(span, "a fucking few minutes") ~ 1, TRUE ~ aann),
    aann = case_when(str_detect(span, "a season-high 19 points") ~ 1, TRUE ~ aann),
    aann = case_when(str_detect(span, "a long and two shorts") ~ 1, TRUE ~ aann),
    aann = case_when(str_detect(span, "a great many more people") ~ 1, TRUE ~ aann),
    aann = factor(aann, levels = c(1,0)),
    og_detected = factor(og_detected, levels = c(1,0)),
    current_detected = factor(current_detected, levels = c(1,0)),
    dep_parse_detected = factor(dep_parse_detected, levels = c(1,0)),
    current_with_dt_detected = factor(current_with_dt_detected, levels = c(1,0))
  )
# 
# annotated %>%
#   write_csv("~/Downloads/permissed_sampled_annotated_with_predictions.csv")

# annotated <- read_csv("~/Downloads/permissed_sampled_annotated_old1k_new_regex.csv") %>%
annotated <- read_csv("~/Downloads/permissed_sampled_annotated_test1_1k_final_regex.csv") %>%
  mutate(
    aann = factor(aann, levels = c(1,0)),
    og_detected = factor(og_detected, levels = c(1,0)),
    current_detected = factor(current_detected, levels = c(1,0)),
    dep_parse_detected = factor(dep_parse_detected, levels = c(1,0)),
    current_with_dt_detected = factor(current_with_dt_detected, levels = c(1,0)),
    final_detected = factor(final_detected, levels = c(1,0))
  )

og_recall = recall_vec(annotated$aann, annotated$og_detected)
current_recall = recall_vec(annotated$aann, annotated$current_detected)
dep_parse_recall = recall_vec(annotated$aann, annotated$dep_parse_detected)
current_dt_recall = recall_vec(annotated$aann, annotated$current_with_dt_detected)
final_recall = recall_vec(annotated$aann, annotated$final_detected)

og_precision = precision_vec(annotated$aann, annotated$og_detected)
current_precision = precision_vec(annotated$aann, annotated$current_detected)
dep_parse_precision = precision_vec(annotated$aann, annotated$dep_parse_detected)
current_dt_precision = precision_vec(annotated$aann, annotated$current_with_dt_detected)
final_precision = precision_vec(annotated$aann, annotated$final_detected)

precision_vec(annotated$aann, annotated$og_detected)

annotated %>%
  filter(aann == 1 & dep_parse_detected != 1) %>%
  count(span)

annotated %>%
  filter(aann == 1 & og_detected != 1) %>%
  count(span)

annotated %>%
  filter(aann == 1 & current_detected != 1) %>%
  count(span)

## new results!

dir = "mahowald-aann"

model_meta <- read_csv("data/results/babylm_lms.csv")

train_constructions <- read_csv("data/results/target_constructions.csv")
mahowald_meta <- read_csv(glue("data/{dir}/aanns_meta.csv"))
all_corruptions <- read_csv(glue("data/{dir}/aanns_corruption.csv"))

unseen <- read_csv("data/mahowald-aann/mahowald-aanns-unseen_good.csv") %>%
  pull(idx)

aanns <- read_csv(glue("data/{dir}/mahowald-aanns-unseen_good.csv"))

human_data <- read_csv("data/mturk_ratings_20231004.csv") %>%
  inner_join(aanns) %>%
  inner_join(mahowald_meta) %>%
  select(idx, adjclass, numclass, nounclass, sentence, rating=answer) %>%
  mutate(
    adjclass = str_remove(adjclass, "adj-"),
    nounclass = str_remove(nounclass, "noun-"),
    adjclass=case_when(
      adjclass %in% c("neg", "pos") ~ "qual",
      TRUE ~ adjclass
    ),
    adjclass = factor(adjclass, levels=c("quant", "ambig", "qual", "human", "color", "stubborn"))
  )

good_ids <- human_data %>%
  filter(rating > 7) %>%
  pull(idx)

best_models <- tribble(
  ~suffix,~lr,
  "babylm", "1e-3",
  "counterfactual-babylm-indef-naan-rerun", "1e-3",
  "counterfactual-babylm-indef-anan", "3e-4",
  "counterfactual-babylm-aann-prototypical_only", "1e-3",
  "counterfactual-babylm-aann-no_prototypical", "3e-4",
  "counterfactual-babylm-adj_num_freq_balanced", "1e-4",
  "counterfactual-babylm-indef_articles_with_pl_nouns-removal", "3e-4",
  "counterfactual-babylm-indef-naan-non-num", "1e-4",
  "counterfactual-babylm-all_det_removal", "1e-3",
  "counterfactual-babylm-indef-removal", "1e-4",
  "counterfactual-babylm-measure_nouns_as_singular", "1e-4",
  "counterfactual-babylm-random_removal", "3e-4",
  "counterfactual-babylm-only_other_det_removal", "1e-3",
  "counterfactual-babylm-only_indef_articles_with_pl_nouns_removal", "1e-3",
  "counterfactual-babylm-only_measure_nps_as_singular_removal", "1e-3",
  "counterfactual-babylm-only_random_removal", "1e-3",
  "counterfactual-babylm-old_union_new_regex_aanns_removal", "1e-3",
  "counterfactual-babylm-new_regex_aanns_removal", "1e-3",
  "counterfactual_babylm_naans_new", "1e-4",
  "counterfactual_babylm_anans_new", "1e-4",
  "counterfactual_babylm_300_naans_new", "1e-3",
  "counterfactual_babylm_300_anans_new", "1e-3",
  "counterfactual_babylm_aann_dtanns", "1e-4",
  "counterfactual_babylm_measure_nps_as_singular_new", "1e-3",
  "counterfactual_babylm_indef_articles_with_pl_nouns_removal_new", "1e-3",
  "counterfactual_babylm_aann_high_variability_all", "1e-3",
  "counterfactual_babylm_aann_high_variability_adj", "1e-3",
  "counterfactual_babylm_aann_high_variability_noun", "1e-3",
  "counterfactual_babylm_aann_high_variability_numeral", "1e-3",
  "counterfactual_babylm_aann_low_variability_all", "1e-3",
  "counterfactual_babylm_aann_low_variability_adj", "1e-3",
  "counterfactual_babylm_aann_low_variability_noun", "1e-3",
  "counterfactual_babylm_aann_low_variability_numeral", "1e-3"
)

unigram_scores <- dir_ls("results/unigrams/") %>%
  keep(str_detect(., "csv")) %>%
  map_df(read_csv, .id = "model") %>%
  mutate(
    model = str_remove(model, "results/unigrams/"),
    model = str_remove(model, ".csv"),
    suffix = model
  )

unigram_scores <- bind_rows(
  unigram_scores,
  unigram_scores %>% filter(model == "babylm") %>% mutate(model = "babylm-naan", suffix="counterfactual-babylm-indef-naan-rerun"),
  unigram_scores %>% filter(model == "babylm") %>% mutate(model = "babylm-anan", suffix="counterfactual-babylm-indef-anan"),
)

unigram_scores %>%
  count(model)

# ngrams

fourgram_scores <- dir_ls("results/fourgrams/") %>%
  keep(str_detect(., "csv")) %>%
  map_df(read_csv, .id = "model") %>%
  mutate(
    model = str_remove(model, "results/fourgrams/"),
    model = str_remove(model, ".csv"),
    suffix = model
  )

fourgram_slors <- fourgram_scores %>%
  inner_join(unigram_scores %>% select(-model) %>% rename_with(function(x) {str_c("ngram_", x)}, ends_with("score")), by = c("idx", "suffix")) %>%
  mutate(
    construction_score = construction_score - ngram_construction_score,
    default_nan_score = default_nan_score - ngram_default_nan_score,
    order_swap_score = order_swap_score - ngram_order_swap_score,
    no_article_score = no_article_score - ngram_no_article_score,
    no_modifier_score = no_modifier_score - ngram_no_modifier_score,
    no_numeral_score = no_numeral_score - ngram_no_numeral_score,
  ) %>%
  filter(idx %in% good_ids) %>%
  select(-starts_with("ngram")) %>%
  select(model, suffix, idx, score = construction_score)
  # pivot_longer(construction_score:no_numeral_score, names_to = "surface_form", values_to = "logprob")

scores <- dir_ls("results/", recurse = TRUE, regexp = "mahowald-") %>%
  keep(str_detect(., "csv")) %>%
  map_df(read_csv, .id = "model") %>% 
  mutate(
    construction = str_extract(model, "(?<=mahowald-)(.*)(?=/)"),
    suffix = str_remove(model, "results/mahowald-(naan|anan|aann)/smolm-autoreg-bpe-"),
    suffix = str_remove(suffix, "-\\de-\\d.csv"),
    suffix = str_remove(suffix, "-seed_\\d{3,4}"),
    seed = case_when(
      str_detect(model, "seed") ~ as.numeric(str_extract(model, "(?<=seed_)(.*)(?=-\\de)")),
      TRUE ~ 42
    ),
    model = str_replace(model, "-seed_\\d{3,4}-", "-"),
    model = str_remove(model, ".csv"),
    model = str_remove(model, "results/"),
    model = str_remove(model, "autoreg-bpe-babylm-"),
    model = str_remove(model, "autoreg-bpe-counterfactual-babylm-"),
    model = str_remove(model, "aann-"),
    model = str_remove(model, "no_aann"),
    model = str_remove(model, "mahowald-(aann|naan|anan)/"),
    model = case_when(
      str_detect(model, "smolm-1e-3") ~ "smolm-aann-1e-3",
      str_detect(model, "smolm-1e-4") ~ "smolm-aann-1e-4",
      str_detect(model, "smolm-3e-4") ~ "smolm-aann-3e-4",
      TRUE ~ model
    ),
    # model = str_remove(model, "-\\de-\\d"),
    train_construction = str_extract(model, "(?<=-)(.*)") %>% str_remove("(counterfactual-)") %>% str_remove("indef-") %>% str_remove("-\\de-\\d") %>% str_replace("all-det-removal", "no-det-ann") %>% str_replace("infilling|removal", "none")
  ) %>% 
  rename(
    target_construction = construction
  )

results <- scores %>% 
  filter(idx %in% good_ids) %>%
  group_by(suffix, seed, train_construction, target_construction) %>%
  summarize(
    default_preference = mean(default_nan_score > construction_score),
    order_swap = mean(construction_score > order_swap_score),
    no_article = mean(construction_score > no_article_score),
    no_modifier = mean(construction_score > no_modifier_score),
    no_numeral = mean(construction_score > no_numeral_score),
    overall = mean(construction_score > order_swap_score & construction_score > no_article_score & construction_score > no_modifier_score & construction_score > no_numeral_score)
  )

slors <- scores %>%
  filter(idx %in% good_ids) %>%
  select(-train_construction) %>%
  mutate(
    lr = str_extract(model, "\\de-\\d"),
    suffix = str_remove(suffix, "-\\de-\\d.csv"),
    model = str_remove(model, "-\\de-\\d")
  ) %>% 
  inner_join(best_models) %>%
  inner_join(unigram_scores %>% select(-model) %>% rename_with(function(x) {str_c("ngram_", x)}, ends_with("score")), by = c("idx", "suffix")) %>%
  mutate(
    construction_score = construction_score - ngram_construction_score,
    order_swap_score = order_swap_score - ngram_order_swap_score,
    no_article_score = no_article_score - ngram_no_article_score,
    no_modifier_score = no_modifier_score - ngram_no_modifier_score,
    no_numeral_score = no_numeral_score - ngram_no_numeral_score,
  )

slor_results <- slors %>%
  group_by(model, suffix, seed, target_construction) %>%
  summarize(
    accuracy = mean(
      construction_score > order_swap_score & 
        construction_score > no_article_score & 
        construction_score > no_numeral_score & 
        construction_score > no_modifier_score
    )
  )

slor_results %>%
  count(model) %>%
  View()

noised_results <- slor_results %>%
  filter(str_detect(model, "300")) %>%
  mutate(
    noise = case_when(
        str_detect(model, "anan") ~ "ANAN",
        str_detect(model, "naan") ~ "NAAN"
    ),
    train_condition = "AANN",
    # train_condition = case_when(
    #   str_detect(model, "anan") ~ "ANAN",
    #   str_detect(model, "naan") ~ "NAAN"
    # ),
    target_construction = str_to_upper(target_construction),
    target_construction = glue("Test on {target_construction}"),
    train_condition = factor(
      train_condition, 
      levels = c("AANN", "No AANN", "ANAN", "NAAN"),
    ),
    remove = case_when(
      target_construction == "Test on NAAN" & noise == "ANAN" ~ 1,
      target_construction == "Test on ANAN" & noise == "NAAN" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(remove != 1)


noised_and_all <- bind_rows(
  noised_results %>% select(-remove),
  slor_results %>%
    filter(model %in% c("smolm-aann", "smolm-new_regex_aanns_removal", "smolm-autoreg-bpe-counterfactual_babylm_anans_new", "smolm-autoreg-bpe-counterfactual_babylm_naans_new")) %>%
    mutate(
      train_condition = case_when(
        model == "smolm-aann" ~ "AANN",
        model == "smolm-autoreg-bpe-counterfactual_babylm_anans_new" ~ "ANAN",
        model == "smolm-autoreg-bpe-counterfactual_babylm_naans_new" ~ "NAAN",
        TRUE ~ "No AANN"
      ),
      train_condition = factor(
        train_condition, 
        levels = c("AANN", "No AANN", "ANAN", "NAAN"),
      ),
      target_construction = str_to_upper(target_construction),
      target_construction = glue("Test on {target_construction}")
    )
) %>%
  mutate(
    color = case_when(
      train_condition == "AANN" & is.na(noise) ~ "#7570b3",
      train_condition == "AANN" & noise == "ANAN" ~ "#ae4d9f",
      train_condition == "AANN" & noise == "NAAN" ~ "#488795",
      train_condition == "No AANN" ~ "#d95f02",
      train_condition == "ANAN" ~ "#e7298a",
      train_condition == "NAAN" ~ "#1b9e77"
    ),
    fill = case_when(
      train_condition == "AANN" & is.na(noise) ~ "#7570b3",
      train_condition == "AANN" & noise == "ANAN" ~ "#ae4d9f",
      train_condition == "AANN" & noise == "NAAN" ~ "#488795",
      train_condition == "No AANN" ~ "#d95f02",
      train_condition == "ANAN" ~ "#e7298a",
      train_condition == "NAAN" ~ "#1b9e77"
    ),
    shape = case_when(
      train_condition == "AANN" & is.na(noise) ~ "circle filled",
      train_condition == "AANN" & noise == "ANAN" ~ "diamond filled",
      train_condition == "AANN" & noise == "NAAN" ~ "triangle filled",
      train_condition == "No AANN" ~ "square filled",
      train_condition == "ANAN" ~ "diamond filled",
      train_condition == "NAAN" ~ "triangle filled"
    ),
    shape = factor(shape)
  )

set.seed(1024)

noised_and_all %>%
  ggplot(aes(train_condition, accuracy, color = color)) +
  geom_hline(yintercept=0, linetype = "dotdash", color = "gray") +
  geom_jitter(aes(shape=shape, fill = fill), size = 3, alpha = 0.7, width = 0.1) +
  # geom_hline(yintercept = 0.0625, linetype = "dashed") +
  geom_hline(yintercept = 0.20, linetype = "dashed") +
  geom_hline(
    data = results %>% 
      filter(str_detect(suffix, "gpt2"), target_construction == "aann") %>%
      select(-train_construction) %>%
      mutate(target_construction = "Test on AANN"),
    aes(yintercept = overall),
    linetype = "dotted",
    color = "firebrick"
  ) +
  geom_hline(
    data = results %>% 
      filter(str_detect(suffix, "Llama"), target_construction == "aann") %>%
      select(-train_construction) %>%
      mutate(target_construction = "Test on AANN"),
    aes(yintercept = overall),
    linetype = "dotted",
    color = "steelblue"
  ) +
  geom_text(
    data = tibble(
      train_condition = "NAAN",
      accuracy = 0.9,
      target_construction="Test on AANN"
    ),
    label = "Llama-2-7B",
    color = "steelblue",
    family = "Times",
    size = 3.5
  ) +
  geom_text(
    data = tibble(
      train_condition = "NAAN",
      accuracy = 0.73,
      target_construction="Test on AANN"
    ),
    label = "GPT-2 XL",
    color = "firebrick",
    family = "Times",
    size = 3.5
  ) +
  # geom_curve(
  #   xend = 3.1, yend = 0.14, 
  #   x = 2.7, y = 0.001, 
  #   curvature = -0.3, 
  #   arrow = arrow(length = unit(2, "mm")), 
  #   data = tibble(train_condition="AANN", target_construction="Test on AANN"),
  #   color = "grey"
  # ) +
  # geom_text(
  #   data = tibble(
  #     train_condition = "AANN",
  #     accuracy = 0.14,
  #     target_construction="Test on AANN"
  #   ),
  #   x = 3.8,
  #   label = "2 & 4-gram",
  #   color = "darkgrey",
  #   family = "Times",
  #   size = 4
  # ) +
  # geom_curve(
  #   xend = 1.6, yend = 0.14, 
  #   x = 2, y = 0.063, 
  #   curvature = 0.3, 
  #   arrow = arrow(length = unit(2, "mm")), 
  #   data = tibble(train_condition="AANN", target_construction="Test on AANN"),
  #   color = "black"
  # ) +
  geom_curve(
    xend = 1.6, yend = 0.14, 
    x = 2, y = 0.2, 
    curvature = -0.25, 
    arrow = arrow(length = unit(2, "mm")), 
    data = tibble(train_condition="AANN", target_construction="Test on AANN"),
    color = "black"
  ) +
  geom_text(
    data = tibble(
      train_condition = "AANN",
      accuracy = 0.14,
      target_construction="Test on AANN"
    ),
    x = 1.2,
    label = "Chance",
    color = "black",
    family = "Times",
    size = 4
  ) +
  # geom_point(data = noised_results %>% filter(noise == "ANAN"), shape = 23, size = 3, fill = "#ae4d9f", color = "#ae4d9f", alpha = 0.7) +
  # geom_point(data = noised_results %>% filter(noise == "NAAN"), shape = 24, size = 3, fill = "#488795", color = "#488795", alpha = 0.7) +
  facet_wrap(~target_construction) +
  # scale_color_manual(values = c("#7570b3","#d95f02","#e7298a","#1b9e77"), aesthetics = c("color", "fill")) +
  # scale_shape_manual(values = c(21, 22, 23, 24)) +
  scale_color_identity(aesthetics = c("color", "fill")) +
  scale_shape_identity()+
  scale_y_continuous(limits = c(0.0, 1.0)) +
  theme_bw(base_size = 17, base_family="Times") +
  # theme_minimal(base_size=17, base_family = "Times") +
  # ggthemes::theme_tufte(base_size=17, base_family = "Times") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(color = "black", size = 11),
    legend.title = element_blank(),
    # panel.grid.minor.x = element_blank(),
    # panel.border = element_blank(),
    # axis.line = element_line(),
    axis.ticks = element_line(),
    panel.background = element_rect(fill='transparent', color=NA), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    legend.background = element_rect(fill='transparent', color = NA), #transparent legend bg
    legend.box.background = element_rect(fill='transparent', color=NA) #transparent legend panel
    # axis.text.x = element_markdown(color = "black")
  ) +
  labs(
    x = "Train Condition",
    y = "Accuracy\n(3 LM runs)"
  )

set.seed(1024)

# ggsave("slides/exp1.pdf", height=3.64, width = 10.89, dpi = 300, device = cairo_pdf

# ggsave("paper/camera-ready/counterfactualaccuracies.pdf", height = 3.06, width = 9.17, dpi = 300, device=cairo_pdf)
ggsave("paper/camera-ready/counterfactualaccuracies.pdf", height = 3.64, width = 10.89, dpi = 300, device=cairo_pdf)


noised_and_all %>%
  filter(is.na(noise)) %>%
  filter(str_detect(target_construction, "AANN"), str_detect(train_condition, "AANN")) %>%
  ggplot(aes(train_condition, accuracy, color = train_condition, fill = train_condition, shape = train_condition)) +
  geom_hline(yintercept=0, linetype = "dotdash", color = "gray") +
  geom_point(size = 3, alpha = 0.7) +
  # geom_hline(yintercept = 0.0625, linetype = "dashed") +
  geom_hline(yintercept = 0.2, linetype = "dashed") +
  geom_hline(
    data = results %>% 
      filter(str_detect(suffix, "gpt2"), target_construction == "aann") %>%
      select(-train_construction) %>%
      mutate(target_construction = "Test on AANN"),
    aes(yintercept = overall),
    linetype = "dotted",
    color = "firebrick"
  ) +
  geom_hline(
    data = results %>% 
      filter(str_detect(suffix, "Llama"), target_construction == "aann") %>%
      select(-train_construction) %>%
      mutate(target_construction = "Test on AANN"),
    aes(yintercept = overall),
    linetype = "dotted",
    color = "steelblue"
  ) +
  # geom_text(x = "NAAN", y = 0.87, label = "LLama-2-7B", data=tibble(target_construction="AANN")) +
  geom_text(
    data = tibble(
      train_condition = "No AANN",
      accuracy = 0.9,
      target_construction="Test on AANN"
    ),
    label = "Llama-2-7B",
    color = "steelblue",
    family = "Times",
    size = 3.5
  ) +
  geom_text(
    data = tibble(
      train_condition = "No AANN",
      accuracy = 0.73,
      target_construction="Test on AANN"
    ),
    label = "GPT-2 XL",
    color = "firebrick",
    family = "Times",
    size = 3.5
  ) +
  geom_curve(
    xend = 1.9, yend = 0.14, 
    x = 1.5, y = 0.001, 
    curvature = -0.3, 
    arrow = arrow(length = unit(2, "mm")), 
    data = tibble(train_condition="AANN", target_construction="Test on AANN"),
    color = "grey"
  ) +
  geom_text(
    data = tibble(
      train_condition = "AANN",
      accuracy = 0.14,
      target_construction="Test on AANN"
    ),
    x = 2.2,
    label = "2 & 4-gram",
    color = "darkgrey",
    family = "Times",
    size = 4
  ) +
  # geom_curve(
  #   xend = 0.87, yend = 0.14, 
  #   x = 1.1, y = 0.063, 
  #   curvature = 0.3, 
  #   arrow = arrow(length = unit(2, "mm")), 
  #   data = tibble(train_condition="AANN", target_construction="Test on AANN"),
  #   color = "black"
  # ) +
  # geom_text(
  #   data = tibble(
  #     train_condition = "AANN",
  #     accuracy = 0.14,
  #     target_construction="Test on AANN"
  #   ),
  #   x = 0.7,
  #   label = "Chance",
  #   color = "black",
  #   family = "Times",
  #   size = 4
  # ) +
  geom_curve(
    xend = 0.87, yend = 0.14, 
    x = 1.1, y = 0.2, 
    curvature = -0.25, 
    arrow = arrow(length = unit(2, "mm")), 
    data = tibble(train_condition="AANN", target_construction="Test on AANN"),
    color = "black"
  ) +
  geom_text(
    data = tibble(
      train_condition = "AANN",
      accuracy = 0.14,
      target_construction="Test on AANN"
    ),
    x = 0.7,
    label = "Chance",
    color = "black",
    family = "Times",
    size = 4
  ) +
  # annotation_label = annotate(
  #   geom = "label",
  #   x = 7.53, y = 0.35,
  #   label = str_wrap("Chance Performance", width = 25),
  #   hjust = "left",
  #   family = "Times",
  #   fontface = "italic",
  #   lineheight = 1,
  #   size = 5
  # )
  facet_wrap(~target_construction) +
  # scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill"), direction = -1) +
  scale_color_manual(values = c("#7570b3","#d95f02","#e7298a","#1b9e77"), aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(21, 22, 23, 24)) +
  scale_y_continuous(limits = c(0, 1.0)) +
  theme_bw(base_size = 16, base_family="Times") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(color = "black", size = 11)
    # axis.text.x = element_markdown(color = "black")
  ) +
  labs(
    x = "Train Condition",
    y = "Accuracy\n(3 LM runs)"
  )

ggsave("slides/aann-no-aann.pdf", height = 3.53, width = 4.24)


## slides full - no noise

noised_and_all %>%
  filter(is.na(noise)) %>%
  ggplot(aes(train_condition, accuracy, color = color)) +
  geom_hline(yintercept=0, linetype = "dotdash", color = "gray") +
  # geom_jitter(aes(shape=shape, fill = fill), size = 3, alpha = 0.7, width = 0.1) +
  geom_point(aes(shape=shape, fill = fill), size = 3, alpha = 0.7) +
  # geom_hline(yintercept = 0.0625, linetype = "dashed") +
  geom_hline(yintercept = 0.2, linetype = "dashed") +
  geom_hline(
    data = results %>% 
      filter(str_detect(suffix, "gpt2"), target_construction == "aann") %>%
      select(-train_construction) %>%
      mutate(target_construction = "Test on AANN"),
    aes(yintercept = overall),
    linetype = "dotted",
    color = "firebrick"
  ) +
  geom_hline(
    data = results %>% 
      filter(str_detect(suffix, "Llama"), target_construction == "aann") %>%
      select(-train_construction) %>%
      mutate(target_construction = "Test on AANN"),
    aes(yintercept = overall),
    linetype = "dotted",
    color = "steelblue"
  ) +
  geom_text(
    data = tibble(
      train_condition = "NAAN",
      accuracy = 0.9,
      target_construction="Test on AANN"
    ),
    label = "Llama-2-7B",
    color = "steelblue",
    family = "Times",
    size = 3.5
  ) +
  geom_text(
    data = tibble(
      train_condition = "NAAN",
      accuracy = 0.73,
      target_construction="Test on AANN"
    ),
    label = "GPT-2 XL",
    color = "firebrick",
    family = "Times",
    size = 3.5
  ) +
  geom_curve(
    xend = 3.1, yend = 0.14, 
    x = 2.7, y = 0.001, 
    curvature = -0.3, 
    arrow = arrow(length = unit(2, "mm")), 
    data = tibble(train_condition="AANN", target_construction="Test on AANN"),
    color = "grey"
  ) +
  geom_text(
    data = tibble(
      train_condition = "AANN",
      accuracy = 0.14,
      target_construction="Test on AANN"
    ),
    x = 3.8,
    label = "2 & 4-gram",
    color = "darkgrey",
    family = "Times",
    size = 4
  ) +
  # geom_curve(
  #   xend = 1.6, yend = 0.14, 
  #   x = 2, y = 0.063, 
  #   curvature = 0.3, 
  #   arrow = arrow(length = unit(2, "mm")), 
  #   data = tibble(train_condition="AANN", target_construction="Test on AANN"),
  #   color = "black"
  # ) +
  # geom_text(
  #   data = tibble(
  #     train_condition = "AANN",
  #     accuracy = 0.14,
  #     target_construction="Test on AANN"
  #   ),
  #   x = 1.1,
  #   label = "Chance",
  #   color = "black",
  #   family = "Times",
  #   size = 4
  # ) +
  geom_curve(
    xend = 1.6, yend = 0.14, 
    x = 2, y = 0.2, 
    curvature = -0.25, 
    arrow = arrow(length = unit(2, "mm")), 
    data = tibble(train_condition="AANN", target_construction="Test on AANN"),
    color = "black"
  ) +
  geom_text(
    data = tibble(
      train_condition = "AANN",
      accuracy = 0.14,
      target_construction="Test on AANN"
    ),
    x = 1.2,
    label = "Chance",
    color = "black",
    family = "Times",
    size = 4
  ) +
  # geom_point(data = noised_results %>% filter(noise == "ANAN"), shape = 23, size = 3, fill = "#ae4d9f", color = "#ae4d9f", alpha = 0.7) +
  # geom_point(data = noised_results %>% filter(noise == "NAAN"), shape = 24, size = 3, fill = "#488795", color = "#488795", alpha = 0.7) +
  facet_wrap(~target_construction) +
  # scale_color_manual(values = c("#7570b3","#d95f02","#e7298a","#1b9e77"), aesthetics = c("color", "fill")) +
  # scale_shape_manual(values = c(21, 22, 23, 24)) +
  scale_color_identity(aesthetics = c("color", "fill")) +
  scale_shape_identity()+
  scale_y_continuous(limits = c(0.0, 1.0)) +
  theme_bw(base_size = 17, base_family="Times") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(color = "black", size = 11)
    # axis.text.x = element_markdown(color = "black")
  ) +
  labs(
    x = "Train Condition",
    y = "Accuracy\n(3 LM runs)"
  )

set.seed(1024)

# ggsave("paper/camera-ready/counterfactualaccuracies.pdf", height = 3.06, width = 9.17, dpi = 300, device=cairo_pdf)
ggsave("slides/counterfactualaccuracies-no-noise.pdf", height = 3.64, width = 10.89, dpi = 300, device=cairo_pdf)

## SLORs

final_fourgram_slors <- fourgram_slors %>%
  ungroup() %>%
  filter(!str_detect(model, "(naan|anan|old)")) %>%
  filter(!str_detect(model, "prototypical")) %>%
  mutate(
    # logprob = logprob/log(2),
    suffix = str_remove(suffix, "(counterfactual-babylm-indef-|counterfactual-babylm-|counterfactual_babylm_)") %>%
      str_remove("aann-"),
    suffix = factor(
      suffix,
      levels = c("babylm","adj_num_freq_balanced", "aann_dtanns", "new_regex_aanns_removal", "indef_articles_with_pl_nouns_removal_new", "measure_nps_as_singular_new", "random_removal", "only_random_removal", "only_other_det_removal", "only_indef_articles_with_pl_nouns_removal", "only_measure_nps_as_singular_removal"),
      labels = c("Unablated", "A/An Adj-Num\nFreq Balanced", "No DT-ANNs", "No AANNs", "No A few/couple/\ndozen/etc. NNS", "No Measure\nNNS as Singular", "Random\nRemoval", "Onlyrandom\nRemoval", "onlyNo DT-ANNs", "onlyNo A few/couple/\ndozen/etc. NNS", "onlyNo Measure\nNNS as Singular")
    )
  ) %>%
  filter(!is.na(suffix)) %>%
  group_by(model, suffix) %>%
  summarize(
    slor = mean(score),
    ste = 1.96 * plotrix::std.error(score)
  ) %>%
  ungroup() %>%
  mutate(
    suffix = fct_reorder(suffix, slor, .fun=sum)
  )

final_slors <- slors %>%
  ungroup() %>%
  filter(!str_detect(model, "(naan|anan|old)"), target_construction == "aann") %>%
  filter(!str_detect(model, "prototypical")) %>%
  # filter(surface_form == "construction_score") %>% 
  select(model, suffix, seed, idx, lr, score = construction_score) %>%
  mutate(
    # logprob = logprob/log(2),
    suffix = str_remove(suffix, "(counterfactual-babylm-indef-|counterfactual-babylm-|counterfactual_babylm_)") %>%
      str_remove("aann-"),
    suffix = factor(
      suffix,
      levels = c("babylm","adj_num_freq_balanced", "aann_dtanns", "new_regex_aanns_removal", "indef_articles_with_pl_nouns_removal_new", "measure_nps_as_singular_new", "random_removal", "only_random_removal", "only_other_det_removal", "only_indef_articles_with_pl_nouns_removal", "only_measure_nps_as_singular_removal"),
      labels = c("Unablated", "A/An Adj-Num\nFreq Balanced", "No DT-ANNs", "No AANNs", "No A few/couple/\ndozen/etc. NNS", "No Measure\nNNS as Singular", "Onlyrandom\nRemoval", "Random\nRemoval", "onlyNo DT-ANNs", "onlyNo A few/couple/\ndozen/etc. NNS", "onlyNo Measure\nNNS as Singular")
    )
  ) %>%
  filter(!is.na(suffix)) %>%
  group_by(model, suffix) %>%
  summarize(
    slor = mean(score),
    ste = 1.96 * plotrix::std.error(score)
  ) %>%
  ungroup() %>%
  mutate(
    suffix = fct_reorder(suffix, slor, .fun=sum)
  )


final_slors %>% count(suffix)

model_pairings <- tribble(
  ~suffix, ~condition,
  "Unablated", "s",
  "A/An Adj-Num\nFreq Balanced", "n",
  "No DT-ANNs", "n",
  "No A few/couple/\ndozen/etc. NNS", "n",
  "No Measure\nNNS as Singular", "n",
  "No AANNs", "n",
  "onlyNo A few/couple/\ndozen/etc. NNS", "s",
  "onlyNo Measure\nNNS as Singular", "s",
  "onlyNo DT-ANNs", "s",
  "Onlyrandom\nRemoval", "s",
  "Random\nRemoval", "n"
) %>%
  mutate(condition = case_when(condition == "s" ~ "AANNs seen during training", TRUE ~ "AANNs removed from training"))

lm_plot <- final_slors %>%
  inner_join(model_pairings) %>%
  mutate(
    LM = "Our LMs",
    suffix = str_remove(suffix, "only") %>% str_replace("Onlyr", "R"),
    suffix = factor(suffix, levels = rev(c("Unablated", "No AANNs", "No DT-ANNs", "No A few/couple/\ndozen/etc. NNS", "No Measure\nNNS as Singular", "A/An Adj-Num\nFreq Balanced", "Random\nRemoval")))
  ) %>%
  ggplot(aes(slor, suffix, shape = condition, color = condition, fill = condition)) +
  geom_vline(aes(xintercept=slor), data = final_slors %>% filter(suffix=="Unablated"), linetype = "dotted") +
  geom_point(size=3, alpha = 0.7) +
  geom_linerange(aes(xmax = slor + ste, xmin = slor - ste), alpha = 0.7) +
  facet_wrap(~LM) +
  scale_shape_manual(values = c(22, 21)) +
  scale_color_manual(values = c("#d95f02", "#7570b3"), aesthetics = c("color", "fill")) +
  scale_x_continuous(breaks = scales::pretty_breaks(6), limits = c(1.2, 2.2)) +
  # scale_color_brewer(aesthetics = c("color", "fill"), palette = "Dark2") +
  theme_bw(base_size = 17, base_family = "Times") +
  theme(
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(color = "black"),
    legend.position = "top",
    panel.background = element_rect(fill='transparent', color=NA), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    legend.background = element_rect(fill='transparent', color = NA), #transparent legend bg
    legend.box.background = element_rect(fill='transparent', color=NA) #transparent legend panel
    # axis.text.y = element_text(angle=15)
  ) +
  labs(
    color = "Condition",
    fill = "Condition",
    shape = "Condition",
    x = "Avg. SLOR (95% CI, 3 LM Runs)"
  )

fourgram_plot <- final_fourgram_slors %>%
  inner_join(model_pairings) %>%
  mutate(
    LM = "4-gram Baselines",
    suffix = str_remove(suffix, "only") %>% str_replace("Onlyr", "R"),
    suffix = factor(suffix, levels = rev(c("Unablated", "No AANNs", "No DT-ANNs", "No A few/couple/\ndozen/etc. NNS", "No Measure\nNNS as Singular", "A/An Adj-Num\nFreq Balanced", "Random\nRemoval")))
  ) %>%
  ggplot(aes(slor, suffix, shape = condition, color = condition, fill = condition)) +
  geom_vline(aes(xintercept=slor), data = final_fourgram_slors %>% filter(suffix=="Unablated"), linetype = "dotted") +
  geom_point(size=3, alpha = 0.7) +
  geom_linerange(aes(xmax = slor + ste, xmin = slor - ste), alpha = 0.7) +
  facet_wrap(~LM) +
  scale_shape_manual(values = c(22, 21)) +
  scale_x_continuous(breaks = scales::pretty_breaks(6), limits = c(5.4, 6.4)) +
  # scale_color_brewer(aesthetics = c("color", "fill"), palette = "Dark2") +
  scale_color_manual(values = c("#d95f02", "#7570b3"), aesthetics = c("color", "fill")) +
  theme_bw(base_size = 17, base_family = "Times") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(color = "black"),
    legend.position = "top",
    # axis.text.y = element_text(angle=15),
    panel.background = element_rect(fill='transparent', color=NA), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    legend.background = element_rect(fill='transparent', color = NA), #transparent legend bg
    legend.box.background = element_rect(fill='transparent', color=NA) #transparent legend panel
  ) +
  labs(
    color = "Condition",
    fill = "Condition",
    shape = "Condition",
    x = "Avg. SLOR (95% CI)"
  )

combined <- lm_plot + fourgram_plot & theme(legend.position = "top",
                                            legend.box.margin = margin(t=-5, b=-5), 
                                            panel.background = element_rect(fill='transparent', color=NA),
                                            plot.background = element_rect(fill='transparent', color=NA), 
                                            legend.background = element_rect(fill='transparent', color = NA), 
                                            legend.box.background = element_rect(fill='transparent', color=NA))
combined_plot <- combined + plot_layout(guides="collect")

# ggsave("paper/camera-ready/ourlms_vs_fourgrams.pdf", height = 5.00, width = 12.10, dpi=300, device=cairo_pdf)
ggsave("slides/ourlms_vs_fourgrams.pdf", combined_plot, height = 5.00, width = 12.10, dpi=300, device=cairo_pdf)


final_slor_accuracies <- slors %>%
  ungroup() %>%
  filter(!str_detect(model, "(naan|anan|old)"), target_construction == "aann") %>%
  filter(!str_detect(model, "prototypical")) %>%
  mutate(
    correctness = construction_score > order_swap_score & 
      construction_score > no_article_score & 
      construction_score > no_numeral_score & 
      construction_score > no_modifier_score,
    order_swap = construction_score > order_swap_score
  ) %>%
  group_by(model, suffix, lr) %>%
  summarize(
    result = Hmisc::smean.cl.boot(correctness) %>% t %>% as.data.frame,
    order_swap = mean(order_swap)
  ) %>%
  unnest(result) %>%
  # summarize(
  #   accuracy = mean(correctness)
  # ) %>%
  # filter(surface_form == "construction_score") %>% 
  # select(model, suffix, lr, Mean, ) %>%
  mutate(
    # logprob = logprob/log(2),
    suffix = str_remove(suffix, "(counterfactual-babylm-indef-|counterfactual-babylm-|counterfactual_babylm_)") %>%
      str_remove("aann-"),
    suffix = factor(
      suffix,
      levels = c("babylm","adj_num_freq_balanced", "aann_dtanns", "new_regex_aanns_removal", "indef_articles_with_pl_nouns_removal_new", "measure_nps_as_singular_new", "random_removal", "only_random_removal", "only_other_det_removal", "only_indef_articles_with_pl_nouns_removal", "only_measure_nps_as_singular_removal"),
      labels = c("Unablated", "A/An Adj-Num\nFreq Balanced", "No DT-ANNs", "No AANNs", "No A few/couple/\ndozen/etc. NNS", "No Measure\nNNS as Singular", "Onlyrandom\nRemoval", "Random\nRemoval", "onlyNo DT-ANNs", "onlyNo A few/couple/\ndozen/etc. NNS", "onlyNo Measure\nNNS as Singular")
    )
  ) %>%
  filter(!is.na(suffix))

final_slor_accuracies %>%
  inner_join(model_pairings) %>%
  ungroup() %>%
  mutate(
    suffix = str_remove(suffix, "only") %>% str_replace("Onlyr", "R"),
    suffix = factor(suffix, levels = rev(c("Unablated", "No AANNs", "No DT-ANNs", "No A few/couple/\ndozen/etc. NNS", "No Measure\nNNS as Singular", "A/An Adj-Num\nFreq Balanced", "Random\nRemoval")))
  ) %>%
  ggplot(aes(Mean, suffix, shape = condition, color = condition, fill = condition)) +
  geom_point(size=3, alpha = 0.7) +
  geom_linerange(aes(xmax = Upper, xmin = Lower), alpha = 0.7) +
  geom_vline(xintercept = 0.2, linetype="dashed") +
  scale_shape_manual(values = c(22, 21)) +
  scale_color_manual(values = c("#d95f02", "#7570b3"), aesthetics = c("color", "fill")) +
  # scale_x_continuous(breaks = seq(0.25, 0.75, by = 0.1), limits = c(0.22, 0.79)) +
  scale_x_continuous(breaks = scales::pretty_breaks(6), limits = c(0.1, 0.8)) +
  # scale_color_brewer(aesthetics = c("color", "fill"), palette = "Dark2") +
  # guides(
  #   color = guide_legend(nrow = 2),
  #   fill = guide_legend(nrow = 2),
  #   shape = guide_legend(nrow = 2)
  # ) +
  theme_bw(base_size = 17, base_family = "Times") +
  theme(
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(color = "black"),
    # legend.position = "top"
    # axis.text.y = element_text(angle=15)
  ) +
  labs(
    color = "Condition",
    fill = "Condition",
    shape = "Condition",
    x = "Accuracy (3 LM Runs, Boostrap CIs)"
  )


## fourgram accs?

final_fourgram_slor_accuracies <- fourgram_scores %>%
  inner_join(unigram_scores %>% select(-model) %>% rename_with(function(x) {str_c("ngram_", x)}, ends_with("score")), by = c("idx", "suffix")) %>%
  mutate(
    construction_score = construction_score - ngram_construction_score,
    default_nan_score = default_nan_score - ngram_default_nan_score,
    order_swap_score = order_swap_score - ngram_order_swap_score,
    no_article_score = no_article_score - ngram_no_article_score,
    no_modifier_score = no_modifier_score - ngram_no_modifier_score,
    no_numeral_score = no_numeral_score - ngram_no_numeral_score,
  ) %>%
  filter(idx %in% good_ids) %>%
  select(-starts_with("ngram")) %>%
  ungroup() %>%
  filter(!str_detect(model, "(naan|anan|old)")) %>%
  filter(!str_detect(model, "prototypical")) %>%
  mutate(
    correctness = construction_score > order_swap_score & 
      construction_score > no_article_score & 
      construction_score > no_numeral_score & 
      construction_score > no_modifier_score,
    order_swap = construction_score > order_swap_score
  ) %>%
  group_by(model, suffix) %>%
  summarize(
    result = Hmisc::smean.cl.boot(correctness) %>% t %>% as.data.frame,
    order_swap = mean(order_swap)
  ) %>%
  unnest(result) %>%
  mutate(
    suffix = str_remove(suffix, "(counterfactual-babylm-indef-|counterfactual-babylm-|counterfactual_babylm_)") %>%
      str_remove("aann-"),
    suffix = factor(
      suffix,
      levels = c("babylm","adj_num_freq_balanced", "aann_dtanns", "new_regex_aanns_removal", "indef_articles_with_pl_nouns_removal_new", "measure_nps_as_singular_new", "random_removal", "only_random_removal", "only_other_det_removal", "only_indef_articles_with_pl_nouns_removal", "only_measure_nps_as_singular_removal"),
      labels = c("Unablated", "A/An Adj-Num\nFreq Balanced", "No DT-ANNs", "No AANNs", "No A few/couple/\ndozen/etc. NNS", "No Measure\nNNS as Singular", "Onlyrandom\nRemoval", "Random\nRemoval", "onlyNo DT-ANNs", "onlyNo A few/couple/\ndozen/etc. NNS", "onlyNo Measure\nNNS as Singular")
    )
  ) %>%
  filter(!is.na(suffix))

final_slor_accuracies %>%
  inner_join(model_pairings) %>%
  ungroup() %>%
  mutate(
    suffix = str_remove(suffix, "only") %>% str_replace("Onlyr", "R"),
    suffix = factor(suffix, levels = rev(c("Unablated", "No AANNs", "No DT-ANNs", "No A few/couple/\ndozen/etc. NNS", "No Measure\nNNS as Singular", "A/An Adj-Num\nFreq Balanced", "Random\nRemoval")))
  ) %>%
  ggplot(aes(Mean, suffix, shape = condition, color = condition, fill = condition)) +
  geom_point(size=3, alpha = 0.7) +
  geom_linerange(aes(xmax = Upper, xmin = Lower), alpha = 0.7) +
  geom_vline(xintercept = 0.2, linetype="dashed") +
  scale_shape_manual(values = c(22, 21)) +
  scale_color_manual(values = c("#d95f02", "#7570b3"), aesthetics = c("color", "fill")) +
  # scale_x_continuous(breaks = seq(0.25, 0.75, by = 0.1), limits = c(0.22, 0.79)) +
  scale_x_continuous(breaks = scales::pretty_breaks(6), limits = c(0.1, 0.8)) +
  # scale_color_brewer(aesthetics = c("color", "fill"), palette = "Dark2") +
  # guides(
  #   color = guide_legend(nrow = 2),
  #   fill = guide_legend(nrow = 2),
  #   shape = guide_legend(nrow = 2)
  # ) +
  theme_bw(base_size = 17, base_family = "Times") +
  theme(
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(color = "black"),
    # legend.position = "top"
    # axis.text.y = element_text(angle=15)
  ) +
  labs(
    color = "Condition",
    fill = "Condition",
    shape = "Condition",
    x = "Accuracy (3 LM Runs, Boostrap CIs)"
  )



# variability

productivity_ref_results <- slors %>%
  filter(model %in% c("smolm-aann", "smolm-new_regex_aanns_removal")) %>%
  filter(target_construction=="aann") %>%
  group_by(model) %>%
  summarize(
    ste = 1.96 * plotrix::std.error(construction_score),
    slor = mean(construction_score)
  ) %>%
  mutate(
    productivity = case_when(
      model == "smolm-aann" ~ "Unablated",
      TRUE ~ "No AANNs"
    ),
    condition = case_when(
      model == "smolm-aann" ~ "Unablated",
      TRUE ~ "No AANNs"
    )
  ) %>%
  select(-model)

productivity_results <- slors %>%
  filter(str_detect(model, "variability")) %>%
  mutate(
    productivity = case_when(
      str_detect(suffix, "high") ~ "Low", # named models and data in the reversed manner :facepalm:
      TRUE ~ "High"
    ),
    condition = str_extract(suffix, "(all|adj|numeral|noun)")
  ) %>%
  group_by(productivity, condition) %>%
  summarize(
    ste = 1.96 * plotrix::std.error(construction_score),
    slor = mean(construction_score)
  ) %>%
  ungroup() %>%
  mutate(
    condition = str_to_title(condition),
  )

bind_rows(
  productivity_ref_results,
  productivity_results
) %>%
  mutate(
    productivity = factor(productivity, levels = c("No AANNs", "Low", "High", "Unablated")),
    condition = factor(
      condition, 
      levels = rev(c("Unablated", "All", "Adj", "Numeral", "Noun", "No AANNs")),
      labels = rev(c("Unablated", "All Open Slots", "Adj Slot", "Num Slot", "Noun Slot", "No AANNs"))
    )
  ) %>%
  ggplot(aes(slor, condition, group = condition, color = productivity, shape = productivity, fill = productivity)) +
  geom_point(size = 3,alpha = 0.7) +
  geom_linerangeh(aes(xmin = slor - ste, xmax = slor + ste),alpha = 0.7) +
  scale_color_manual(values = c("#d95f02", "#ffa450", "#9892d3", "#7570b3"), aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(22, 23, 24, 21)) +
  scale_x_continuous(limits = c(1.6, 2.2), breaks = scales::pretty_breaks()) +
  theme_bw(base_size = 17, base_family="Times") +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title.y = element_blank(),
    legend.box.margin = margin(t=-15, b=-15)
    # axis.text.x = element_markdown(color = "black")
  ) +
  guides(
    color = guide_legend(title.position = "top", title.hjust=0.5), 
    shape = guide_legend(title.position= "top", title.hjust=0.5),
    fill = guide_legend(title.position= "top", title.hjust=0.5)
  ) +
  labs(
    color = "Relative Variability of Observed AANNs",
    shape = "Relative Variability of Observed AANNs",
    fill = "Relative Variability of Observed AANNs",
    x = "Avg. SLOR (95% CI; 3 LM Runs)",
    y = "Condition"
  )


# 721,371
ggsave("paper/camera-ready/variability.pdf", height = 3.20, width = 5.50, dpi = 300, device = cairo_pdf)
# semantics?

raw_slors <- scores %>%
  # filter(idx %in% good_ids) %>%
  select(-train_construction) %>%
  mutate(
    lr = str_extract(model, "\\de-\\d"),
    suffix = str_remove(suffix, "-\\de-\\d.csv"),
    model = str_remove(model, "-\\de-\\d")
  ) %>% 
  inner_join(best_models) %>%
  inner_join(unigram_scores %>% select(-model) %>% rename_with(function(x) {str_c("ngram_", x)}, ends_with("score")), by = c("idx", "suffix")) %>%
  mutate(
    construction_score = construction_score - ngram_construction_score,
    order_swap_score = order_swap_score - ngram_order_swap_score,
    no_article_score = no_article_score - ngram_no_article_score,
    no_modifier_score = no_modifier_score - ngram_no_modifier_score,
    no_numeral_score = no_numeral_score - ngram_no_numeral_score,
  )


raw_slors %>%
  filter(suffix %in% c("babylm", "counterfactual-babylm-new_regex_aanns_removal"), target_construction == "aann") %>%
  inner_join(human_data) %>%
  filter(adjclass %in% c("quant", "ambig", "human", "qual", "stubborn", "color")) %>%
  group_by(suffix, nounclass, adjclass) %>%
  summarize(
    ste = 1.96 * plotrix::std.error(construction_score),
    slor = mean(construction_score)
  ) %>%
  ungroup() %>%
  filter(nounclass %in% c("objects")) %>%
  mutate(
    model = factor(suffix, levels = c("babylm", "counterfactual-babylm-new_regex_aanns_removal"), labels = c("Unablated", "No AANNs")),
    adjclass = case_when(
      adjclass == "stubborn" ~ "Stubborn<br>(<span style='font-size: 11pt;'>e.g., <em>a <b>tall</b> five pencils</em></span>)",
      adjclass == "color" ~ "Color<br>(<span style='font-size: 11pt;'>e.g., <em>a <b>blue</b> five pencils</em></span>)",
      adjclass == "qual" ~ "Qualitative<br>(<span style='font-size: 11pt;'>e.g., <em>a <b>lovely</b> five pencils</em></span>)",
      adjclass == "ambig" ~ "Ambiguous<br>(<span style='font-size: 11pt;'>e.g., <em>a <b>mediocre</b> five pencils</em></span>)",
      adjclass == "quant" ~ "Quantitative<br>(<span style='font-size: 11pt;'>e.g., <em>a <b>paltry</b> five pencils</em></span>)",
    ),
    adjclass = fct_reorder(adjclass, slor, .fun=sum)
  ) %>% 
  ggplot(aes(slor, adjclass, color = model, fill = model, shape = model)) +
  geom_point(size = 2.5) +
  # geom_linerange(aes(ymin = slor-ste, ymax=slor+ste)) +
  geom_linerangeh(aes(xmin = slor-ste, xmax=slor+ste)) +
  # geom_line(aes(group = model)) +
  # geom_col(position = position_dodge(0.9)) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill"), direction = -1) +
  scale_shape_manual(values = c(24, 23)) +
  # scale_shape_manual(values = c(21, 22, 23, 24), guide="none") +
  # facet_wrap(~ nounclass, nrow = 1, scales = "free_x") +
  theme_bw(base_size = 16, base_family = "Times") +
  theme(
    legend.position = "top",
    # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    # axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_markdown(color="black"),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "black")
  ) +
  labs(
    # y = "Avg. SLOR (95% CI)",
    x = "Avg. SLOR (95% CI)",
    color = "Ablation",
    fill = "Ablation",
    shape = "Ablation"
  )

human_data %>%
  filter(nounclass == "objects") %>%
  group_by(adjclass) %>%
  summarize(
    ste = 1.96 * plotrix::std.error(rating),
    rating = mean(rating)
  ) %>%
  ungroup() %>%
  mutate(
    adjclass = factor(adjclass, levels=rev(c("quant", "ambig", "qual", "color", "stubborn")), 
                      labels = c("Stubborn<br>(<span style='font-size: 11pt;'>e.g., <em>a <b>tall</b> five pencils</em></span>)",
                                 "Color<br>(<span style='font-size: 11pt;'>e.g., <em>a <b>blue</b> five pencils</em></span>)",
                                 "Qualitative<br>(<span style='font-size: 11pt;'>e.g., <em>a <b>lovely</b> five pencils</em></span>)",
                                 "Ambiguous<br>(<span style='font-size: 11pt;'>e.g., <em>a <b>mediocre</b> five pencils</em></span>)",
                                 "Quantitative<br>(<span style='font-size: 11pt;'>e.g., <em>a <b>paltry</b> five pencils</em></span>)")),
  ) %>%
  ggplot(aes(rating, adjclass)) +
  geom_point(size = 2.5) +
  geom_linerangeh(aes(xmin = rating-ste, xmax=rating+ste)) +
  scale_x_continuous(limits = c(1,9), breaks=c(1,3,5,7,9)) +
  theme_bw(base_size = 16, base_family = "Times") +
  theme(
    legend.position = "top",
    # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    # axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_markdown(color="black"),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "black"),
  )

# combined

object_slors <- raw_slors %>%
  filter(suffix %in% c("babylm", "counterfactual-babylm-new_regex_aanns_removal"), target_construction == "aann") %>%
  inner_join(human_data) %>%
  filter(adjclass %in% c("quant", "ambig", "human", "qual", "stubborn", "color")) %>%
  group_by(suffix, nounclass, adjclass) %>%
  summarize(
    ste = 1.96 * plotrix::std.error(construction_score),
    slor = mean(construction_score)
  ) %>%
  ungroup() %>%
  filter(nounclass %in% c("objects")) %>%
  mutate(
    model = factor(suffix, levels = c("babylm", "counterfactual-babylm-new_regex_aanns_removal"), labels = c("Unablated LM", "LM with No AANNs")),
  ) %>%
  select(model, adjclass, rating=slor, ste) %>%
  mutate(system = "LMs")

human_ratings <- human_data %>%
  filter(nounclass == "objects") %>%
  group_by(adjclass) %>%
  summarize(
    ste = 1.96 * plotrix::std.error(rating),
    rating = mean(rating)
  ) %>%
  ungroup() %>%
  mutate(model = "Human", system = "Human")

bind_rows(object_slors, human_ratings) %>%
  mutate(
    adjclass = factor(adjclass, levels=rev(c("quant", "ambig", "qual", "color", "stubborn")), 
                      labels = c("Stubborn<br>(<span style='font-size: 11pt;'>e.g., <em>a <b>tall</b> five pencils</em></span>)",
                                 "Color<br>(<span style='font-size: 11pt;'>e.g., <em>a <b>blue</b> five pencils</em></span>)",
                                 "Qualitative<br>(<span style='font-size: 11pt;'>e.g., <em>a <b>lovely</b> five pencils</em></span>)",
                                 "Ambiguous<br>(<span style='font-size: 11pt;'>e.g., <em>a <b>mediocre</b> five pencils</em></span>)",
                                 "Quantitative<br>(<span style='font-size: 11pt;'>e.g., <em>a <b>paltry</b> five pencils</em></span>)")),
  ) %>%
  ggplot(aes(rating, adjclass, fill = model, shape = model, color = model)) +
  geom_point(size = 2.5) +
  geom_linerangeh(aes(xmin = rating-ste, xmax=rating+ste)) +
  facet_wrap(~system, scales = "free_x") +
  scale_color_manual(values = c("Human" = "black", "Unablated LM" = "#7570b3", "LM with No AANNs" = "#d95f02"), aesthetics = c("color", "fill")) +
  # scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill"), direction = -1) +
  scale_shape_manual(values = c(21, 23, 24)) +
  scale_x_continuous(breaks = scales::pretty_breaks(5)) +
  # scale_x_continuous(limits = c(1,9), breaks=c(1,3,5,7,9)) +
  theme_bw(base_size = 16, base_family = "Times") +
  theme(
    legend.position = "top",
    axis.title.y = element_blank(),
    axis.text.y = element_markdown(color="black"),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "black"),
    legend.title = element_blank()
  ) +
  labs(
    x = "Ratings (Human) / Avg. SLOR (3 LM Runs)"
  )

ggsave("paper/camera-ready/object-ratings.pdf", height=4.26, width=8.74, dpi=300, device=cairo_pdf)


bind_rows(object_slors, human_ratings) %>%
  mutate(
    adjclass = factor(adjclass, levels=rev(c("quant", "ambig", "qual", "color", "stubborn")), 
                      labels = c("Stubborn<br>(<span style='font-size: 11pt;'><em>a <b>tall</b> five pencils</em></span>)",
                                 "Color<br>(<span style='font-size: 11pt;'><em>a <b>blue</b> five pencils</em></span>)",
                                 "Qualitative<br>(<span style='font-size: 11pt;'><em>a <b>lovely</b> five pencils</em></span>)",
                                 "Ambiguous<br>(<span style='font-size: 11pt;'><em>a <b>mediocre</b> five pencils</em></span>)",
                                 "Quantitative<br>(<span style='font-size: 11pt;'><em>a <b>paltry</b> five pencils</em></span>)")),
  ) %>%
  mutate(
    model = factor(model, levels = c("Human", "LM with No AANNs", "Unablated LM"), labels = c("Human", "No AANNs", "Unablated")),
  ) %>%
  ggplot(aes(rating, adjclass, fill = model, shape = model, color = model)) +
  geom_point(size = 2.5) +
  geom_linerangeh(aes(xmin = rating-ste, xmax=rating+ste)) +
  facet_wrap(~system, scales = "free_x", nrow = 2) +
  scale_color_manual(values = c("Human" = "black", "Unablated" = "#7570b3", "No AANNs" = "#d95f02"), aesthetics = c("color", "fill")) +
  # scale_color_manual(values = c("Human" = "black", "Unablated LM" = "#7570b3", "LM with No AANNs" = "#d95f02"), aesthetics = c("color", "fill")) +
  # scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill"), direction = -1) +
  scale_shape_manual(values = c(21, 23, 24)) +
  scale_x_continuous(breaks = scales::pretty_breaks(5)) +
  # scale_x_continuous(limits = c(1,9), breaks=c(1,3,5,7,9)) +
  theme_bw(base_size = 16, base_family = "Times") +
  theme(
    legend.position = "top",
    axis.title.y = element_blank(),
    axis.text.y = element_markdown(color="black"),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "black"),
    legend.title = element_blank()
  ) +
  labs(
    x = "Ratings (Human) / Avg. SLOR (3 LM Runs)"
  )

# ggsave("paper/camera-ready/object-ratings-vertical.pdf", height=7.31, width=5.59, dpi=300, device=cairo_pdf)
ggsave("paper/camera-ready/object-ratings-vertical.pdf", height=8.00, width=5.81, dpi=300, device=cairo_pdf)


# all model-human comparisons

noun_adj_examples <- tribble(
  ~nounclass, ~adjclass, ~example,
  "objects", "stubborn", "Stubborn<br>(<span style='font-size: 11pt;'><em>a <b>tall</b> five <b>pencils</b></em></span>)",
  "objects", "color","Color<br>(<span style='font-size: 11pt;'><em>a <b>blue</b> five <b>pencils</b></em></span>)",
  "objects", "qual","Qualitative<br>(<span style='font-size: 11pt;'><em>a <b>lovely</b> five <b>pencils</b></em></span>)",
  "objects", "ambig","Ambiguous<br>(<span style='font-size: 11pt;'><em>a <b>mediocre</b> five <b>pencils</b></em></span>)",
  "objects", "quant","Quantitative<br>(<span style='font-size: 11pt;'><em>a <b>paltry</b> five <b>pencils</b></em></span>)",
  "art", "quant", "Quantitative<br>(<span style='font-size: 11pt;'><em>a <b>staggering</b> twenty <b>operas</b></em></span>)",
  "art", "qual", "Qualitative<br>(<span style='font-size: 11pt;'><em>a <b>charming</b> five <b>operas</b></em></span>)",
  "art", "ambig", "Ambiguous<br>(<span style='font-size: 11pt;'><em>a <b>devastating</b> three <b>operas</b></em></span>)",
  "distance", "quant", "Quantitative<br>(<span style='font-size: 11pt;'><em>a <b>meager</b> three <b>blocks</b></em></span>)",
  "distance", "qual", "Qualitative<br>(<span style='font-size: 11pt;'><em>a <b>hideous</b> twenty <b>blocks</b></em></span>)",
  "distance", "ambig", "Ambiguous<br>(<span style='font-size: 11pt;'><em>an <b>astonishing</b> three <b>blocks</b></em></span>)",
  "human", "quant", "Quantitative<br>(<span style='font-size: 11pt;'><em>a <b>whopping</b> twenty <b>pianists</b></em></span>)",
  "human", "qual", "Qualitative<br>(<span style='font-size: 11pt;'><em>an <b>uninviting</b> three <b>pianists</b></em></span>)",
  "human", "ambig", "Ambiguous<br>(<span style='font-size: 11pt;'><em>a <b>surprising</b> twenty <b>pianists</b></em></span>)",
  "human", "human", "Human<br>(<span style='font-size: 11pt;'><em>a <b>talented</b> five <b>pianists</b></em></span>)",
  "human", "stubborn", "Stubborn<br>(<span style='font-size: 11pt;'><em>a <b>large</b> five <b>pianists</b></em></span>)",
  "temporal", "quant", "Quantitative<br>(<span style='font-size: 11pt;'><em>a <b>mere</b> three <b>hours</b></em></span>)",
  "temporal", "qual", "Qualitative<br>(<span style='font-size: 11pt;'><em>an <b>enchanting</b> five <b>hours</b></em></span>)",
  "temporal", "ambig", "Ambiguous<br>(<span style='font-size: 11pt;'><em>an <b>impressive</b> three <b>hours</b></em></span>)",
  "unit_like", "quant", "Quantitative<br>(<span style='font-size: 11pt;'><em>a <b>hefty</b> three <b>paragraphs</b></em></span>)",
  "unit_like", "qual", "Qualitative<br>(<span style='font-size: 11pt;'><em>a <b>haunting</b> twenty <b>paragraphs</b></em></span>)",
  "unit_like", "ambig", "Ambiguous<br>(<span style='font-size: 11pt;'><em>a <b>pathetic</b> five <b>paragraphs</b></em></span>)",
)

model_human_data <- bind_rows(
  human_data %>%
    filter(adjclass %in% c("quant", "ambig", "human", "qual", "stubborn", "color")) %>%
    mutate(normalized_rating = scale(rating)) %>%
    group_by(nounclass, adjclass) %>%
    summarize(
      ste = 1.96 * plotrix::std.error(rating),
      rating = mean(rating),
      normalized_ste = 1.96 * plotrix::std.error(normalized_rating),
      normalized_rating = mean(normalized_rating)
    ) %>%
    ungroup() %>%
    mutate(model = "Humans", system = "Human"),
  raw_slors %>%
    filter(suffix %in% c("babylm", "counterfactual-babylm-new_regex_aanns_removal"), target_construction == "aann") %>%
    inner_join(human_data) %>%
    filter(adjclass %in% c("quant", "ambig", "human", "qual", "stubborn", "color")) %>%
    mutate(normalized_slor = scale(construction_score)) %>%
    group_by(suffix, nounclass, adjclass) %>%
    summarize(
      ste = 1.96 * plotrix::std.error(construction_score),
      slor = mean(construction_score),
      normalized_ste = 1.96 * plotrix::std.error(normalized_slor),
      normalized_slor = mean(normalized_slor)
    ) %>%
    ungroup() %>%
    mutate(
      model = factor(suffix, levels = c("babylm", "counterfactual-babylm-new_regex_aanns_removal"), labels = c("Unablated", "No AANNs")),
    ) %>%
    select(model, nounclass, adjclass, rating=slor, ste, normalized_rating = normalized_slor, normalized_ste) %>%
    mutate(system = "LMs")
)

model_human_data %>% 
  inner_join(noun_adj_examples) %>%
  mutate(
    nounclass = str_to_title(nounclass) %>% str_replace("_", "-"),
    example = fct_reorder(example, rating, .fun = mean)
  ) %>%
  ggplot(aes(normalized_rating, example, fill = model, shape = model, color = model)) +
  geom_point(size = 2.5) +
  # geom_linerangeh(aes(xmin = rating-ste, xmax=rating+ste)) +
  geom_linerangeh(aes(xmin = normalized_rating-normalized_ste, xmax=normalized_rating+normalized_ste)) +
  # facet_grid(nounclass ~ system, scales = "free") +
  facet_wrap(~nounclass, scales = "free_y",ncol=1) +
  scale_color_manual(values = c("Humans" = "black", "Unablated" = "#7570b3", "No AANNs" = "#d95f02"), aesthetics = c("color", "fill")) +
  # scale_color_manual(values = c("Human" = "black", "Unablated LM" = "#7570b3", "LM with No AANNs" = "#d95f02"), aesthetics = c("color", "fill")) +
  # scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill"), direction = -1) +
  scale_shape_manual(values = c(21, 23, 24)) +
  # scale_x_continuous(limits = c(0, 9)) +
  # scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  scale_x_continuous(breaks = scales::pretty_breaks(), limits = c(-2, 1)) +
  theme_bw(base_size = 15, base_family = "Times") +
  theme(
    legend.position = "top",
    axis.title.y = element_blank(),
    axis.text.y = element_markdown(color="black"),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "black"),
    legend.title = element_blank()
  ) +
  labs(
    # x = "Ratings (Human) / Avg. SLOR (3 LM Runs)"
    x = "z-scored Ratings/SLOR"
  )

ggsave("paper/camera-ready/human_and_models_z_scored.pdf", height = 15.00, width = 5.5, dpi = 300, device=cairo_pdf)


# single plot

model_human_data %>% 
  inner_join(noun_adj_examples) %>%
  mutate(
    nounclass = str_to_title(nounclass) %>% str_replace("_", "-"),
    example = fct_reorder(example, rating, .fun = mean)
  ) %>%
  filter(nounclass == "Human") %>%
  ggplot(aes(normalized_rating, example, fill = model, shape = model, color = model)) +
  geom_point(size = 2.5) +
  # geom_linerangeh(aes(xmin = rating-ste, xmax=rating+ste)) +
  geom_linerangeh(aes(xmin = normalized_rating-normalized_ste, xmax=normalized_rating+normalized_ste)) +
  # facet_grid(nounclass ~ system, scales = "free") +
  # facet_wrap(~nounclass, scales = "free_y",ncol=1) +
  scale_color_manual(values = c("Humans" = "black", "Unablated" = "#7570b3", "No AANNs" = "#d95f02"), aesthetics = c("color", "fill")) +
  # scale_color_manual(values = c("Human" = "black", "Unablated LM" = "#7570b3", "LM with No AANNs" = "#d95f02"), aesthetics = c("color", "fill")) +
  # scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill"), direction = -1) +
  scale_shape_manual(values = c(21, 23, 24)) +
  # scale_x_continuous(limits = c(0, 9)) +
  # scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  scale_x_continuous(breaks = scales::pretty_breaks(), limits = c(-2, 1)) +
  theme_bw(base_size = 15, base_family = "Times") +
  theme(
    legend.position = "top",
    axis.title.y = element_blank(),
    axis.text.y = element_markdown(color="black"),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "black"),
    legend.title = element_blank()
  ) +
  labs(
    # x = "Ratings (Human) / Avg. SLOR (3 LM Runs)"
    x = "z-scored Ratings/SLOR"
  )

# ggsave("paper/camera-ready/human_and_models_z_scored-objects.pdf", height = 4.06, width = 4.97, dpi=300, device=cairo_pdf)
ggsave("paper/camera-ready/human_and_models_z_scored-human.pdf", height = 4.00, width = 5.29, dpi=300, device=cairo_pdf)


model_human_data %>% 
  inner_join(noun_adj_examples) %>%
  mutate(
    nounclass = str_to_title(nounclass) %>% str_replace("_", "-"),
    example = fct_reorder(example, rating, .fun = mean)
  ) %>%
  filter(nounclass == "Human", adjclass %in% c("quant", "qual", "stubborn")) %>%
  ggplot(aes(normalized_rating, example, fill = model, shape = model, color = model)) +
  geom_linerangeh(aes(xmin = normalized_rating-normalized_ste, xmax=normalized_rating+normalized_ste)) +
  geom_point(size = 2.5, alpha = 0.7) +
  scale_color_manual(values = c("Humans" = "black", "Unablated" = "#7570b3", "No AANNs" = "#d95f02"), aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(25, 22, 21)) +
  scale_x_continuous(breaks = scales::pretty_breaks(), limits = c(-2, 1)) +
  theme_bw(base_size = 15, base_family = "Times") +
  theme(
    legend.position = "top",
    axis.title.y = element_blank(),
    axis.text.y = element_markdown(color="black"),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "black"),
    legend.title = element_blank(),
    legend.box.margin = margin(t=-15, b=-15)
  ) +
  labs(
    x = "z-scored Ratings/SLOR"
  )

ggsave("paper/camera-ready/human_and_models_z_scored-main-text.pdf", height = 2.39, width = 5.65, dpi=300, device=cairo_pdf)

model_human_data %>% 
  inner_join(noun_adj_examples) %>%
  mutate(
    nounclass = str_to_title(nounclass) %>% str_replace("_", "-"),
    example = fct_reorder(example, rating, .fun = mean)
  ) %>%
  ggplot(aes(rating, example, fill = model, shape = model, color = model)) +
  geom_point(size = 2.5) +
  geom_linerangeh(aes(xmin = rating-ste, xmax=rating+ste)) +
  facet_grid(nounclass ~ system, scales = "free") +
  scale_color_manual(values = c("Human" = "black", "Unablated" = "#7570b3", "No AANNs" = "#d95f02"), aesthetics = c("color", "fill")) +
  # scale_color_manual(values = c("Human" = "black", "Unablated LM" = "#7570b3", "LM with No AANNs" = "#d95f02"), aesthetics = c("color", "fill")) +
  # scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill"), direction = -1) +
  scale_shape_manual(values = c(21, 23, 24)) +
  # scale_x_continuous(limits = c(0, 9)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  theme_bw(base_size = 15, base_family = "Times") +
  theme(
    legend.position = "top",
    axis.title.y = element_blank(),
    axis.text.y = element_markdown(color="black"),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "black"),
    legend.title = element_blank()
  ) +
  labs(
    x = "Ratings (Human) / Avg. SLOR (3 LM Runs)"
  )
  
ggsave("paper/camera-ready/human_and_models.pdf", height = 13.50, width = 7.00, dpi = 300, device=cairo_pdf)


## ALL ACCURACIES

slors %>%
  ungroup() %>%
  filter(!str_detect(model, "(naan|anan|old)"), target_construction == "aann") %>%
  filter(!str_detect(model, "prototypical")) %>%
  # filter(surface_form == "construction_score") %>% 
  # select(model, suffix, seed, idx, lr, score = construction_score) %>%
  mutate(
    # logprob = logprob/log(2),
    suffix = str_remove(suffix, "(counterfactual-babylm-indef-|counterfactual-babylm-|counterfactual_babylm_)") %>%
      str_remove("aann-"),
    suffix = factor(
      suffix,
      levels = c("babylm","adj_num_freq_balanced", "aann_dtanns", "new_regex_aanns_removal", "indef_articles_with_pl_nouns_removal_new", "measure_nps_as_singular_new", "random_removal", "only_random_removal", "only_other_det_removal", "only_indef_articles_with_pl_nouns_removal", "only_measure_nps_as_singular_removal"),
      labels = c("Unablated", "A/An Adj-Num\nFreq Balanced", "No DT-ANNs", "No AANNs", "No A few/couple/\ndozen/etc. NNS", "No Measure\nNNS as Singular", "Onlyrandom\nRemoval", "Random\nRemoval", "onlyNo DT-ANNs", "onlyNo A few/couple/\ndozen/etc. NNS", "onlyNo Measure\nNNS as Singular")
    )
  ) %>%
  filter(!is.na(suffix)) %>%
  group_by(suffix, seed, target_construction) %>%
  summarize(
    default_preference = mean(default_nan_score > construction_score),
    order_swap = mean(construction_score > order_swap_score),
    no_article = mean(construction_score > no_article_score),
    no_modifier = mean(construction_score > no_modifier_score),
    no_numeral = mean(construction_score > no_numeral_score),
    overall = mean(construction_score > order_swap_score & construction_score > no_article_score & construction_score > no_modifier_score & construction_score > no_numeral_score)
  ) %>% View()

