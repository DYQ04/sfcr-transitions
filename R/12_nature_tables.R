library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(openxlsx)

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
base <- file.path(root, "outputs/reruns/six_country")
nature <- file.path(base, "Nature_R_visualization")
out <- file.path(nature, "tables_nature")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

read_main <- function(x) read_csv(file.path(base, "tables/main", x), show_col_types = FALSE)
read_supp <- function(x) read_csv(file.path(base, "tables/supplement", x), show_col_types = FALSE)
read_res <- function(x) read_csv(file.path(base, "results_summary", x), show_col_types = FALSE)
read_src <- function(x) read_csv(file.path(nature, "source_data", x), show_col_types = FALSE)
write_tbl <- function(df, name) {
  write_csv(df, file.path(out, paste0(name, ".csv")))
  write.xlsx(df, file.path(out, paste0(name, ".xlsx")), overwrite = TRUE)
  x <- df %>% mutate(across(everything(), as.character))
  x <- x %>% mutate(across(everything(), ~ str_replace_all(.x, "\\|", "\\\\|")))
  header <- paste0("| ", paste(names(x), collapse = " | "), " |")
  divider <- paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |")
  rows <- apply(x, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |"))
  writeLines(c(header, divider, rows), file.path(out, paste0(name, ".md")), useBytes = TRUE)
}
pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), 100 * x)
num <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), x)
ci <- function(lo, hi, digits = 2) paste0(num(lo, digits), "-", num(hi, digits))

baseline <- read_main("table_main_1_baseline_characteristics.csv")
tprob <- read_main("table_main_2_transition_probabilities.csv")
models <- read_src("nature_Figure3A_pooled_effects.csv")
meta <- read_src("nature_Figure4A_B_meta_summary.csv")
country <- read_src("nature_Figure4D_F_country_probabilities.csv")
stdp <- read_src("nature_Figure3B_C_standardized_probabilities.csv")
ard <- read_src("nature_Figure3D_absolute_risk_difference.csv")
subgroup <- read_src("nature_Figure5A_B_subgroup_benefit.csv")
landmark <- read_src("nature_Figure5C_landmark.csv")
proxy <- read_src("nature_Figure5D_proxy.csv")
strict <- read_src("nature_Figure5E_strict_adl.csv")
panel_meanings <- read_csv(file.path(nature, "panel_meanings.csv"), show_col_types = FALSE)

table1 <- baseline %>%
  mutate(
    age_mean = round(age_mean, 1),
    female = pct(female_pct),
    partnered = pct(partnered_pct),
    living_child = pct(living_child_pct),
    adl_disabled = pct(adl_disabled_pct)
  ) %>%
  select(cohort, n, age_mean, female, partnered, living_child, adl_disabled)

table2 <- tprob %>%
  filter(!is.na(sfcr_cat4)) %>%
  mutate(
    sfcr_cat4 = recode(sfcr_cat4, partner_child = "Partner + child", child_only = "Child only", partner_only = "Partner only", neither = "Neither"),
    next_independent = pct(next_independent),
    next_disabled = pct(next_disabled),
    next_dead = pct(next_dead)
  ) %>%
  select(origin_state = state_label, sfcr_cat4, n, next_independent, next_disabled, next_dead)

table3 <- models %>%
  mutate(
    RRR = round(rrr, 3),
    CI95 = paste0(round(rrr_lo, 3), "-", round(rrr_hi, 3)),
    p_value = signif(p.value, 3)
  ) %>%
  select(transition, contrast, RRR, CI95, p_value)

table4 <- meta %>%
  mutate(
    OR = round(or, 3),
    CI95 = paste0(round(or_lo, 3), "-", round(or_hi, 3)),
    I2 = sprintf("%.1f%%", I2),
    tau2 = round(tau2, 4)
  ) %>%
  select(transition, OR, CI95, I2, tau2)

table5 <- country %>%
  select(cohort, cohort_country, transition, N, denom, prob, analytic_intervals) %>%
  mutate(prob_pct = pct(prob)) %>%
  arrange(transition, desc(prob))

table6 <- stdp %>%
  mutate(probability = pct(prob)) %>%
  select(origin_state, sfcr_cat4, next_state, probability)

table7 <- ard %>%
  mutate(risk_difference_pp = round(risk_difference * 100, 2)) %>%
  select(origin_state, next_state, Neither, `Child only`, `Partner only`, `Partner + child`, risk_difference_pp)

table8 <- subgroup %>%
  mutate(benefit_pp = round(benefit * 100, 2)) %>%
  select(sex_clean, age_group, wealth_tertile, metric, benefit_pp) %>%
  arrange(metric, desc(benefit_pp))

table9 <- landmark %>%
  mutate(
    sfcr_cat4 = recode(sfcr_cat4, partner_child = "Partner + child", child_only = "Child only", partner_only = "Partner only", neither = "Neither"),
    proportion = pct(prop)
  ) %>%
  select(sfcr_cat4, next_state_label, N, proportion)

table10 <- proxy %>%
  mutate(
    sfcr_cat4 = recode(sfcr_cat4, partner_child = "Partner + child", child_only = "Child only", partner_only = "Partner only", neither = "Neither"),
    probability = pct(prob)
  ) %>%
  select(proxy_group, sfcr_cat4, transition, probability)

table11 <- strict %>%
  pivot_wider(names_from = definition, values_from = prevalence) %>%
  mutate(default_adl = pct(default_adl), strict_adl = pct(strict_adl))

table12 <- panel_meanings

tables <- list(
  Table_1_baseline_characteristics = table1,
  Table_2_observed_transition_probabilities = table2,
  Table_3_primary_transition_models = table3,
  Table_4_meta_heterogeneity = table4,
  Supplementary_Table_1_country_probabilities = table5,
  Supplementary_Table_2_standardized_probabilities = table6,
  Supplementary_Table_3_absolute_risk_differences = table7,
  Supplementary_Table_4_subgroup_benefit = table8,
  Supplementary_Table_5_first_onset_landmark = table9,
  Supplementary_Table_6_proxy_sensitivity = table10,
  Supplementary_Table_7_strict_adl_definition = table11,
  Supplementary_Table_8_panel_meanings = table12
)

for (nm in names(tables)) write_tbl(tables[[nm]], nm)

wb <- createWorkbook()
for (nm in names(tables)) {
  addWorksheet(wb, substr(nm, 1, 31))
  writeData(wb, substr(nm, 1, 31), tables[[nm]])
}
saveWorkbook(wb, file.path(out, "Nature_R_tables_all.xlsx"), overwrite = TRUE)

summary_text <- c(
  "# Nature R Tables",
  "",
  "This directory contains manuscript-facing main tables and story-focused supplementary tables derived from the six-country Nature R visualization data.",
  "",
  "Main tables:",
  "- Table 1: baseline characteristics by cohort.",
  "- Table 2: observed transition probabilities by origin state and SFCR group.",
  "- Table 3: primary transition-specific model estimates.",
  "- Table 4: two-stage meta-analysis and heterogeneity.",
  "",
  "Supplementary tables:",
  "- Supplementary Table 1: country-level post-disability recovery and mortality probabilities.",
  "- Supplementary Table 2: standardized transition probabilities.",
  "- Supplementary Table 3: absolute risk differences.",
  "- Supplementary Table 4: subgroup benefit patterns.",
  "- Supplementary Table 5: first-onset landmark outcomes.",
  "- Supplementary Table 6: proxy sensitivity.",
  "- Supplementary Table 7: strict ADL definition.",
  "- Supplementary Table 8: panel meanings and source data."
)
writeLines(summary_text, file.path(out, "README_tables_nature.md"), useBytes = TRUE)

writing_pack <- c(
  "# 中文写作底稿：Structural Family Care Reserve and Disability Transitions in Later Life",
  "",
  "## 一句话主论点",
  "在六个 HRS-family 老龄化队列构建的跨国纵向 person-interval 数据中，结构性家庭照护储备，即伴侣/配偶与在世子女的结构性可得性，与更有利的老年失能转移轨迹相关，主要表现为更低的死亡转移风险和更高的失能后恢复独立概率。",
  "",
  "## 摘要中文草稿",
  "背景：老年失能并非静态终点，而是在独立、失能和死亡之间动态转移的过程。临床实践中，年龄、慢病和功能状态常被用于风险分层，但伴侣和子女等结构性家庭照护资源是否能够帮助识别失能恶化、恢复和死亡风险，仍缺乏跨国纵向证据。",
  "方法：本研究基于六个 HRS-family 老龄化队列（CHARLS、HRS、KLoSA、MHAS、SHARE 和 ELSA）构建 person-wave interval 数据。主暴露为结构性家庭照护储备，包括是否有伴侣/配偶以及是否有在世子女，并构建四分类变量（partner + child、partner only、child only、neither）和评分变量。主结局为基于五项 ADL（洗澡、穿衣、进食、上下床、如厕）统一重建的三状态转移：独立、失能和死亡。主分析采用离散时间多状态框架，并按起始状态分层估计独立至失能、独立至死亡、失能至恢复独立和失能至死亡的转移。",
  "结果：主分析包含 564,316 个 person-wave intervals；描述性转移分析包含 577,451 个 intervals，覆盖 34 个 cohort-country 单元。观察到的转移包括 independent -> independent 423,779 次，independent -> disabled 42,145 次，independent -> dead 23,786 次，disabled -> independent 27,333 次，disabled -> disabled 43,478 次，disabled -> dead 16,930 次。与 neither 组相比，partner + child 组与更低的 independent -> disabled 风险（RRR 0.827, 95% CI 0.755-0.907）、更低的 independent -> dead 风险（RRR 0.584, 95% CI 0.512-0.666）、更高的 disabled -> independent 概率（RRR 1.172, 95% CI 1.025-1.339）以及更低的 disabled -> dead 风险（RRR 0.678, 95% CI 0.554-0.830）相关。标准化概率显示，起始独立者中，partner + child 组下一波死亡概率为 4.2%，低于 neither 组的 6.5%；起始失能者中，partner + child 组恢复独立概率为 30.6%，高于 neither 组的 26.9%，死亡概率为 13.3%，低于 neither 组的 16.1%。两阶段 meta-analysis 显示，死亡转移和失能后恢复方向较稳定，而 independent -> disabled 的异质性最高（I² 75.4%）。",
  "结论：结构性家庭照护储备可作为老年失能转移风险分层的重要社会家庭指标。伴侣和子女同时可得的老年人在跨国队列中表现出更有利的功能轨迹，尤其体现为更高的失能后恢复概率和更低的死亡转移风险。结果支持在老年医学、康复和出院管理中将家庭结构资源纳入功能预后评估，但因研究为观察性分析，应避免作强因果解释。",
  "",
  "## Results 中文段落底稿",
  "六国样本和状态转移结构：本研究最终纳入六个 HRS-family 队列，包括 CHARLS、HRS、KLoSA、MHAS、SHARE 和 ELSA。主推断分析包含 564,316 个 person-wave intervals；描述性转移图表基于 577,451 个有效 intervals。各队列对分析贡献不同，其中 SHARE 贡献 205,817 个 interval，HRS 贡献 179,511 个 interval，ELSA、CHARLS、KLoSA 和 MHAS 分别贡献 69,089、59,794、36,493 和 26,747 个 interval。整体状态转移显示，多数起始独立者在下一波仍保持独立（86.5%），但 8.6% 转为失能，4.9% 死亡。起始失能者中，31.2% 下一波恢复独立，49.6% 持续失能，19.3% 死亡，提示失能是可恢复、可持续并可向死亡转移的动态状态。",
  "结构性家庭照护储备分布和观察性转移：六个队列中 partner + child 均为最常见的结构性家庭照护储备类型，比例从 HRS 的 59.6% 到 CHARLS 的 87.8% 不等。观察性转移概率已显示出明显梯度：在起始独立者中，partner + child 组下一波保持独立的比例为 88.7%，转为失能为 7.4%，死亡为 3.9%；neither 组对应比例为 83.0%、10.2% 和 6.8%。在起始失能者中，partner + child 组恢复独立比例为 36.1%，死亡比例为 15.7%；neither 组恢复独立为 25.4%，死亡为 22.6%。",
  "主模型结果：在多状态转移模型中，结构性家庭照护储备与更有利的转移模式相关。与 neither 组相比，partner + child 组在 independent -> disabled 转移中的 RRR 为 0.827（95% CI 0.755-0.907），在 independent -> dead 转移中的 RRR 为 0.584（95% CI 0.512-0.666）。对于起始失能者，partner + child 组更可能恢复独立（RRR 1.172, 95% CI 1.025-1.339），且 disabled -> dead 风险更低（RRR 0.678, 95% CI 0.554-0.830）。child only 组在多数转移中也呈保护方向，尤其对死亡转移较明显；partner only 组效果相对不稳定。",
  "标准化概率和绝对风险差：模型标准化概率显示，起始独立者中，partner + child 组的下一波死亡概率为 4.2%，低于 neither 组的 6.5%；下一波失能概率为 10.0%，也低于 neither 组的 11.1%。起始失能者中，partner + child 组恢复独立概率为 30.6%，高于 neither 组的 26.9%；死亡概率为 13.3%，低于 neither 组的 16.1%。换算为绝对差异，partner + child 相比 neither 使起始独立者保持独立增加约 3.4 个百分点、死亡减少约 2.2 个百分点；使起始失能者恢复独立增加约 3.7 个百分点、死亡减少约 2.9 个百分点。",
  "跨国异质性：两阶段 meta-analysis 进一步验证了主分析方向。每增加 1 单位 SFCR score，independent -> dead 的合并 OR 为 0.792（95% CI 0.749-0.837），disabled -> independent 的合并 OR 为 1.074（95% CI 1.017-1.133），disabled -> dead 的合并 OR 为 0.883（95% CI 0.816-0.956）。independent -> disabled 的合并 OR 为 0.964（95% CI 0.884-1.051），异质性最高（I² 75.4%），提示新发失能路径更可能受国家制度、家庭结构、测量或样本差异影响。失能后观察性恢复概率在国家间差异明显，最高包括 Switzerland 44.2%、Luxembourg 38.9%、Mexico 38.9% 和 China 38.4%；死亡概率最高包括 Hungary 73.8%、Croatia 61.5% 和 Portugal 41.8%。",
  "稳健性和扩展分析：亚组分析提示获益在女性高龄组更明显。partner + child 相比 neither 对 lower disability risk 的最大绝对获益出现在 Female 75+ T3（6.9 个百分点）、Female 75+ T1（6.1 个百分点）和 Female 75+ T2（5.0 个百分点）；lower mortality risk 的最大获益出现在 Female 75+ T1（6.2 个百分点）。first-onset landmark 分析显示，首次失能后 partner + child 组下一波恢复独立 47.5%，死亡 13.6%；neither 组恢复独立 38.8%，死亡 19.6%。proxy sensitivity 显示，proxy 样本总体风险更高，但 SFCR 梯度并未消失。strict ADL 定义降低了失能患病率水平，但没有推翻队列间的主要排序和主结论方向。",
  "",
  "## Discussion 中文段落底稿",
  "本研究表明，结构性家庭照护储备不仅是老年人社会背景信息，也可能是失能转移风险分层的重要临床指标。伴侣和在世子女同时可得的老年人，在跨国纵向队列中表现出更低的死亡转移风险和更高的失能后恢复概率。该结果与老年医学中关于康复支持、用药管理、就医陪伴、跌倒预防和日常功能训练依赖家庭照护资源的临床经验一致。",
  "值得注意的是，家庭结构储备对死亡转移和失能后恢复的关联比对新发失能的关联更稳定。新发失能受到慢病积累、生活环境、医疗可及性、社会制度和测量差异的共同影响，因此跨国异质性较高并不意外。相比之下，失能后是否恢复以及是否死亡，可能更直接受到照护监测、康复执行、营养支持和及时就医的影响，因此与家庭照护储备的关联更稳定。",
  "本研究的临床意义在于，老年失能患者的风险评估不应只关注年龄、慢病、认知或抑郁，也应系统记录伴侣、子女和居住支持等结构性家庭照护资源。对于无伴侣且无在世子女的个体，临床团队可能需要更早介入出院计划、康复转介、社区随访和长期照护安排。该指标简单、跨队列可协调，并可用于快速识别失能后恢复机会较低或死亡风险较高的人群。",
  "本研究仍有局限。首先，研究为观察性分析，家庭结构储备与健康轨迹之间可能存在选择效应，不能直接解释为因果关系。其次，不同队列在财富、抑郁、认知和部分协变量测量上存在差异，尽管研究采用了最小公分母协调和队列内标准化策略，仍可能存在残余不可比性。第三，国家特异模型系数目前并非所有 cohort-country 单元均可稳定估计，因此跨国图谱中应区分 observed probability profile 和 model-based country-specific effects。第四，家庭照护储备是结构性可得性指标，并不等同于实际照护质量、照护强度或关系质量。",
  "总体而言，本研究支持将结构性家庭照护储备纳入老年失能转移研究和临床风险分层。未来研究可进一步结合实际照护行为、照护质量、社区服务可及性和长期照护制度指标，以区分家庭结构资源本身、照护实践和国家制度环境对失能恢复和死亡风险的相对贡献。"
)
writeLines(writing_pack, file.path(out, "Chinese_manuscript_draft.md"), useBytes = TRUE)
