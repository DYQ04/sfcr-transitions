# sfcr-transitions

本仓库提供项目 **Structural Family Care Reserve and Disability Transitions in Later Life** 的可复现 R 分析代码。

本项目基于 HRS-family 老龄化队列，围绕家庭照护结构储备与晚年失能转移之间的关系，整合 CHARLS（中国）、HRS（美国）、KLoSA（韩国）、MHAS（墨西哥）、SHARE（欧洲多国，保留 respondent country 作为 country-level 分析单元）和 ELSA（英格兰）等六个主要数据来源，完成跨队列数据整理、变量 harmonization、person-wave 转移区间构建、离散时间多状态模型估计、两阶段 meta 分析，以及论文图表所需结果文件的导出。整体流程旨在形成一套从原始队列文件到可复现统计结果和论文级图表的完整分析框架。
<img width="1140" height="570" alt="image" src="https://github.com/user-attachments/assets/9bd12ab6-dc21-4665-a9e6-9b2f42cb928b" />


## 仓库内容

本仓库主要包含：

- 原始数据扫描、变量整理、区间构建、模型估计、meta 分析和图表导出的 R 代码；
- 基于 `targets` 的可复现分析流程；
- 原始数据目录占位文件；
- 论文图表生成所需的脚本和辅助函数；
- 用于说明项目结构和运行方式的文档文件。

## 文件结构

```text
sfcr-transitions/
├── R/
│   ├── 00_utils.R
│   ├── 01_manifest.R
│   ├── 02_prepare_long.R
│   ├── 03_prepare_supplements.R
│   ├── 04_harmonize.R
│   ├── 05_codebooks.R
│   ├── 06_intervals.R
│   ├── 07_models.R
│   ├── 08_meta_summary.R
│   ├── 09_tables.R
│   ├── 10_six_country_run.R
│   ├── 11_nature_figures.R
│   └── 12_nature_tables.R
│
├── data_raw/
│   ├── CHARLS/
│   ├── HRS/
│   ├── KLoSA/
│   ├── LASI/
│   ├── MHAS/
│   ├── SHARE/
│   └── ELSA/
│
├── outputs/
│   ├── figures/
│   ├── tables/
│   ├── source_data/
│   └── model_summaries/
│
├── docs/
│   └── figures/
│
├── renv/
├── _targets.R
├── run.R
├── renv.lock
├── .gitignore
└── README.md
```

## 数据准备

请将各队列数据放置于 `data_raw/` 目录下，例如：

```text
data_raw/
├── CHARLS/
├── HRS/
├── KLoSA/
├── LASI/
├── MHAS/
├── SHARE/
└── ELSA/
```

代码会递归扫描 `data_raw/`，因此各队列内部文件夹名称不需要与上方示意完全一致。

## 运行方式

首先安装并恢复 R 包环境：

```r
install.packages("renv")
renv::restore()
targets::tar_make()
```

也可以在终端中运行：

```bash
Rscript run.R
```

主要结果文件将生成在 `outputs/` 目录下。

## 脚本说明

- `R/00_utils.R`：通用函数、路径设置与队列目录配置。
- `R/01_manifest.R`：扫描原始文件并生成数据清单。
- `R/02_prepare_long.R`：读取各队列工作文件并整理为长格式数据。
- `R/03_prepare_supplements.R`：整理辅助变量和补充数据来源。
- `R/04_harmonize.R`：构建 harmonized wave-level 变量。
- `R/05_codebooks.R`：导出变量 harmonization codebook。
- `R/06_intervals.R`：构建 person-wave disability transition 区间。
- `R/07_models.R`：估计 one-stage transition models。
- `R/08_meta_summary.R`：执行 two-stage meta-analysis 并汇总结果。
- `R/09_tables.R`：导出核心分析表格。
- `R/10_six_country_run.R`：执行主分析流程并生成作图数据。
- `R/11_nature_figures.R`：导出论文图件。
- `R/12_nature_tables.R`：导出论文写作所需的最终表格。

## 方法说明

- SHARE 保留 respondent country，并将其作为 country-level 分析单元。
- ADL disability 基于五个 harmonized ADL 条目重建。
- `SFCR` 根据是否有配偶/伴侣以及是否有存活子女定义。
- 转移结局基于相邻 wave 间的状态变化构建，包括 independent、disabled 和 dead。
- 主要模型采用离散时间多状态框架，并结合 one-stage 与 two-stage 分析结果进行汇总。
- 本仓库用于代码共享和可复现分析展示，不作为数据仓库使用。
