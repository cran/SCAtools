# SCAtools：充分條件分析 R 套件

`SCAtools` 是一個實驗版 R 套件，用 empty-space frontier 分析方向明確的
充分條件敘述。它刻意區分：

1. **充分條件的邏輯方向**：HH、LH、HL、LL；
2. **原始散點圖實際為空的位置**：corner 1、2、3、4。

| SCA 編號 | 方向 | 充分條件敘述 | 實際空白 corner |
|---:|:---:|---|---:|
| 1 | HH | High X sufficient for High Y | 4（右下） |
| 2 | LH | Low X sufficient for High Y | 3（左下） |
| 3 | HL | High X sufficient for Low Y | 2（右上） |
| 4 | LL | Low X sufficient for Low Y | 1（左上） |

因此，`direction = "HH"` 不會把圖上的 corner 4 改叫 corner 1；輸出會同時
顯示「SCA logical direction 1」和「physical empty corner 4」。

## 安裝

原始碼與問題回報：<https://github.com/youngchanresearcher/SCAtools>

```r
install.packages(c("NCA", "ggplot2"))
install.packages("SCAtools_0.4.3.tar.gz", repos = NULL, type = "source")
library(SCAtools)
```

## 最小範例

```r
set.seed(42)
dat <- sca_random(
  n = 100,
  intercepts = 0,
  slopes = 1,
  direction = "HH"
)

fit <- sca_analysis(
  data = dat,
  x = "X",
  y = "Y",
  direction = "HH",
  ceilings = c("ce_fdh", "cr_fdh"),
  threshold.x = "percentile",
  threshold.y = "percentage.range",
  test.rep = 1000
)

fit
sca_table(fit)
sca_thresholds(fit, ceiling = "ce_fdh", inequality = "strict")

# 同一個模型換一個尺度呈現，不需要重新配適
sca_thresholds(fit, scale = "actual")
sca_thresholds(fit, scale = "sd")
sca_plot(fit)
sca_test_plot(fit, ceiling = "ce_fdh")

# 選用：標準化與 influential-observation diagnostics
sca_normalize(dat)
sca_outliers(dat, "X", "Y", direction = "HH")
```

## 門檻表的尺度與慣例

門檻表可以用真實數值、範圍百分比、最大值百分比、百分位或標準差呈現。真實數值一律
保留在 `sufficiency_threshold_actual` 與 `outcome_level_actual` 兩欄，所以換尺度不必
重新配適模型。`sca_scales()` 會列出所有可用的尺度。

有兩種報告慣例，差別只出現在含 `L` 的方向：

| 慣例 | `HH` 讀作 | `LL` 讀作 |
|---|---|---|
| `absolute`（預設） | `X > 70% => Y > 80%` | `X < 30% => Y < 20%` |
| `directional` | `X > 70% => Y > 80%` | `X > 70% => Y > 80%` |

`absolute` 讓 0 永遠落在軸的低端，方向完全由不等號承載，因此不同方向的數字可以直接
比較。`directional` 讓 0 落在「最不充分」的一端：`L` 方向的軸會鏡射，不等號跟著翻轉，
四個方向都讀成「愈多愈充分」。

## 為什麼尺度轉換不交給 NCA

NCA 的百分比一律從軸的低端量，但百分位卻依**實體空區角落**鏡射，而超出範圍的邊界儲
存格又用第三套慣例。由於實體角落與邏輯充分性方向並不對應，直接沿用會得到無法一致
閱讀的表格。因此 `SCAtools` 一律向引擎索取真實數值，所有尺度轉換都在本套件內完成。

同樣的道理適用於 `NN` 與 `NA` 標記——它們的意義在對位換質下會顛倒：

| 引擎儲存格 | 必要性讀法 | 充分性讀法 | `status` |
|---|---|---|---|
| `NN` | 沒有最低要求 | 不存在可達成的門檻 | `no_threshold` |
| `NA` | 要求超出範圍 | 所有觀測都已滿足 | `always_satisfied` |

## 四種方向

```r
sca_corner_map()

# HH：High X -> High Y；右下 corner 4 應為空
fit_hh <- sca_analysis(dat, "X", "Y", direction = "HH")

# HL：High X -> Low Y；右上 corner 2 應為空
fit_hl <- sca_analysis(dat, "X", "Y", direction = "HL")
```

若同一次分析有多個 X，可以對每個 X 指定方向，但所有敘述必須分析同一個
Y 方向。例如 `c("HH", "LH")` 都以 High Y 為結果，可以同時分析；
`c("HH", "HL")` 一個分析 High Y、一個分析 Low Y，必須拆成兩個模型。

## 如何理解 threshold table

`sca_thresholds()` 會同時保留 NCA 表格使用的顯示尺度，以及引擎內部的原始
尺度門檻。這很重要，因為分析下方 corners 時，NCA 會按照「低 Y」的方向
重新排列百分比；直接把畫面百分比當成原始 Y 值會得到錯誤規則。SCA 表會
依原始 Y 尺度重新排序，同時保留引擎顯示值供追溯。

預設 `inequality = "strict"` 使用邏輯補集所要求的嚴格不等式。例如 HH：

```text
X > threshold  =>  Y > outcome level
```

只有當量尺或校準方式明確把邊界納入 high/low set 時，才使用
`inequality = "inclusive"`。

## 必須保留的研究限制

- effect size 是指定 X–Y scope 內的空白區比例，會受 scope 與 frontier
  technique 影響。
- permutation p-value 檢驗打亂 X–Y 配對後，是否很少出現同樣大的空白區；
  它不是因果充分性的證明。
- 樣本內沒有反例，不等於母體中不存在反例。
- 因果方向、時間先後、測量誤差、離群值、共同原因與樣本外驗證仍須另外處理。
- 因此，沒有強因果設計時，建議把結果稱為 **empirical sufficiency frontier
  pattern**，而不是無條件的 causal guarantee。

## 套件結構與授權

`SCAtools` 使用 NCA 5.0.2 或更新版本作為計算引擎，負責 frontier、effect
size、permutation test、模擬與 power；本套件負責正確的 SCA 方向映射、輸出、
threshold rules 與圖形。這樣可避免複製後與官方 NCA 演算法逐漸分歧。

NCA 與本套件皆採 GPL-3-or-later。

安裝後另附 `docs/sufficiency-logic.md`（完整邏輯推導）與
`examples/four-directions.R`（可執行範例）。

`citation("SCAtools")` 會同時回傳本套件與 NCA 方法論文兩筆引用資料。

## 參照線：OLS 不是 frontier

`reference = "ols"` 會在 frontier 旁邊多畫一條 outcome 對 condition 的最小平方
迴歸線，也就是 `lm(y ~ x)` 那條線，純粹用來對照：

```r
fit <- sca_analysis(
  dat, x = "X", y = "Y", direction = "HH",
  ceilings = "ce_fdh",
  reference = "ols"
)

sca_reference(fit)   # 截距、斜率、R 平方；沒有 effect size
```

迴歸線講的是「條件變動時，結果的期望值怎麼跟著動」；sufficiency frontier 講的是
「哪些條件—結果組合不會出現」。frontier 由最極端的觀察值決定，迴歸線由集中趨勢
決定，兩者是不同的量，所以分開報告：參照線沒有空區、沒有 effect size、沒有
permutation p 值、也沒有 sufficiency threshold，不會出現在 `sca_table()` 的任何
一列。

0.4.1 之前 `"ols"` 是預設 `ceilings` 的第一個元素，現在不再是預設。放在
`ceilings` 裡仍然可用，只會警告並移到 `reference`；單獨只給 `"ols"` 會報錯，因為
沒有任何空區可以估計。

