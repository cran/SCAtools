# SCAtools：充分條件分析

在 R 中進行充分條件分析（Sufficient Condition Analysis, SCA）。

## 這個方法是什麼

當一個條件出現就保證結果出現，這個條件就是結果的**充分條件**。放到資料上，
充分條件的主張會做出明確的預測：與它矛盾的個案不會出現。把條件 X 對結果 Y
畫成散佈圖，這些矛盾個案落在圖的某一個角落。例如「高 X 是高 Y 的充分條件」
預測右下角（高 X 而低 Y）是空的。

充分條件分析估計這個空白角落：配適空白區的邊界、計算空白區有多大、檢定它是否
大於 X 與 Y 隨機配對時會出現的大小，並把邊界轉成門檻——每一個 Y 的水準需要多少
X 才能保證達到。

充分條件的主張有四個方向，各自預測不同的空白角落：

| 方向 | 主張 | 空白角落 |
|:---:|---|---|
| `HH` | 高 X 是高 Y 的充分條件 | 右下 |
| `LH` | 低 X 是高 Y 的充分條件 | 左下 |
| `HL` | 高 X 是低 Y 的充分條件 | 右上 |
| `LL` | 低 X 是低 Y 的充分條件 | 左上 |

你只要說明主張的方向，套件會找到對應的角落。

## 可以用來做什麼

- **找出保證結果的水準。** 門檻表回答「X 要多少，Y 才保證達到這個水準？」，
  每個結果水準各給一個答案，可以用原始單位、百分比、百分位或標準差呈現。
- **看見迴歸看不到的效果。** 迴歸斜率描述結果**平均**如何隨條件變動。一個條件
  可能幾乎不改變平均，卻完全排除了低結果。充分條件分析衡量的是第二種效果，
  `sca_compare()` 把兩者並列，讓研究清楚說明資料呈現的是哪一種。
- **比較多個條件。** 一次呼叫可以分析多個條件，各有自己的方向，結果放在同一張表。
- **規劃研究。** `sca_power()` 估計要偵測某個大小的充分效果需要多少觀測值。

## 安裝

```r
install.packages("SCAtools")
```

## 怎麼使用

套件附有四組模擬資料，示範迴歸效果與充分效果可能組合出的四種情形。

```r
library(SCAtools)

# 組態 II：迴歸斜率幾乎是平的，但右下角是空的
dat <- subset(sca_four_configurations, configuration == "II")

# 1. 配適「高 X 是高 Y 的充分條件」
fit <- sca_analysis(dat, x = "X", y = "Y", direction = "HH",
                    scope = c(0, 100, 0, 100), test.rep = 1000)
fit                     # 摘要，並列出結果所依據的每一項設定

# 2. 讀取結果
sca_table(fit)          # 每個條件的效果量、p 值與支持判定
sca_thresholds(fit)     # 保證每個 Y 水準所需的 X 水準
sca_plot(fit)           # 散佈圖、邊界線與空白角落

# 3. 與迴歸並列
sca_compare(fit)        # 這組資料：斜率接近 0，充分效果很大
```

`sca()` 一步完成同樣的分析，附置換檢定並畫圖：

```r
sca(dat, x = "X", y = "Y", direction = "HH")
```

### 檢查結果

| 函數 | 用途 |
|---|---|
| `sca_test_plot()` | 置換分配與觀測到的效果量 |
| `sca_scope()` | 每個效果量所除的範圍（scope） |
| `sca_scope_sensitivity()` | 範圍改變時效果量如何變化 |
| `sca_resample()` | 效果量在子樣本間的變動 |
| `sca_outliers()` | 哪些觀測值撐起邊界 |
| `sca_locationshift()` | 邊界是整體平移還是門檻 |
| `sca_magnitude()` | 效果量的慣用等級 |

## 怎麼讀結果

- **效果量**是空白面積佔範圍的比例，從 0（沒有空白角落）到 1。旁邊的
  `effect_size_null` 是同一份資料在隨機配對下會得到的值。
- **p 值**來自打亂 X 與 Y 配對的置換檢定。
- **門檻**寫成規則，例如 `X > 38.9 => Y > 30`。`no_threshold` 表示沒有任何 X
  能達到該結果水準；`always_satisfied` 表示範圍內每個觀測值都已經達到。
- **尺度。** 配適好的模型不必重新配適就能換尺度呈現：

  ```r
  sca_thresholds(fit, scale = "actual")      # 原始單位
  sca_thresholds(fit, scale = "percentile")  # 百分位
  sca_thresholds(fit, scale = "sd")          # 標準差
  ```

  預設的 `absolute` 慣例中，每條軸的 0 都在低端，由不等號表示方向；`directional`
  慣例會把低水準的軸鏡像翻轉，讓每條規則都讀成「充分條件越多」。`sca_scales()`
  列出所有選項。

## 良好做法

- **先依理論決定方向，再畫圖檢查資料。** 每個方向預測單一個空白角落，前提是關係
  是單調的。
- **報告範圍（scope）。** 效果量是面積除以範圍，請說明使用的是觀測範圍還是理論
  範圍，以及理由。`sca_scope()` 和輸出的 `Settings` 區塊會記錄這項設定。
- **報告邊界估計方法與置換次數。** CE-FDH（`"ce_fdh"`）以階梯狀貼著資料；
  CR-FDH（`"cr_fdh"`）以一條直線穿過它。置換次數通常用 1000 次以上。
- **事先設定實務門檻。** 如果結論需要效果量達到某個最低值，用 `relevance =` 事先
  指定。
- **把門檻當作經驗規則報告**，適用於所研究的母體與設計；能推論到多遠取決於研究設計。

可以用 `reference = "ols"` 或 `"qr"` 加上一條最小平方法或中位數迴歸線作為對照，
由 `sca_reference()` 報告；它不屬於充分條件分析的一部分。

## 延伸閱讀

- `?SCAtools`：方法說明與參考文獻。
- `citation("SCAtools")`：引用格式。
- `system.file("docs", "sufficiency-logic.md", package = "SCAtools")` 逐步推導四個
  方向；`system.file("examples", "four-directions.R", package = "SCAtools")` 實際執行。
- NSCA 套件以 SCAtools 為基礎，檢定同時必要且充分的條件。

原始碼與問題回報：<https://github.com/youngchanresearcher/SCAtools>
