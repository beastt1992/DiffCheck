**中文版** | [English](README.md)

---

# DiffCheck.lsp — AutoCAD 兩區域差異比對工具

**在同一張 DWG 裡框選兩個範圍，自動比對差異，用雲形線標記。**

---

## 問題

AutoCAD 內建的 **DWG Compare** 只能比較兩個獨立檔案。但建築師常常在同一張 DWG 裡並排放新舊版本——底圖改完之後，沒有快速的方法找出到底改了什麼。

```
底圖改了幾條線
→ 肉眼比對兩張圖？
→ 哪裡改了？哪裡沒改？
→ 漏看一條線，施工就出問題
```

手動比對兩張複雜的圖面，既慢又容易出錯。

---

## 解法

DiffCheck 讓你框選兩個區域（舊圖 vs 新圖），自動對齊、比對每個物件，在差異位置畫出紅色雲形線——幾秒內完成。

```
1. 框選 Region A（舊圖）
2. 框選 Region B（新圖）
3. 紅色雲形線自動標出差異位置
```

---

## 技術原理

DiffCheck 使用 **Spatial Anchor Voting** 計算兩個區域之間的位移，為每個物件產生幾何簽名（類型 + 座標 + 特徵值），排序後用 **O(N log N) 線性掃描**找出差異。附近的差異會被合併成群組，畫成一個雲形線，避免畫面雜亂。超大物件（圖框、邊界線）自動過濾。

---

## 安裝

1. 下載 `DiffCheck.lsp`
2. 在 AutoCAD 輸入 `APPLOAD`
3. 載入檔案
4. 輸入 `DFC` 執行

**小技巧：** 加入 AutoCAD Startup Suite，每次開啟自動載入。

---

## 使用方式

輸入 `DFC`：

```
1. 框選舊圖區域（Region A）
2. 框選新圖區域（Region B）
3. 完成——差異位置出現紅色雲形線
```

指令列輸出：

```
Select Region A (old):
  238 objects selected
Select Region B (new):
  238 objects selected
  Auto-align votes: 31
  Generating Signatures & Sorting (Ultra Fast)...
  Matching & Grouping...
  ── Results ──
  Matched (Unchanged): 220
  Changes detected:    18
  All differences marked on Region B (DIFF_CLOUD layer).
  Time: 1.23s
```

---

## 指令

| 指令 | 說明 |
|------|------|
| `DFC` | 執行比對 |
| `DFCC` | 清除所有雲形線 |
| `DFCT` | 調整合併距離、間距、弧長 |

---

## 支援的物件類型

| 類型 | 簽名方式 |
|------|---------|
| LINE | 正規化端點 |
| CIRCLE | 圓心 + 半徑 |
| ARC | 圓心 + 半徑 + 角度 |
| LWPOLYLINE | 頂點 + bulge + 封閉旗標 |
| TEXT / MTEXT | 插入點 + 高度 + 內容 |
| INSERT（圖塊） | 名稱 + 插入點 + 縮放 + 旋轉 |
| DIMENSION | 類型 + 量測值 + 顯示文字 |

---

## 參數設定

用 `DFCT` 互動調整，或直接改檔案頂部：

| 變數 | 預設值 | 說明 |
|------|--------|------|
| `*dc:tol*` | `2.0` | 座標容差（圖面單位） |
| `*dc:pad*` | `20.0` | 雲形線與物件的間距 |
| `*dc:arc*` | `30.0` | 雲形線弧段長度 |
| `*dc:merge*` | `50.0` | 合併附近差異的最大間距 |
| `*dc:maxbox*` | `0.4` | 大物件過濾門檻（超過區域 40% 的物件自動忽略） |

**小技巧：**
- 假陽性太多？加大 `*dc:tol*`（試 5.0 或 10.0）
- 雲形線太大重疊？減小 `*dc:merge*`
- 自動對齊失敗？工具會提示你手動點兩個對應基準點

---

## 注意事項

| 項目 | 說明 |
|------|------|
| Hatch（填充） | 略過——種子點在編輯後不穩定 |
| LEADER / MLEADER | 目前版本略過 |
| 自動對齊失敗 | 提示手動兩點對齊 |
| 僅 2D | 不比較 Z 座標 |
| 圖塊屬性 | 比較名稱/縮放/旋轉，不比屬性值 |

---

## 相容性

| 版本 | 狀態 |
|------|------|
| AutoCAD 2014+ | ✅ 支援 |
| 2014 以下 | 未測試 |

也適用於 BricsCAD、GstarCAD 等支援 AutoLISP 的 CAD 平台。

---

## 常見問題

**雲形線太多 / 假陽性？**
用 `DFCT` 加大容差，或設 `*dc:tol*` 為 5.0。標註文字位置微移是最常見的原因。

**位移看起來不對？**
如果自動對齊投票數低於 3，工具會請你手動點兩個對應基準點。選兩個區域都有的柱心或牆角。

**跑完什麼都沒出現？**
確認兩個區域包含支援的物件類型（LINE、CIRCLE 等）。鎖定或凍結圖層上的物件可能沒被選到。

---

## 版本紀錄

| 版本 | 說明 |
|------|------|
| v21 | O(N log N) 排序合併、空間錨點投票、區域化框合併、大物件過濾、手動對齊 fallback、指令改為 DFC/DFCC/DFCT |

---

## 支持這個專案

如果 DiffCheck 幫你節省了時間，歡迎請我喝杯咖啡 ☕

[![ko-fi](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/beastt1992)

---

## 授權

MIT License — 免費使用、修改、散佈。

---

**為所有 AutoCAD 使用者而做。**
