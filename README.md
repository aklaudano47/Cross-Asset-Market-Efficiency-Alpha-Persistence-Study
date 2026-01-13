# Cross-Asset Market Efficiency & Alpha Persistence Study

### Executive Summary
This repository contains a comprehensive **Statistical Audit of the Efficient Market Hypothesis (EMH)** across 11 asset classes. This project functions as an **Anomaly Analysis Framework** designed to identify where structural and behavioral "pockets" of alpha persist despite modern high-frequency arbitrage. 

By benchmarking "noisy" assets against the **Dow Jones Industrial Average** (the efficient control group), this study quantifies the decay of alpha across three distinct horizons.

---

### Three Pillars of Alpha Analysis

#### 1. Structural Seasonality (The Physical Thesis)
* **Logic**: Analyzed quarterly mean-reversion and expansion cycles.
* **Mechanism**: Investigated biological harvest windows and storage bottlenecks in physical commodities.
* **Result**: Confirmed significant alpha persistence in **Lumber** and **Wheat**, validating that physical supply-chain rigidity creates non-arbitrable windows.

#### 2. Monthly Behavioral Momentum (The "Herding" Thesis)
* **Logic**: Evaluated intermediate-term trend persistence based on 1-month lookbacks.
* **Mechanism**: Quantified behavioral "herding" and delayed information reaction in soft commodities (Sugar, Coffee).
* **Result**: Identified specific regimes where momentum remains a robust signal before being neutralized by market efficiency.

#### 3. Weekly Tactical Volatility (The "Breakout" Thesis)
* **Logic**: High-volatility breakouts using a 75th-percentile threshold.
* **Mechanism**: Analyzed short-term liquidity shocks and retail-driven momentum in high-beta assets like **Bitcoin**.
* **Result**: Utilized an expanding window validation to verify that tactical signals were generated using only historical data to mitigate look-ahead bias.

---

### Quantitative Rigor & Validation

* **Non-Parametric Significance Testing**: Every identified anomaly was subjected to a **20,000-iteration Monte Carlo Permutation Test**.
* **Alpha Decay Mapping**: By generating synthetic "null distributions," I isolated which returns were statistically significant ($p < 0.05$) versus those resulting from stochastic noise.

* **Expanding Window Validation**: For tactical signals, I implemented an expanding window approach to verify that the signals remained persistent when evaluated chronologically.

---

### Top Anomaly Results (Statistical Significance)

| Asset | Strategy | Observed Alpha (Spread) | P-Value (20k Reps) |
| :--- | :--- | :--- | :--- |
| **Lumber** | Seasonality | 6.72% | **p < 0.0001*** |
| **Bitcoin** | Weekly Momentum | 4.30% | **p < 0.001*** |
| **Sugar** | Monthly Momentum | 1.96% | **p < 0.05*** |

---

### Research Context
This research establishes that **Market Efficiency is not uniform**. While the Dow Jones (Control) showed rapid alpha decay, physical commodities and volatile digital assets retained structural edges. 

This study served as the statistical foundation for the more advanced **[Commodity Alpha Engine](https://github.com/aklaudano47/Commodity_Alpha_Engine)**, which translates these observations into a friction-adjusted, recursive trading framework.
