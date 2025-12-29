# Cross-Asset-Market-Efficiency-Alpha-Persistence-Study

Executive Summary

This project investigates structural and behavioral anomalies across 11 asset classes (Commodities, Equities, and Crypto) over a 10-year horizon. By comparing volatile assets against the Dow Jones Industrial Average (the efficient control group), this study identifies specific "frictions" where alpha persists.

Research Pillars

Structural Seasonality: Quarterly cycles in physical commodities (e.g., Lumber, Corn).
Behavioral Momentum: Monthly "herding" patterns (e.g., Sugar, Coffee).
Tactical Volatility: Weekly walk-forward breakouts using a 75th-percentile threshold (e.g., Bitcoin).

Key Quantitative Methodology

Monte Carlo Permutation Testing: Every signal was validated against 20,000 randomized iterations to ensure p-values were robust against "luck" or data-mining bias.
Recursive Walk-Forward Framework: All tactical signals were generated using only prior data to eliminate look-ahead bias.
Alpha Decay Comparison: Used the Dow Jones as a grey-scale baseline to visually prove the disappearance of alpha in highly efficient equity markets.

Top Results

Lumber: 6.72% Seasonal Alpha (p < 0.0001).
Bitcoin: 4.30% Weekly Tactical Alpha (p < 0.001).
Sugar: 1.96% Monthly Momentum Alpha (p < 0.05).
