read_clean_calc_quant_yh <- function(df) {

    library(dplyr)
    library(lubridate)
    library(TTR)
    library(tidyverse)
    library(zoo)
    library(tidyr)

    # ------------------------------------------------------------
    # 🔥 รองรับโครงสร้าง Yahoo Finance
    # ------------------------------------------------------------
    df <- df %>%
        rename(
            Date   = date,
            Price  = close,
            Open   = open,
            High   = high,
            Low    = low,
            Volume = volume
        ) %>%
        mutate(
            Date   = as.Date(Date),
            Price  = as.numeric(Price),
            Open   = as.numeric(Open),
            High   = as.numeric(High),
            Low    = as.numeric(Low),
            Volume = as.numeric(Volume)
        ) %>%
        arrange(Date) %>%
        
        # ------------------------------------------------------------
        # 🔥 Change robust
        # ------------------------------------------------------------
        mutate(
            Change = (Price / lag(Price) - 1) * 100,
            Change = replace_na(Change, 0)
        ) %>%
        
        # ------------------------------------------------------------
        # 🔥 Normalized / LogReturn / CumReturn
        # ------------------------------------------------------------
        mutate(
            Normalized = Price / first(Price) * 100,
            LogReturn = log(Price / lag(Price)),
            LogReturn = replace_na(LogReturn, 0),
            CumReturn = cumsum(LogReturn)
        )

    # ------------------------------------------------------------
    # 🔥 Signal robust
    # ------------------------------------------------------------
    df <- df %>%
        mutate(
            Signal = sign(Change),
            Signal = replace_na(Signal, 0),
            Accumulation = cumsum(Signal)
        )

    # ------------------------------------------------------------
    # 🔥 Indicators
    # ------------------------------------------------------------
    df <- df %>%
        mutate(
            RSI7   = RSI(Price, n = 7),
            RSI14  = RSI(Price, n = 14),
            RSI30  = RSI(Price, n = 30),
            RSI60  = RSI(Price, n = 60),
            RSI90  = RSI(Price, n = 90),
            RSI100 = RSI(Price, n = 100),
            RSI150 = RSI(Price, n = 150),
            RSI200 = RSI(Price, n = 200),

            EMA12  = EMA(Price, n = 12),
            EMA26  = EMA(Price, n = 26),
            EMA50  = EMA(Price, n = 50),
            EMA100 = EMA(Price, n = 100),
            EMA150 = EMA(Price, n = 150),
            EMA200 = EMA(Price, n = 200),

            dif_EMA1226  = EMA12 - EMA26,
            dif_EMA50200 = EMA50 - EMA200,
            perc_dif_EMA1226 = (EMA12 - EMA26) / EMA26 * 100,
            perc_dif_EMA50200 = (EMA50 - EMA200) / EMA200 * 100
        ) %>%
        mutate(
            macd_obj = MACD(Price, nFast = 12, nSlow = 26, nSig = 9),
            MACD = macd_obj[, 1],
            MACDSignal = macd_obj[, 2],
            MACDHist = MACD - MACDSignal
        ) %>%
        select(-macd_obj) %>%
        mutate(
            ATR14 = ATR(cbind(High, Low, Price), n = 14)[, "atr"],
            Volatility30 = runSD(LogReturn, n = 30)
        )

    # ------------------------------------------------------------
    # 🔥 Price structure
    # ------------------------------------------------------------
    df <- df %>%
        mutate(
            Mid = (High + Low) / 2,
            Mid_H_pct = (High - Mid) / Mid * 100,
            Mid_L_pct = (Low  - Mid) / Mid * 100
        )

    # ------------------------------------------------------------
    # 🔥 Trend, Returns, Pressure
    # ------------------------------------------------------------
    df <- df %>%
        mutate(
            Trend_EMA = ifelse(EMA50 > EMA200, 1, -1),

            EMA_Spread = ((EMA50 - EMA200) / EMA200) * 100,

            Return_5d = Price / lag(Price, 5) - 1,
            Return_10d = Price / lag(Price, 10) - 1,

            ATR_pct = ATR14 / Price * 100,

            Buy_Pressure = (Price - Low) / (High - Low),
            Sell_Pressure = (High - Price) / (High - Low),

            RSI_Change = RSI14 - lag(RSI14),

            Overbought = ifelse(RSI14 > 70, 1, 0),
            Oversold   = ifelse(RSI14 < 30, 1, 0)
        )

    # ------------------------------------------------------------
    # 🔥 Signals (Long / Short)
    # ------------------------------------------------------------
    df <- df %>%
        mutate(
            Long_Signal = ifelse(
                Trend_EMA == 1 &
                RSI14 < 40 &
                MACDHist > 0,
                1, 0
            ),

            Short_Signal = ifelse(
                Trend_EMA == -1 &
                RSI14 > 60 &
                MACDHist < 0,
                1, 0
            )
        )

    # ------------------------------------------------------------
    # 🔥 Lags + Breakout 10D + 20D (แก้ให้ใช้ High/Low)
    # ------------------------------------------------------------
    df <- df %>%
        mutate(
            Lag1 = lag(LogReturn, 1),
            Lag2 = lag(LogReturn, 2),
            Lag3 = lag(LogReturn, 3),

            Rolling_Max_10 = rollapply(lag(High), 10, max, fill = NA, align = "right"),
            Rolling_Min_10 = rollapply(lag(Low), 10, min, fill = NA, align = "right"),

            Rolling_Max_20 = rollapply(lag(High), 20, max, fill = NA, align = "right"),
            Rolling_Min_20 = rollapply(lag(Low), 20, min, fill = NA, align = "right"),

            Breakout_Up_10   = ifelse(!is.na(Rolling_Max_10) & High > Rolling_Max_10, 1, 0),
            Breakout_Down_10 = ifelse(!is.na(Rolling_Min_10) & Low  < Rolling_Min_10, 1, 0),

            Breakout_Up_20   = ifelse(!is.na(Rolling_Max_20) & High > Rolling_Max_20, 1, 0),
            Breakout_Down_20 = ifelse(!is.na(Rolling_Min_20) & Low  < Rolling_Min_20, 1, 0)
        )

    # ------------------------------------------------------------
    # 🔥 Risk & Leverage
    # ------------------------------------------------------------
    df <- df %>%
        mutate(
            Leverage = case_when(
                ATR_pct < 2 ~ 10,
                ATR_pct < 4 ~ 5,
                ATR_pct < 6 ~ 3,
                TRUE ~ 2
            ),

            SL_pct = ATR_pct * 1.2,
            TP_pct = SL_pct * 2,

            Long_TP  = Price * (1 + TP_pct / 100),
            Long_SL  = Price * (1 - SL_pct / 100),
            Short_TP = Price * (1 - TP_pct / 100),
            Short_SL = Price * (1 + SL_pct / 100),

            Liquidation_pct = 100 / Leverage,

            Risk_Level = case_when(
                Volatility30 > 0.06 ~ "High",
                Volatility30 > 0.03 ~ "Medium",
                TRUE ~ "Low"
            ),

            Signal_Label = case_when(
                Signal == 1 ~ "Up",
                Signal == 0 ~ "Neutral",
                Signal == -1 ~ "Down"
            ),

            Long_Signal_Filtered = ifelse(
                Long_Signal == 1 & Risk_Level != "High",
                1, 0
            )
        )

    # ------------------------------------------------------------
    # 🔥 TSMOM
    # ------------------------------------------------------------
    df <- df %>%
        mutate(
            signal_s = (lag(Price) - lag(EMA50)) / lag(Volatility30),
            signal_dir = sign(signal_s),

            pnl_tsmom = signal_dir * LogReturn / lag(Volatility30),
            pnl_tsmom = replace_na(pnl_tsmom, 0),

            Q_tsmom = cumsum(pnl_tsmom)
        )

    # ------------------------------------------------------------
    # 🔥 Aspect Model (แก้ Breakout + Volatility)
    # ------------------------------------------------------------
    df <- df %>%
        mutate(
            Trend_Aspect = Price > EMA200,

            Momentum_Aspect = Return_10d > 0,

            Breakout_Aspect =
                (!is.na(Rolling_Max_20) & High > Rolling_Max_20) |
                (!is.na(Rolling_Min_20) & Low  < Rolling_Min_20),

            Volatility_Aspect = ATR_pct > lag(ATR_pct)
        ) %>%
        mutate(
            Aspect_Label = case_when(

                !Trend_Aspect & !Momentum_Aspect & !Breakout_Aspect & !Volatility_Aspect ~
                    "Downtrend + Weak + No Breakout + Low Volatility",

                !Trend_Aspect & !Momentum_Aspect & !Breakout_Aspect & Volatility_Aspect ~
                    "Downtrend + Weak + No Breakout + Volatility Rising (Crash Risk)",

                !Trend_Aspect & !Momentum_Aspect & Breakout_Aspect & !Volatility_Aspect ~
                    "False Breakdown (Weak Downtrend Breakout)",

                !Trend_Aspect & !Momentum_Aspect & Breakout_Aspect & Volatility_Aspect ~
                    "Strong Breakdown (Downtrend Accelerating)",

                !Trend_Aspect & Momentum_Aspect & !Breakout_Aspect & !Volatility_Aspect ~
                    "Bear Rally (Dead Cat Bounce)",

                !Trend_Aspect & Momentum_Aspect & !Breakout_Aspect & Volatility_Aspect ~
                    "Strong Bear Rally (Bounce with Volatility)",

                !Trend_Aspect & Momentum_Aspect & Breakout_Aspect & !Volatility_Aspect ~
                    "False Bull Breakout (Up Breakout in Downtrend)",

                !Trend_Aspect & Momentum_Aspect & Breakout_Aspect & Volatility_Aspect ~
                    "Aggressive Bull Breakout but Still in Downtrend",

                Trend_Aspect & !Momentum_Aspect & !Breakout_Aspect & !Volatility_Aspect ~
                    "Uptrend but Weak Momentum (Pullback)",

                Trend_Aspect & !Momentum_Aspect & !Breakout_Aspect & Volatility_Aspect ~
                    "Uptrend Losing Strength (Volatility Rising)",

                Trend_Aspect & !Momentum_Aspect & Breakout_Aspect & !Volatility_Aspect ~
                    "Weak Breakout (Possible Fakeout)",

                Trend_Aspect & !Momentum_Aspect & Breakout_Aspect & Volatility_Aspect ~
                    "Breakout but Momentum Weak (Caution)",

                Trend_Aspect & Momentum_Aspect & !Breakout_Aspect & !Volatility_Aspect ~
                    "Early Uptrend (Momentum but No Breakout)",

                Trend_Aspect & Momentum_Aspect & !Breakout_Aspect & Volatility_Aspect ~
                    "Pre-Breakout (Trend + Momentum + Volatility Rising)",

                Trend_Aspect & Momentum_Aspect & Breakout_Aspect & !Volatility_Aspect ~
                    "Confirmed Breakout (Trend + Momentum)",

                Trend_Aspect & Momentum_Aspect & Breakout_Aspect & Volatility_Aspect ~
                    "Strong Trend (Best Long Condition)",

                TRUE ~ "Undefined"
            )
        )

    # ------------------------------------------------------------
    # 🔥 RSI Classes
    # ------------------------------------------------------------
    df <- df %>%
        mutate(
            RSI7_class = case_when(
                RSI7 >= 85 ~ "Extreme Overbought",
                RSI7 >= 70 ~ "Overbought",
                RSI7 >  30 ~ "Neutral",
                RSI7 >= 15 ~ "Oversold",
                RSI7 >= 0  ~ "Extreme Oversold",
                TRUE ~ "Check Data"
            ),

            RSI14_class = case_when(
                RSI14 >= 85 ~ "Extreme Overbought",
                RSI14 >= 70 ~ "Overbought",
                RSI14 >  30 ~ "Neutral",
                RSI14 >= 15 ~ "Oversold",
                RSI14 >= 0  ~ "Extreme Oversold",
                TRUE ~ "Check Data"
            ),

            RSI30_class = case_when(
                RSI30 >= 85 ~ "Extreme Overbought",
                RSI30 >= 70 ~ "Overbought",
                RSI30 >  30 ~ "Neutral",
                RSI30 >= 15 ~ "Oversold",
                RSI30 >= 0  ~ "Extreme Oversold",
                TRUE ~ "Check Data"
            ),

            RSI7_class_num = case_when(
                RSI7 >= 85 ~ "5",
                RSI7 >= 70 ~ "4",
                RSI7 >  30 ~ "3",
                RSI7 >= 15 ~ "2",
                RSI7 >= 0  ~ "1",
                TRUE ~ "Check Data"
            ),

            RSI14_class_num = case_when(
                RSI14 >= 85 ~ "5",
                RSI14 >= 70 ~ "4",
                RSI14 >  30 ~ "3",
                RSI14 >= 15 ~ "2",
                RSI14 >= 0  ~ "1",
                TRUE ~ "Check Data"
            ),

            RSI30_class_num = case_when(
                RSI30 >= 85 ~ "5",
                RSI30 >= 70 ~ "4",
                RSI30 >  30 ~ "3",
                RSI30 >= 15 ~ "2",
                RSI30 >= 0  ~ "1",
                TRUE ~ "Check Data"
            )
        )

    # ------------------------------------------------------------
    # 🔥 Final ordering
    # ------------------------------------------------------------
    df <- df %>% arrange(desc(Date))

    return(df)
}
