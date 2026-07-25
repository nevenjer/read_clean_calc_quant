read_clean_calc_quant <- function(x) {
    
    library(dplyr)
    library(lubridate)
    library(TTR)
    library(tidyverse)
    library(zoo)
    library(tidyr)
    
    # --- เพิ่มแค่ส่วนนี้ ---
    if (is.character(x)) {
        df <- read.csv(x)
    } else if (is.data.frame(x)) {
        df <- x
    } else {
        stop("Input must be a filename (character) or data.frame")
    }
    
    # --- ส่วนเดิมทั้งหมดของคุณ เริ่มตรงนี้ ---
    
    colnames(df)[1:7] <- c(
        "Date", "Price", "Open", "High", "Low", "Volume", "Change"
    )
    
    df <- df %>%
        mutate(
            Date  = mdy(Date),
            Price = as.numeric(gsub(",", "", Price)),
            Open  = as.numeric(gsub(",", "", Open)),
            High  = as.numeric(gsub(",", "", High)),
            Low   = as.numeric(gsub(",", "", Low)),
            
            Volume = gsub(",", "", Volume),
            Volume = case_when(
                grepl("K", Volume) ~ as.numeric(gsub("K", "", Volume)) * 1e3,
                grepl("M", Volume) ~ as.numeric(gsub("M", "", Volume)) * 1e6,
                Volume %in% c("", "-", "null", "N/A", NA) ~ NA_real_,
                TRUE ~ suppressWarnings(as.numeric(Volume))
            ),
            
            Change = as.numeric(gsub("%", "", Change))
        ) %>%
        
        arrange(Date) %>%
        
        mutate(
            Normalized = Price / first(Price) * 100
        ) %>%
        
        mutate(
            LogReturn = log(Price / lag(Price))
        ) %>%
        
        mutate(
            CumReturn = cumsum(replace_na(LogReturn, 0))
        ) %>%
        
        mutate(
            RSI7   = RSI(Price, n = 7),
            RSI14  = RSI(Price, n = 14),
            RSI30  = RSI(Price, n = 30),
            RSI60  = RSI(Price, n = 60),
            RSI90  = RSI(Price, n = 90),
            RSI100 = RSI(Price, n = 100),
            RSI150 = RSI(Price, n = 150),
            RSI200 = RSI(Price, n = 200)
        ) %>%
        
        mutate(
            EMA12  = EMA(Price, n = 12),
            EMA26  = EMA(Price, n = 26),
            EMA50  = EMA(Price, n = 50),
            EMA100 = EMA(Price, n = 100),
            EMA150 = EMA(Price, n = 150),
            EMA200 = EMA(Price, n = 200),
            dif_EMA1226  = EMA12 - EMA26,
            dif_EMA50200 = EMA50 - EMA200
        ) %>%
        
        mutate(
            macd_obj = MACD(Price, nFast = 12, nSlow = 26, nSig = 9),
            MACD = macd_obj[, 1],
            MACDSignal = macd_obj[, 2],
            MACDHist = MACD - MACDSignal
        ) %>%
        select(-macd_obj) %>%
        
        mutate(
            ATR14 = ATR(cbind(High, Low, Price), n = 14)[, "atr"]
        ) %>%
        
        mutate(
            Volatility30 = runSD(LogReturn, n = 30)
        ) %>%
        
        mutate(
            Signal = ifelse(Change > 0, 1,
                            ifelse(Change < 0, -1, 0))
        ) %>%
        
        mutate(
            Accumulation = cumsum(Signal)
        ) %>%
        
        mutate(
            Mid = (High + Low) / 2,
            Mid_H_pct = (High - Mid) / Mid * 100,
            Mid_L_pct = (Low  - Mid) / Mid * 100
        ) %>%
        
        mutate(
            Trend_EMA = ifelse(EMA50 > EMA200, 1, -1),
            EMA_Spread = (EMA50 - EMA200) / EMA200 * 100
        ) %>%
        
        mutate(
            Return_5d = Price / lag(Price, 5) - 1,
            Return_10d = Price / lag(Price, 10) - 1,
            ATR_pct = ATR14 / Price * 100
        ) %>%
        
        mutate(
            Buy_Pressure = (Price - Low) / (High - Low),
            Sell_Pressure = (High - Price) / (High - Low)
        ) %>%
        
        mutate(
            RSI_Change = RSI14 - lag(RSI14),
            Overbought = ifelse(RSI14 > 70, 1, 0),
            Oversold = ifelse(RSI14 < 30, 1, 0)
        ) %>%  
        
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
        ) %>%      
        
        mutate(
            Lag1 = lag(LogReturn, 1),
            Lag2 = lag(LogReturn, 2),
            Lag3 = lag(LogReturn, 3),
            
            Rolling_Max_10 = zoo::rollmax(Price, 10, fill = NA, align = "right"),
            Rolling_Min_10 = zoo::rollapply(Price, 10, min, fill = NA, align = "right"),
            
            Breakout_Up = ifelse(Price > Rolling_Max_10, 1, 0),
            Breakout_Down = ifelse(Price < Rolling_Min_10, 1, 0)
        ) %>%
        
        mutate(
            Leverage = case_when(
                ATR_pct < 2 ~ 10,
                ATR_pct < 4 ~ 5,
                ATR_pct < 6 ~ 3,
                TRUE ~ 2
            ),
            
            SL_pct = ATR_pct * 1.2,
            TP_pct = SL_pct * 2,
            
            Long_TP = Price * (1 + TP_pct / 100),
            Long_SL = Price * (1 - SL_pct / 100),
            
            Short_TP = Price * (1 - TP_pct / 100),
            Short_SL = Price * (1 + SL_pct / 100),
            
            Liquidation_pct = 100 / Leverage
        ) %>%
        
        mutate(
            Risk_Level = case_when(
                Volatility30 > 0.06 ~ "High",
                Volatility30 > 0.03 ~ "Medium",
                TRUE ~ "Low"
            )
        ) %>%
        
        mutate(
            Signal_Label = case_when(
                Signal == 1  ~ "Up",
                Signal == 0  ~ "Neutral",
                Signal == -1 ~ "Down",
                TRUE         ~ NA_character_ # กันเหนียวไว้ถ้ามีค่าอื่นที่ไม่ใช่ 1, 0, -1
            )
        ) %>%
        
        mutate(
            Long_Signal_Filtered = ifelse(
                Long_Signal == 1 & Risk_Level != "High",
                1, 0
            )
        ) %>% 
        
        mutate(
            signal_s = (lag(Price) - lag(EMA50)) / lag(Volatility30),
            signal_dir = sign(signal_s),
            pnl_tsmom = signal_dir * LogReturn / lag(Volatility30),
            Q_tsmom = cumsum(replace_na(pnl_tsmom, 0))
        ) %>%
        
        mutate(
            Trend_Aspect      = Price > EMA200,
            Momentum_Aspect   = Return_10d > 0,
            Breakout_Aspect   = Price > Rolling_Max_10,
            Volatility_Aspect = ATR14 > lag(ATR14)
        ) %>%
        
        mutate(
            Aspect_Label = case_when(
                
                # 1) FALSE FALSE FALSE FALSE
                !Trend_Aspect & !Momentum_Aspect & !Breakout_Aspect & !Volatility_Aspect ~ 
                    "Downtrend + Weak + No Breakout + Low Volatility",
                
                # 2) FALSE FALSE FALSE TRUE
                !Trend_Aspect & !Momentum_Aspect & !Breakout_Aspect & Volatility_Aspect ~ 
                    "Downtrend + Weak + No Breakout + Volatility Rising (Crash Risk)",
                
                # 3) FALSE FALSE TRUE FALSE
                !Trend_Aspect & !Momentum_Aspect & Breakout_Aspect & !Volatility_Aspect ~ 
                    "False Breakdown (Weak Downtrend Breakout)",
                
                # 4) FALSE FALSE TRUE TRUE
                !Trend_Aspect & !Momentum_Aspect & Breakout_Aspect & Volatility_Aspect ~ 
                    "Strong Breakdown (Downtrend Accelerating)",
                
                # 5) FALSE TRUE FALSE FALSE
                !Trend_Aspect & Momentum_Aspect & !Breakout_Aspect & !Volatility_Aspect ~ 
                    "Bear Rally (Dead Cat Bounce)",
                
                # 6) FALSE TRUE FALSE TRUE
                !Trend_Aspect & Momentum_Aspect & !Breakout_Aspect & Volatility_Aspect ~ 
                    "Strong Bear Rally (Bounce with Volatility)",
                
                # 7) FALSE TRUE TRUE FALSE
                !Trend_Aspect & Momentum_Aspect & Breakout_Aspect & !Volatility_Aspect ~ 
                    "False Bull Breakout (Up Breakout in Downtrend)",
                
                # 8) FALSE TRUE TRUE TRUE
                !Trend_Aspect & Momentum_Aspect & Breakout_Aspect & Volatility_Aspect ~ 
                    "Aggressive Bull Breakout but Still in Downtrend",
                
                # 9) TRUE FALSE FALSE FALSE
                Trend_Aspect & !Momentum_Aspect & !Breakout_Aspect & !Volatility_Aspect ~ 
                    "Uptrend but Weak Momentum (Pullback)",
                
                # 10) TRUE FALSE FALSE TRUE
                Trend_Aspect & !Momentum_Aspect & !Breakout_Aspect & Volatility_Aspect ~ 
                    "Uptrend Losing Strength (Volatility Rising)",
                
                # 11) TRUE FALSE TRUE FALSE
                Trend_Aspect & !Momentum_Aspect & Breakout_Aspect & !Volatility_Aspect ~ 
                    "Weak Breakout (Possible Fakeout)",
                
                # 12) TRUE FALSE TRUE TRUE
                Trend_Aspect & !Momentum_Aspect & Breakout_Aspect & Volatility_Aspect ~ 
                    "Breakout but Momentum Weak (Caution)",
                
                # 13) TRUE TRUE FALSE FALSE
                Trend_Aspect & Momentum_Aspect & !Breakout_Aspect & !Volatility_Aspect ~ 
                    "Early Uptrend (Momentum but No Breakout)",
                
                # 14) TRUE TRUE FALSE TRUE
                Trend_Aspect & Momentum_Aspect & !Breakout_Aspect & Volatility_Aspect ~ 
                    "Pre-Breakout (Trend + Momentum + Volatility Rising)",
                
                # 15) TRUE TRUE TRUE FALSE
                Trend_Aspect & Momentum_Aspect & Breakout_Aspect & !Volatility_Aspect ~ 
                    "Confirmed Breakout (Trend + Momentum)",
                
                # 16) TRUE TRUE TRUE TRUE
                Trend_Aspect & Momentum_Aspect & Breakout_Aspect & Volatility_Aspect ~ 
                    "Strong Trend (Best Long Condition)",
                
                TRUE ~ "Undefined"
            )
        ) %>%
        
            mutate(
            RSI7_class = case_when(
            RSI7 >= 85  ~ "Extreme Overbought",
            RSI7 >= 70  ~ "Overbought",
            RSI7 >  30  ~ "Neutral", # ครอบคลุมตั้งแต่ 30.00001 ถึง 69.9999
            RSI7 >= 15  ~ "Oversold",
            RSI7 >= 0   ~ "Extreme Oversold",
            TRUE         ~ "Check Data" # ถ้ายังหลุดมาอีก แสดงว่าข้อมูลอาจเป็นค่าว่าง (Null)
            )
        ) %>%

        mutate(
            RSI14_class = case_when(
            RSI14 >= 85  ~ "Extreme Overbought",
            RSI14 >= 70  ~ "Overbought",
            RSI14 >  30  ~ "Neutral", # ครอบคลุมตั้งแต่ 30.00001 ถึง 69.9999
            RSI14 >= 15  ~ "Oversold",
            RSI14 >= 0   ~ "Extreme Oversold",
            TRUE         ~ "Check Data" # ถ้ายังหลุดมาอีก แสดงว่าข้อมูลอาจเป็นค่าว่าง (Null)
            )
        ) %>%

        mutate(
            RSI30_class = case_when(
            RSI30 >= 85  ~ "Extreme Overbought",
            RSI30 >= 70  ~ "Overbought",
            RSI30 >  30  ~ "Neutral", # ครอบคลุมตั้งแต่ 30.00001 ถึง 69.9999
            RSI30 >= 15  ~ "Oversold",
            RSI30 >= 0   ~ "Extreme Oversold",
            TRUE         ~ "Check Data" # ถ้ายังหลุดมาอีก แสดงว่าข้อมูลอาจเป็นค่าว่าง (Null)
            )
        ) %>% 

            mutate(
            RSI7_class_num = case_when(
            RSI7 >= 85  ~ "5",
            RSI7 >= 70  ~ "4",
            RSI7 >  30  ~ "3", 
            RSI7 >= 15  ~ "2",
            RSI7 >= 0   ~ "1",
            TRUE         ~ "Check Data" 
            )
        ) %>%

        mutate(
            RSI14_class_num = case_when(
            RSI14 >= 85  ~ "5",
            RSI14 >= 70  ~ "4",
            RSI14 >  30  ~ "3", 
            RSI14 >= 15  ~ "2",
            RSI14 >= 0   ~ "1",
            TRUE         ~ "Check Data" 
            )
        ) %>%

        mutate(
            RSI30_class_num = case_when(
            RSI30 >= 85  ~ "5",
            RSI30 >= 70  ~ "4",
            RSI30 >  30  ~ "3", 
            RSI30 >= 15  ~ "2",
            RSI30 >= 0   ~ "1",
            TRUE         ~ "Check Data" 
            )
        ) %>%     
            
    # -----------------------------
    # \ud83d\udd25 จัดเรียงคอลัมน์ทั้งหมดใหม่
    # -----------------------------
    select(
        # Base
        Date, Price, Open, High, Low, Volume, Change,
        Normalized, LogReturn, CumReturn,
        
        # Price structure
        Mid, Mid_H_pct, Mid_L_pct,
        
        # Volatility
        Volatility30, ATR14, ATR_pct,
        
        # RSI
        RSI7, RSI14, RSI30, RSI60, RSI90, RSI100, RSI150, RSI200,
        RSI_Change, Overbought, Oversold,
        
        # EMA
        EMA12, EMA26, EMA50, EMA100, EMA150, EMA200,
        Trend_EMA, EMA_Spread, dif_EMA1226, dif_EMA50200
        
        # MACD
        MACD, MACDSignal, MACDHist,
        
        # Returns
        Return_5d, Return_10d,
        
        # Pressure
        Buy_Pressure, Sell_Pressure,
        
        # Signals
        Signal, Accumulation,
        Long_Signal, Short_Signal,
        Long_Signal_Filtered,
        
        # Breakout
        Rolling_Max_10, Rolling_Min_10,
        Breakout_Up, Breakout_Down,
        
        # Risk & Leverage
        Leverage, SL_pct, TP_pct,
        Long_TP, Long_SL,
        Short_TP, Short_SL,
        Liquidation_pct,
        Risk_Level, Signal_Label,
        
        # TSMOM
        signal_s, signal_dir, pnl_tsmom, Q_tsmom,
        
        # Lags
        Lag1, Lag2, Lag3,
        
        # Aspect
        Trend_Aspect, Momentum_Aspect, Breakout_Aspect, Volatility_Aspect, Aspect_Label, 
        RSI7_class, RSI14_class, RSI30_class, RSI7_class_num, RSI14_class_num, RSI30_class_num
    ) %>%
        
        # -----------------------------
    # \ud83d\udd25 ขั้นตอนสุดท้าย: เรียงวันที่ล่าสุดบนสุด
    # -----------------------------
    arrange(desc(Date))
    
    return(df)
}
