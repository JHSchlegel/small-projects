#=========================================================================#
############################### Backtesting ###############################
#=========================================================================#

## Import my Backtesting Functions
source("Scripts/Backtesting_Functions.R")
# see Backtesting_Functions.R for function documentation


# Note that we work with -VaR instead of VaR.
# This allows for a more intuitive interpretation and is also the default 
# output in R's rugarch package

#------------------------------------#
########### Importing Data ###########
#------------------------------------#

## Portfolio Plrets
stocks_plret_df <- read.csv("./Data/StockPlrets.csv", header = TRUE)
portfolio_plret_df <- read.csv("./Data/PortfolioPlrets.csv", header = TRUE)

## Import VaR Data
# Univariate:
Uni_Normal_GARCH_VaR <- read.csv("./Data/VaR/Uni_Normal_GARCH_VaR.csv", 
                                 header = TRUE)
Uni_EWMA_VaR <- read.csv("./Data/VaR/Uni_EWMA_VaR.csv", 
                         header = TRUE)
Uni_t_GJR_GARCH_VaR <- read.csv("./Data/VaR/Uni_t_GJR_GARCH.csv", 
                                header = TRUE)
Uni_Skewt_GJR_GARCH_VaR <- read.csv("./Data/VaR/Uni_Skewt_GJR_GARCH.csv", 
                                    header = TRUE)
Uni_Skewt_NGARCH_VaR <- read.csv("./Data/VaR/Uni_Skewt_NGARCH.csv", 
                                 header = TRUE)
Uni_MN_2_2_GARCH_vaR <- read.csv("./Data/VaR/Uni_MN_2_2_GARCH.csv", 
                                header = TRUE)
Uni_MN_3_3_GARCH_vaR <- read.csv("./Data/VaR/Uni_MN_3_3_GARCH.csv", 
                                 header = TRUE)


# Multivariate
Multi_DCC_GARCH_VaR <- read.csv("./Data/VaR/Multi_Normal_DCC_GARCH_Matlab.csv",
                                header = TRUE)

Fortin_Normal_NGARCH_VaR <- read.csv("./Data/VaR/Fortin_cop_norm_NGARCH.csv",
                                     header = TRUE)
Fortin_Normal_sGARCH_VaR <- read.csv("./Data/VaR/Fortin_cop_norm_sGARCH.csv",
                                     header = TRUE)
Fortin_t_NGARCH_VaR <- read.csv("./Data/VaR/Fortin_cop_t_NGARCH.csv",
                                header = TRUE)

Fortin_t_sGARCH_VaR <- read.csv("./Data/VaR/Fortin_cop_t_sGARCH.csv", 
                                header = TRUE)

Fortin_skewt_sGARCH_VaR <- read.csv("./Data/VaR/Fortin_cop_skewt_sGARCH.csv", 
                                header = TRUE)

COMFORT_MVG_CCC_GJR_VaR <- read.csv("./Data/VaR/COMFORT_MVG_CCC_GJR_VaR.csv",
                                    header = TRUE)
COMFORT_MVG_CCC_sGARCH_VaR <- read.csv(
  "./Data/VaR/COMFORT_MVG_CCC_sGARCH_VaR.csv", header = TRUE
  )




## Create list w/ all VaRs for performance table function
all_VaR_list <- list(Normal_GARCH = Uni_Normal_GARCH_VaR,
                     EWMA = Uni_EWMA_VaR, 
                     t_GJR = Uni_t_GJR_GARCH_VaR,
                     Skewt_GJR = Uni_Skewt_GJR_GARCH_VaR,
                     Skewt_NGARCH = Uni_Skewt_NGARCH_VaR,
                     MN_2_2_GARCH = Uni_MN_2_2_GARCH_vaR,
                     MN_3_3_GARCH = Uni_MN_3_3_GARCH_vaR,
                     Normal_DCC_GARCH = Multi_DCC_GARCH_VaR,
                     Fortin_Normal_NGARCH = Fortin_Normal_NGARCH_VaR,
                     Fortin_Normal_sGARCH = Fortin_Normal_sGARCH_VaR,
                     Fortin_t_NGARCH = Fortin_t_NGARCH_VaR,
                     Fortin_t_sGARCH = Fortin_t_sGARCH_VaR,
                     Fortin_skewt_sGARCH = Fortin_skewt_sGARCH_VaR,
                     COMFORT_MVG_CCC_GJR = COMFORT_MVG_CCC_GJR_VaR,
                     COMFORT_MVG_CCC_sGARCH = COMFORT_MVG_CCC_sGARCH_VaR)

#---------------------------------------#
########### Visual Inspection ###########
#---------------------------------------#

## Normal GARCH:
VaR_exceed_plot(Uni_Normal_GARCH_VaR, 3, portfolio_plret_df, VaR_percentile = 5,
                "Normal GARCH")
# even in non-crisis years many exceedances

VaR_exceed_plot(Uni_Normal_GARCH_VaR, 2, portfolio_plret_df, VaR_percentile = 1,
                "Normal GARCH")

## EWMA:
VaR_exceed_plot(Uni_Normal_GARCH_VaR, 3, portfolio_plret_df, VaR_percentile = 5,
                "Normal EWMA")
VaR_exceed_plot(Uni_Normal_GARCH_VaR, 2, portfolio_plret_df, VaR_percentile = 1,
                "Normal EWMA")

## t-GJR:
VaR_exceed_plot(Uni_t_GJR_GARCH_VaR, 3, portfolio_plret_df, VaR_percentile = 5,
                "t GJR")
VaR_exceed_plot(Uni_t_GJR_GARCH_VaR, 2, portfolio_plret_df, VaR_percentile = 1,
                "t GJR")

## skew-t GJR:
VaR_exceed_plot(Uni_Skewt_GJR_GARCH_VaR, 3, portfolio_plret_df, 
                VaR_percentile = 5, "skew-t GJR")
VaR_exceed_plot(Uni_Skewt_GJR_GARCH_VaR, 2, portfolio_plret_df, 
                VaR_percentile = 1, "skew-t GJR")

## skew-t NGARCH:
VaR_exceed_plot(Uni_Skewt_NGARCH_VaR, 3, portfolio_plret_df, VaR_percentile = 5,
                "skew-t NGARCH")
VaR_exceed_plot(Uni_Skewt_NGARCH_VaR, 2, portfolio_plret_df, VaR_percentile = 1,
                "skew-t NGARCH")

## MN(2,2)
VaR_exceed_plot(Uni_MN_2_2_GARCH_vaR, 3, portfolio_plret_df, VaR_percentile = 5,
                "MN(2,2)")
VaR_exceed_plot(Uni_MN_2_2_GARCH_vaR, 2, portfolio_plret_df, VaR_percentile = 1,
                "MN(2,2)")

## MN(3,3)
# weird spikes where -VaR is positive even during crisis of 2008
VaR_exceed_plot(Uni_MN_3_3_GARCH_vaR, 3, portfolio_plret_df, VaR_percentile = 5,
                "MN(3,3)")
VaR_exceed_plot(Uni_MN_3_3_GARCH_vaR, 2, portfolio_plret_df, VaR_percentile = 1,
                "MN(3,3)")

# way too "jumpy"

## Normal DCC:
VaR_exceed_plot(Multi_DCC_GARCH_VaR, 3, portfolio_plret_df, VaR_percentile = 5,
                "Normal DCC")
VaR_exceed_plot(Multi_DCC_GARCH_VaR, 2, portfolio_plret_df, VaR_percentile = 1,
                "Normal DCC")
# way too many exceedances; very low spike towards end of 2008 (< -30%)
# leads to very high tick loss

## Fortin Normal cgarch w/ NGARCH marginals:
VaR_exceed_plot(Fortin_Normal_NGARCH_VaR, 3, portfolio_plret_df, 
                VaR_percentile = 5, "Fortin Normal with NGARCH marginals")
VaR_exceed_plot(Fortin_Normal_NGARCH_VaR, 2, portfolio_plret_df, 
                VaR_percentile = 1, "Fortin Normal with NGARCH marginals")

## Fortin Normal cgarch w/ sGARCH marginals:
VaR_exceed_plot(Fortin_Normal_sGARCH_VaR, 3, portfolio_plret_df, 
                VaR_percentile = 5, "Fortin Normal with sGARCH marginals")
VaR_exceed_plot(Fortin_Normal_sGARCH_VaR, 2, portfolio_plret_df, 
                VaR_percentile = 1, "Fortin Normal with sGARCH marginals")

## Fortin t cgarch w/ NGARCH marginals:
VaR_exceed_plot(Fortin_t_NGARCH_VaR, 3, portfolio_plret_df, VaR_percentile = 5,
                "Fortin t with NGARCH marginals")
VaR_exceed_plot(Fortin_t_NGARCH_VaR, 2, portfolio_plret_df, VaR_percentile = 1,
                "Fortin t with NGARCH marginals")

## Fortin t cgarch w/ sGARCH marginals:
VaR_exceed_plot(Fortin_t_sGARCH_VaR, 3, portfolio_plret_df, VaR_percentile = 5,
                "Fortin t Copula with sGARCH marginals")
VaR_exceed_plot(Fortin_t_sGARCH_VaR, 2, portfolio_plret_df, VaR_percentile = 1,
                "Fortin t Copula with sGARCH marginals")

## Fortin skewed t cgarch w/ sGARCH marginals:
VaR_exceed_plot(Fortin_skewt_sGARCH_VaR, 3, portfolio_plret_df, VaR_percentile = 5,
                "Fortin skewed t Copula with sGARCH marginals")
VaR_exceed_plot(Fortin_skewt_sGARCH_VaR, 2, portfolio_plret_df, VaR_percentile = 1,
                "Fortin skewed t Copula with sGARCH marginals")


## COMFORT MVG CCC GJR:
VaR_exceed_plot(COMFORT_MVG_CCC_GJR_VaR, 3, portfolio_plret_df, 
                VaR_percentile = 5, "COMFORT MVG CCC GJR")
VaR_exceed_plot(COMFORT_MVG_CCC_GJR_VaR, 2, portfolio_plret_df, 
                VaR_percentile = 1, "COMFORT MVG CCC GJR")

## COMFORT MVG CCC sGARCH:
VaR_exceed_plot(COMFORT_MVG_CCC_sGARCH_VaR, 3, portfolio_plret_df,
                VaR_percentile = 5, "COMFORT MVG CCC sGARCH")
VaR_exceed_plot(COMFORT_MVG_CCC_GJR_VaR, 2, portfolio_plret_df, 
                VaR_percentile = 1, "COMFORT MVG CCC sGARCH")




#----------------------------------------------------------------#
########### Exceedances, Coverage and LR Tests Pvalues ###########
#----------------------------------------------------------------#
performance_table(all_VaR_list)$performance_table_01
performance_table(all_VaR_list)$performance_table_05


#---------------------------------------------------------#
########### Ranking According to Mean Tick Loss ###########
#---------------------------------------------------------#
VaR_loss_ranking(all_VaR_list)


#----------------------------------------------------------------#
########### CPA Tests as in Giacomini and White (2006) ###########
#----------------------------------------------------------------#
CPA_table(all_VaR_list)

