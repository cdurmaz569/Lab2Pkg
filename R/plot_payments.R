#' Box Plot of Average Payments
#'
#' This function produces a boxplot of payments by DRG code. It takes in an
#' argument to calculate the payments for either the average covered charges,
#' the average total payment, or the average Medicare payments.
#'
#' @param df dataframe that contains the following columns: 'DRG.Definition',
#' 'Average.Covered.Charges', 'Average.Total.Payments', and
#' 'Average.Medicare.Payments'.
#' @param pay_type string of payment type. Accepts 'covered' for average
#' covered charges, 'total' for average total payment, and 'medicare' for
#' average Medicare payments. Default: 'medicare'.
#'
#' @return boxplot of average payments for all DRG codes
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @importFrom dplyr case_when
#'
#' @examples
#' df.test <- drg
#' plot_payments(df.test, pay_type = "covered")
#'
plot_payments <- function(df, pay_type = "medicare"){

  # Create column in data frame with only DRG codes
  df$drg_codes <- gsub( " .*$", "", df$DRG.Definition)

  # Get column index of specified payment type
  idx = 0
  idx = case_when(

    pay_type == "covered" ~ 10,
    pay_type == "total" ~ 11,
    pay_type == "medicare" ~ 12

  )

  # Check if pay_type has appropriate input
  if(idx == 0){

    print("Unacceptable pay type. Choose from: 'covered', 'total', 'medicare'")

  } else{

    # Visualize data as boxplot
    ggplot(df, aes(x = drg_codes, y = df[, idx])) +
      geom_boxplot() +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(title = paste0(gsub("\\.", " ", colnames(df[idx])), " by DRG Code"),
           x = "DRG Code",
           y = gsub("\\.", " ", colnames(df[idx])))
  }

}
