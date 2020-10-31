#' Calculate Medicare Statistics
#'
#' This function calculates statistics over all the DRG codes for average
#' Medicare payments. It can calculate either the mean, median, or standard
#' deviation of the DRG codes.
#'
#' @param df dataframe that contains the following columns: 'DRG.Definition',
#' 'Average.Covered.Charges', 'Average.Total.Payments', and
#' 'Average.Medicare.Payments'.
#' @param stat string specifying statistic to calculate. Accepts 'mean',
#' 'median', or 'sd'. Default: 'mean'.
#'
#' @return double of specified statistic (mean, median, or sd)
#' @export
#'
#' @examples
#' df.test <- drg
#' get_medicare_stats(drg, stat = "sd")
get_medicare_stats <- function(df, stat = "mean"){

  # Calculate statistic given specified stat
  if(stat == "mean"){

    # Return calculated mean
    return(mean(df$Average.Medicare.Payments))

  } else if(stat == "median"){

    # Return calculated median
    return(median(df$Average.Medicare.Payments))

  } else if(stat == "sd"){

    # Return calculated sd
    return(sd(df$Average.Medicare.Payments))

  } else{

    print("Unacceptable stat. Choose from: 'mean', 'median', 'sd'")

  }

}
