#' Screen Time
#'
#' EXPERIMENTAL STATUS: Provides an estimate of screen time for each contestant in the US42 season.
#' TODO: Improve video sampling rate (currently every 5 seconds)
#' TODO: Extend dataset to additional seasons
#' TODO: Do not classify faces if match likelihood is sufficiently low (currently has a high false positive rate)
#'
#' A dataset summarising the screen time of contestants on the TV show Survivor.
#'
#' @format This data frame contains the following columns:
#' \describe{
#'   \item{\code{version_season}}
#'   \item{\code{season_ts}}{Timestamp in seconds from the beginning of the season}
#'   \item{\code{episode}}{Episode number}
#'   \item{\code{episode_ts}}{Timestamp in seconds from the beginning of the episode}
#'   \item{\code{castaway_id}}{ID of the castaway (primary key). Consistent across seasons and name changes e.g. Amber Brkich / Amber Mariano. The first two letters reference the country of the version played e.g. US, AU (TBA).}
#'   \item{\code{screen_time}}{Assumed amount of screen time for the individual based on the video sampling length.}
#' }
"screen_time"
