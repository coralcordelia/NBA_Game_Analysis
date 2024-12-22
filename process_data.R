game_scores_by_quarter <- read.csv("archive/csv/line_score.csv")
game_desc_small <- read.csv("archive/csv/game_summary.csv")
game_desc_large <- read.csv("archive/csv/game.csv")
attendance <- read.csv("archive/csv/game_info.csv")
nba_teams <- read.csv("archive/csv/team.csv")

library("ppcor")

mem.maxVSize(10**15)
# install.packages("remotes") # unless you have it already
# remotes::install_github("jtextor/dagitty/r")


game_scores_by_quarter <- game_scores_by_quarter[order(game_scores_by_quarter$game_id), ]
game_desc_small <- game_desc_small[order(game_desc_small$game_id), ]
game_desc_large <- game_desc_large[order(game_desc_large$game_id), ]
attendance <- attendance[order(attendance$game_id), ]
q1_diff <- c()
q2_diff <- c()
q3_diff <- c()
q4_diff <- c()
fg_pct_diff <- c()
tov_diff <- c()

df_list <- list(game_scores_by_quarter, game_desc_large, game_desc_small, attendance)

all_game_data <- Reduce(function(x, y) merge(x, y, by = "game_id", all.x = TRUE), df_list)



# print(summary(game_scores_by_quarter))
# print(head(all_game_data, 50))

home_w <- c()
all_teams <- c()
all_years <- c(1970:2022)
season_wl <- matrix(0, length(nba_teams$id), length(all_years))
wl_by_game_home <- c()
wl_by_game_away <- c()
home_skill_adv <- c()
more_points_q1 <- c()
more_points_q2 <- c()
more_points_q3 <- c()
more_points_q4 <- c()
season_type_bin <- c()
pts_diff <- c()


# print(season_wl)

# print(length(nba_teams$id))
print(colnames(all_game_data))



# Only include games between NBA teams
all_game_data <- all_game_data[all_game_data$team_id_home.x %in% nba_teams$id, ]
all_game_data <- all_game_data[all_game_data$team_id_away.x %in% nba_teams$id, ]
all_game_data <- all_game_data[all_game_data$season_id %% 10000 >= 1970, ]
all_game_data <- all_game_data[!(all_game_data$attendance %in% NA), ]
all_game_data <- all_game_data[!(all_game_data$tov_home %in% NA), ]
all_game_data <- all_game_data[!(all_game_data$fg_pct_home %in% NA), ]
all_game_data <- all_game_data[all_game_data$season_type %in% c("Regular Season", "Playoffs"), ]
all_game_data <- all_game_data[!(all_game_data$pts_qtr1_home %in% NA), ]
all_game_data <- all_game_data[!(all_game_data$season_id %% 10000 %in% c(1970, 1975, 1976, 2012)), ]
all_game_data <- all_game_data[!(all_game_data$home_team_id != all_game_data$team_id_home.x), ]
all_game_data <- all_game_data[!(all_game_data$team_id_home.y != all_game_data$team_id_home.x), ]

print(nrow(all_game_data))

for (i in 1:nrow(all_game_data)) {
    q1_diff[i] <- all_game_data$pts_qtr1_home[i] - all_game_data$pts_qtr1_away[i]
    q2_diff[i] <- all_game_data$pts_qtr2_home[i] - all_game_data$pts_qtr2_away[i]
    q3_diff[i] <- all_game_data$pts_qtr3_home[i] - all_game_data$pts_qtr3_away[i]
    q4_diff[i] <- all_game_data$pts_qtr4_home[i] - all_game_data$pts_qtr4_away[i]
    fg_pct_diff[i] <- all_game_data$fg_pct_home[i] - all_game_data$fg_pct_away[i]
    tov_diff[i] <- all_game_data$tov_home[i] - all_game_data$tov_away[i]

    year_num <- all_game_data$season_id[i] %% 10000
    home_team <- all_game_data$home_team_id[i]
    away_team <- all_game_data$visitor_team_id[i]
    season_type_bin[i] <- all_game_data$season_id[i] %/% 20000
    pts_diff[i] <- all_game_data$pts_home.x[i] - all_game_data$pts_away.x[i]
    if (all_game_data$wl_home[i] == "W") {
        home_w[i] <- 1
        if (all_game_data$season_type[i] == "Regular Season") {
            # print(which(nba_teams$id == home_team))
            # print(year_num - 1969)
            season_wl[which(nba_teams$id == home_team), year_num - 1969] <- season_wl[which(nba_teams$id == home_team), year_num - 1969] + 1
            season_wl[which(nba_teams$id == away_team), year_num - 1969] <- season_wl[which(nba_teams$id == away_team), year_num - 1969] - 1
        }
    } else {
        home_w[i] <- 0
        if (all_game_data$season_type[i] == "Regular Season") {
            season_wl[which(nba_teams$id == home_team), year_num - 1969] <- season_wl[which(nba_teams$id == home_team), year_num - 1969] - 1
            season_wl[which(nba_teams$id == away_team), year_num - 1969] <- season_wl[which(nba_teams$id == away_team), year_num - 1969] + 1
        }
    }
}

wl_diff <- c()

for (i in 1:nrow(all_game_data)) {
    year_num <- all_game_data$season_id[i] %% 10000
    home_team <- all_game_data$home_team_id[i]
    away_team <- all_game_data$visitor_team_id[i]
    wl_by_game_home[i] <- season_wl[which(nba_teams$id == home_team), year_num - 1969]
    wl_by_game_away[i] <- season_wl[which(nba_teams$id == away_team), year_num - 1969]
    wl_diff[i] <- season_wl[which(nba_teams$id == home_team), year_num - 1969] - season_wl[which(nba_teams$id == away_team), year_num - 1969]
    if (wl_by_game_home[i] + runif(1, min = -0.1, max = 0.1) > wl_by_game_away[i]) {
        home_skill_adv[i] <- 1
    } else {
        home_skill_adv[i] <- 0
    }
    if (all_game_data$pts_qtr1_home[i] + runif(1, min = -0.1, max = 0.1) > all_game_data$pts_qtr1_away[i]) {
        more_points_q1[i] <- 1
    } else {
        more_points_q1[i] <- 0
    }
    if (all_game_data$pts_qtr2_home[i] + runif(1, min = -0.1, max = 0.1) > all_game_data$pts_qtr2_away[i]) {
        more_points_q2[i] <- 1
    } else {
        more_points_q2[i] <- 0
    }
    if (all_game_data$pts_qtr3_home[i] + runif(1, min = -0.1, max = 0.1) > all_game_data$pts_qtr3_away[i]) {
        more_points_q3[i] <- 1
    } else {
        more_points_q3[i] <- 0
    }
    if (all_game_data$pts_qtr4_home[i] + runif(1, min = -0.1, max = 0.1) > all_game_data$pts_qtr4_away[i]) {
        more_points_q4[i] <- 1
    } else {
        more_points_q4[i] <- 0
    }
}




binarized_df <- data.frame(home_w, home_skill_adv, more_points_q1, more_points_q2, more_points_q3, more_points_q4, season_type_bin, all_game_data$game_id, all_game_data$tov_home, all_game_data$tov_away, all_game_data$fg3a_home, all_game_data$fg3a_away, all_game_data$pf_home, all_game_data$pf_away, all_game_data$fg_pct_home, all_game_data$fg_pct_away)

# mod_5 <- binarized_df[binarized_df$more_points_q1 %in% 1,]
# mod_5 <- mod_5[mod_5$more_points_q2 %in% 1,]
# mod_5 <- mod_5[mod_5$more_points_q3 %in% 1,]
# mod_5 <- mod_5[mod_5$more_points_q4 %in% 1,]
# mod_5 <- mod_5[mod_5$home_w %in% 0,]
# print(nrow(mod_5))

binarized_df <- as.data.frame(scale(binarized_df))

# print(head(home_w, 110))

# for (i in 1:ncol(binarized_df)) {
#     print(head(binarized_df[[i]]))
#     med <- median(binarized_df[[i]])
#     for (j in 1:nrow(binarized_df)) {
#         if (binarized_df[[i]][j] >= med) {
#             binarized_df[[i]][j] = 1
#         }
#         else {
#             binarized_df[[i]][j] = 0
#         }
#     }
# }


# print(summary(binarized_df))

# print(sample(all_game_data$season_id, 50))

# print(head(binarized_df, 20))

mod_binarized_df_1 <- binarized_df[season_type_bin %in% 2, ]
mod_binarized_df_2 <- binarized_df[more_points_q1 %in% 0, ]

# print(summary(mod_binarized_df_1))
# print(summary(mod_binarized_df_2))




find_ace <- function(df) {
    if (ncol(df) == 2) {
        print(table(df))
        output <- mean(df[df[[1]] %in% 1, ][[2]]) - mean(df[df[[1]] %in% 0, ][[2]])
        return(output)
    } else {
        last_condition <- df[[ncol(df)]]
        rest <- df[c(1:ncol(df) - 1)]
        df_cond_1 <- rest[last_condition == 1, ]
        df_cond_1 <- na.omit(df_cond_1)
        df_cond_0 <- rest[last_condition == 0, ]
        df_cond_0 <- na.omit(df_cond_0)
        return((find_ace(df_cond_1) * nrow(df_cond_1) + find_ace(df_cond_0) * nrow(df_cond_0)) / (nrow(df_cond_1) + nrow(df_cond_0)))
    }
}



# find_ace <- function(col1, col2, conditions = data.frame(matrix(nrow = 0, ncol = 0))) {
#     temp_df <- data.frame(col1, col2)
#     print(class(conditions))
#     print(head(conditions))
#     if (ncol(conditions) == 0) {
#         print(table(temp_df))
#         output <- mean(temp_df[col1 %in% 1,]$col2) - mean(temp_df[col1 %in% 0,]$col2)
#         return (output)
#     }
#     else {
#         first_condition = conditions[[1]]
#         # print(head(first_condition))
#         if (ncol(conditions) >= 3) {
#             rest_of_conditions <- conditions[c(2:ncol(conditions))]
#             # print(head(rest_of_conditions))
#         }
#         else if (ncol(conditions == 2)) {
#             rest_of_conditions <- conditions[2]
#             rest_of_conditions <- data.frame(rest_of_conditions)
#             print('hi')
#             print(head(rest_of_conditions))
#         }
#         else {
#             rest_of_conditions <- data.frame(matrix(nrow = 0, ncol = 0))
#         }
#         temp_df_1 <- temp_df[first_condition %in% 1,]
#         temp_df_1 <- na.omit(temp_df_1)
#         temp_df_0 <- temp_df[first_condition %in% 0,]
#         temp_df_0 <- na.omit(temp_df_0)
#         if (ncol(rest_of_conditions) >= 1) {
#         rest_of_conditions_1 <- rest_of_conditions[first_condition %in% 1,]
#         rest_of_conditions_1 <- data.frame(na.omit(rest_of_conditions_1))
#         rest_of_conditions_0 <- rest_of_conditions[first_condition %in% 0,]
#         rest_of_conditions_0 <- data.frame(na.omit(rest_of_conditions_0))
#         }
#         else {
#             rest_of_conditions_1 <- data.frame(matrix(nrow = 0, ncol = 0))
#             rest_of_conditions_0 <- data.frame(matrix(nrow = 0, ncol = 0))
#         }

#         # print('tdf')
#         # print(nrow(temp_df))
#         # print(nrow(temp_df_1))
#         # print(nrow(temp_df_0))
#         print(1)
#         output_1 <- (find_ace(temp_df_1$col1, temp_df_1$col2, rest_of_conditions_1) * nrow(temp_df_1)) / (nrow(temp_df))
#         print(0)
#         output_2 <- (find_ace(temp_df_0$col1, temp_df_0$col2, rest_of_conditions_0) * nrow(temp_df_0)) / (nrow(temp_df))
#         return (output_1 + output_2)
#     }
# }

all_game_data <- cbind(all_game_data, data.frame(
    wl_diff,
    q1_diff,
    q2_diff,
    q3_diff,
    q4_diff,
    pts_diff,
    tov_diff,
    fg_pct_diff
))

playoffs_only <- all_game_data[all_game_data$season_type %in% "Playoffs", ]


inp_table <- table(binarized_df$all_game_data.fg3a_home, binarized_df$home_w)
# print(inp_table)
# print(chisq.test(inp_table))






# print(find_ace(
#     data.frame(binarized_df$home_skill_adv, binarized_df$home_w)))
# print(find_ace(
#     data.frame(
#         binarized_df$home_skill_adv, binarized_df$home_w
#         ,binarized_df$more_points_q1, binarized_df$more_points_q2
#         ,binarized_df$more_points_q3, binarized_df$more_points_q4
#         )))

# arg <- data.frame(
#         all_game_data$attendance,
#         pts_diff,
#         all_game_data$pts_qtr1_home,
#         all_game_data$pts_qtr2_home,
#         all_game_data$pts_qtr3_home,
#         all_game_data$pts_qtr4_home,
#         all_game_data$pts_qtr1_away,
#         all_game_data$pts_qtr2_away,
#         all_game_data$pts_qtr3_away,
#         all_game_data$pts_qtr4_away
#     )

print(nrow(playoffs_only))

# arg <- data.frame(
#     x1 <- playoffs_only$wl_diff,
#     x2 <- playoffs_only$q1_diff,
#     x3 <- playoffs_only$q2_diff,
#     x4 <- playoffs_only$q3_diff,
#     y <- playoffs_only$q4_diff
# )

print(summary(playoffs_only))

arg <- data.frame(
    y <- all_game_data$q2_diff,
    x1 <- all_game_data$q3_diff
)


lin_mod <- lm("y ~ x1", data = arg)

print(summary(lin_mod))

# find_stats_corr_coef(0.9840687, data.frame(playoffs_only$q1_diff, playoffs_only$pts_diff))

# arg <- data.frame(
#         playoffs_only$wl_diff
#         # ,playoffs_only$pts_diff
#         ,playoffs_only$fg_pct_diff
#         # ,playoffs_only$tov_diff
#         # ,playoffs_only$attendance
#     )

# arg <- data.frame(
#         all_game_data$wl_diff,
#         all_game_data$pts_diff,
#         all_game_data$fg_pct_home,
#         all_game_data$fg_pct_away,
#         all_game_data$tov_home,
#         all_game_data$tov_away
#     )

# arg <- data.frame(
#         all_game_data$wl_diff,
#         all_game_data$pts_diff,
#         all_game_data$q1_diff,
#         all_game_data$q2_diff,
#         all_game_data$q3_diff,
#         all_game_data$q4_diff
#     )

# arg <- data.frame(
#         wl_by_game_home,
#         all_game_data$attendance
#     )

print(pcor(
    arg
))

find_stats_corr_coef <- function(coef, data_frame) {
    print("Slope: ") 
        
    print(coef * sd(data_frame[[2]]) / sd(data_frame[[1]]))
        
    print("Mean Point: ")
    
    print(c(
        mean(data_frame[[1]]), mean(data_frame[[2]])
    ))
    return(coef * sd(data_frame[[2]]) / sd(data_frame[[1]]))
}



# print(find_ace(
#     data.frame(
#         binarized_df$home_skill_adv, binarized_df$more_points_q4
#         ,binarized_df$all_game_data.tov_home, binarized_df$all_game_data.tov_away
#         ,binarized_df$all_game_data.pf_home, binarized_df$all_game_data.pf_away
#         ,binarized_df$all_game_data.fg3a_home, binarized_df$all_game_data.fg3a_away
#         ,binarized_df$all_game_data.fg_pct_home, binarized_df$all_game_data.fg_pct_away
#         )))

# print(find_ace(
#     data.frame(binarized_df$more_points_q1, binarized_df$more_points_q4, binarized_df$home_skill_adv)))

# print(find_ace(
#     data.frame(binarized_df$home_skill_adv, binarized_df$home_w, binarized_df
#     binarized_df$all_game_data.tov_home, binarized_df$all_game_data.tov_away, binarized_df$all_game_data.fg_pct_home, binarized_df$all_game_data.fg_pct_away, binarized_df$all_game_data.fg3a_home, binarized_df$all_game_data.fg3a_away)))
# > chisq.test(mod_binarized_df_1$more_points_q1, mod_binarized_df_1$all_game_data$more_points_q2)
