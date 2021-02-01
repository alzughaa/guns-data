# this script is to take notes along the way of writing report Project A

# I'm reading in the data and saving it as guns_data


guns_data <- read.csv("full_data.csv", header = TRUE, sep = ",")

# we're asked to look at 2012 only so I will exclude 2013 and 2014 from my analysis.

guns_12 <- subset(guns_data, guns_data$year == 2012)

# Question.2 --------------------------------------------------------------

# Write a report to analyze the data from 2012. Specifically, we are interested in the proportions of different types of gun deaths, the proportions of each gender type, and the proportion of each race type. Your report should include graphs, descriptive statistics, and accompanying narrative. You can see Instructor Report 1 for an example and watch the accompanying videos for an example of completing this type of analysis. (Your report does not need a press release.)

# Some possible functions to consider: subset, table, prop.table, barplot


# proportions of different types of death

guns_12$intent <- factor(guns_12$intent)


intent.counts <- table(guns_12$intent)
intent.props <- prop.table(intent.counts)

barplot(intent.props, main = "Percentage of Gun Deaths in Types in 2012",
        xlab = "Type of Death",
        ylab = "Percent",
        ylim = c(0,0.75))

intent_table <- round(cbind(intent.counts, intent.props), 2)

# about 35% homicide, 63% suicide, %2 accidental, and about 1% undermined.


# proportions of each gender type

guns_12$sex <- factor(guns_12$sex)

gender_counts <- table(guns_12$sex)
gender_props <- prop.table(gender_counts)

gender_table <- round(cbind(gender_counts, gender_props), 2)


# proportions of each race type

guns_12$race <- factor(guns_12$race)

race_counts <- table(guns_12$race)
race_props <- prop.table(race_counts)

race_table <- round(cbind(race_counts, race_props), 2)


# the relationship between intent and race? 

intent.race_counts <- table(guns_12$intent, guns_12$race)

intent.race_percents <- 100*prop.table(intent.race_counts, 2)

barplot(intent.race_percents, main = "Figure 3. Gunshot Deaths by Race in 2012",
        beside = TRUE,
        xlab = "Race",
        ylab = "Percentage",
        ylim = c(0,110),
        cex.names = .77,
        col = c("green", "red", "blue", "yellow"),
        legend = c("Accedintal", "Homicide", "Suicide", "Undetermined"),
        args.legend = list(title = "Type of Death", cex = .8))


# this is just extra
library(ggplot2)

# Question.7 --------------------------------------------------------------

guns_19 <- read.csv("full_data_19.csv", header = TRUE, sep = ",")

# we're asked to look at 2012 only so I will exclude 2013 and 2014 from my analysis.


# Question.2 --------------------------------------------------------------

# Write a report to analyze the data from 2012. Specifically, we are interested in the proportions of different types of gun deaths, the proportions of each gender type, and the proportion of each race type. Your report should include graphs, descriptive statistics, and accompanying narrative. You can see Instructor Report 1 for an example and watch the accompanying videos for an example of completing this type of analysis. (Your report does not need a press release.)

# Some possible functions to consider: subset, table, prop.table, barplot


# proportions of different types of death

guns_19$intent <- factor(guns_19$intent)


intent.counts.19 <- table(guns_19$intent)
intent.props.19 <- prop.table(intent.counts.19)

barplot(intent.props.19,main = "Figure",
        xlab = "Type of Death",
        ylab = "Percent",
        ylim = c(0,0.70))

intent_table.19 <- round(cbind(intent.counts.19, intent.props.19), 2)

# about 35% homicide, 63% suicide, %2 accidental, and about 1% undermined.


# proportions of each gender type

guns_19$sex <- factor(guns_19$sex)

gender_counts.19 <- table(guns_19$sex)
gender_props.19 <- prop.table(gender_counts.19)

gender_table <- round(cbind(gender_counts.19, gender_props.19), 2)


# proportions of each race type

guns_19$race <- factor(guns_19$race)

race_counts.19 <- table(guns_19$race)
race_props.19 <- prop.table(race_counts.19)

race_table <- round(cbind(race_counts.19, race_props.19), 2)


# the relationship between intent and race? 

intent.race_counts.19 <- table(guns_19$intent, guns_19$race)

intent.race_percents.19 <- 100*prop.table(intent.race_counts.19, 2)

barplot(intent.race_percents.19,beside = TRUE,
        xlab = "Race",
        ylab = "Percent",
        cex.names = .75,
        col = c("green", "red", "blue", "yellow"),
        legend = c("Accedintal", "Homicide", "Suicide", "Undetermined"),
        args.legend = list(title = "Type of Death", cex = .85))

combined <- rbind(guns_12, guns_19)

combined$year <- factor(combined$year)
combined$intent <- factor(combined$intent)
combined$sex <- factor(combined$sex)
combined$race <- factor(combined$race)

combined_intent.counts <- table(combined$intent, combined$race)
combined_intent.perc <- prop.table(combined_intent.counts)


tabulated.data <- table(combined$year, combined$intent, combined$sex, combined$race)


c_sex_year <- table(combined$year, combined$sex)

p_sex_year <- prop.table(c_sex_year, 1)

percent_12_19 <- (cbind(c_sex_year, p_sex_year))


par(mfcol = c(1,2))
box(black)
barplot(intent.race_percents, main = "Figure 3. Gunshot Deaths by Race in 2012",
        beside = TRUE,
        xlab = "Race",
        ylab = "Percentage",
        ylim = c(0,110),
        cex.names = .7,
        col = c("green", "red", "blue", "yellow"),
        legend = c("Accedintal", "Homicide", "Suicide", "Undetermined"),
        args.legend = list(title = "Type of Death", cex = .7))

barplot(intent.race_percents.19,main = "Figure 4. Gunshot Deaths by Race in 2019",
        beside = TRUE,
        xlab = "Race",
        ylab = "Percentage",
        ylim = c(0,110),
        cex.names = .7,
        col = c("green", "red", "blue", "yellow"),
        legend = c("Accedintal", "Homicide", "Suicide", "Undetermined"),
        args.legend = list(title = "Type of Death", cex = .7))




xtabs(~intent + race + sex, data = guns_12 )

ftable(sex + intent ~ race, data = guns_12)

barplot(intent.race_counts.19, intent_12_counts)










acc_diff <- 486-548

hom_diff <- 14909-12093

sui_diff <- 23941-20666

und_diff <- 346-256

difference <- cbind(acc_diff, hom_diff, sui_diff, und_diff)

print(difference)


print(male_diff)
print(female_diff)


# to make comparison simpler, I will create an object called all_guns that will bring 2012 and 2019 data together.

all_guns <- rbind(guns_12, guns_19)

# I will label death_year to indicate the object that counts types of death by year 


death_year <- table(all_guns$year)

# gender by year 

gender_year <- table(all_guns$sex, all_guns$year)

gender_prop_year <- 100*prop.table(gender_year, 2)

print(gender_prop_year)

# intent by year

intent_year <- table(guns_12_19$intent, guns_12_19$year)

print(intent_year)

intent_year_prop <- 100*prop.table(intent_comp, 2)

intent_comp_props

print(intent_comp_props)

# intent by race

intent_by_race <- table(all_guns$intent, all_guns$race, all_guns$year)
print(intent_by_race)

intent_by_race_props <- 100*prop.table(intent_by_race)

par(mfcol = c(1,2))
barplot(intent.race_percents, main = "Figure 3. Gunshot Deaths by Race in 2012",
        xlab = "Race",
        ylab = "Percentage",
        ylim = c(0,120),
        cex.names = .5,
        col = c("green", "red", "blue", "yellow"),
        legend = c("Accedintal", "Homicide", "Suicide", "Undetermined"),
        args.legend = list(title = "Type of Death", cex = .65))
barplot(intent.race_percents.19, main = "Figure 3. Gunshot Deaths by Race in 2019",
        xlab = "Race",
        ylab = "Percentage",
        ylim = c(0,120),
        cex.names = .5,
        col = c("green", "red", "blue", "yellow"),
        legend = c("Accedintal", "Homicide", "Suicide", "Undetermined"),
        args.legend = list(title = "Type of Death", cex = .65))

# differences between 2012 and 2019

death_diff <- 39707-33563



