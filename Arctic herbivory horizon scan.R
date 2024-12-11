#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         Arctic herbivory horizon scan
#        Isabel C Barrio (isabel@lbhi.is)
#                  3-December-2024
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# script to import the data from the two scoring surveys 
# and link them to the list of questions

# libraries --------------------------------------------------------------------
library(readxl)
library(writexl)
library(dplyr)
library(tidyverse)
library(stringr)
library(ggpubr)
library(scales)



# customised functions ----
# defining the parameters for our graphs
theme_horizon <- function(){    # create a new theme function for the style of graphs
  theme_bw()+                   # use a predefined theme as a base
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
        plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
        legend.text = element_text(size = 12, face = "italic"),
        legend.title = element_blank(),
        legend.position = c(0.15, 0.85))  # legend inside plot area x, y position
}


# 1. load datasets -------------------------------------------------------------

# set the working directory
# ICB: setwd("C:/Users/isabel/OneDrive - Menntaský/ISABEL/HERBIVORY NETWORK/herbivory horizon scan/R")

# 2. demographic data --------------------------------------------------------
## 2.1 elicitation survey ----
## load demographic data of participants in the elicitation survey
demog <- read_excel("Arctic Herbivory Horizon Scan_data.xlsx", sheet = "responses") %>% 
                select(resp_ID, career_stage, career_stage_other, 
                       geographic_scope, geographic_scope_other, gender) %>% 
                # fix career stage
                mutate(career_stage_other = case_when(career_stage_other %in% c("Lecturer in ecology with over a decade of experience",
                                                            "Environmental data scientist",
                                                            "Out dated and retired",
                                                            "professor (I guess this would go under senior researcher?)") ~ "Senior researcher (5 or more years from obtaining a research position)",
                                   TRUE ~ career_stage_other),
                       career_stage = case_when(career_stage == "Other" ~ career_stage_other,
                                                TRUE ~ career_stage)) %>% 
                # simplify classes and reorder
                mutate(career_stage = case_when(career_stage == "Post-doctoral fellow" ~ "postdoc",
                                                career_stage == "Senior researcher (5 or more years from obtaining a research position)" ~ "senior researcher",
                                                career_stage == "Recently established researcher (<5 years from obtaining a position)" ~ "young researcher",
                                                TRUE ~ career_stage),
                       career_stage = factor(career_stage, levels = c("BSc student", "PhD student", "postdoc",
                                                                      "young researcher", "senior researcher")),
                       gender = case_when(gender == "Male" ~ "male",
                                          gender == "Female" ~ "female",
                                          gender == "Non-binary" ~ "non-binary")) %>% 
                # fix geographic scope
                mutate(geographic_scope_other = case_when(geographic_scope_other %in% c("Svalbard",
                                                            "Svalbard Islands", "Norwegian high Arctic Svalbard") ~ "Svalbard",
                                                          geographic_scope_other == "Global (i.e. pan-Arctic plus the rest of the world)" ~ "pan-Arctic",
                                                          TRUE ~ "outside the Arctic")) 
  

### 2.1.1 career stage ----
career.stage <- demog %>% count(career_stage) 

# pie chart
# first compute the position of labels for piechart
#percentages of each type of document
data.frame(document_type = career.stage$career_stage,
           percentage = round(career.stage$n/sum(career.stage$n)*100,1))

data <- career.stage %>% 
          arrange(desc(career_stage)) %>%
          mutate(prop = round(n / sum(career.stage$n) *100, 2)) %>%
          mutate(ypos = cumsum(prop) - 0.5*prop)

# basic piechart
ggplot(data, aes(x = "", y = prop, fill = career_stage)) +
   geom_bar(stat = "identity", width = 1, color = "white") +
   coord_polar("y", start = 0) +
   theme_void() + 
     geom_text(aes(y = ypos, label = prop), color = "white", size = 6) +
     scale_fill_brewer(palette = "Set1")

# barplot
career_p <- ggplot(career.stage, aes(career_stage, n)) +
  geom_bar(stat="identity", fill = "#00688B") +
  xlab("career stage") +
  ylab("participants") +
  coord_flip() +
  theme_horizon()


### 2.1.2 gender ----
gender <- demog %>% count(gender) 

# barplot
gender_p <- ggplot(gender, aes(gender, n)) +
  geom_bar(stat="identity", fill = "#00688B") +
  xlab("gender") +
  ylab("participants") +
  coord_flip() +
  theme_horizon()


### 2.1.3 geographic scope ----
geographic.scope <- demog %>% 
                      separate_rows(geographic_scope, sep = ";") %>% 
                        # remove empty rows created because of the last ";"
                        filter(!geographic_scope == "") %>% 
                      mutate(geographic_scope = case_when(geographic_scope %in% c("Other", NA) ~ geographic_scope_other,
                                                TRUE ~ geographic_scope),
                             # order
                             geographic_scope = factor(geographic_scope, 
                                                        levels = c("Alaska", "Canada", "Greenland", "Iceland",
                                                                   "Svalbard", "Fennoscandia", "Russia", "pan-Arctic", "outside the Arctic"))) %>% 
                               count(geographic_scope)

# barplot
geographic_p <- ggplot(geographic.scope, aes(geographic_scope, n)) +
  geom_bar(stat="identity", fill = "#00688B") +
  xlab("geographic scope") +
  ylab("participants") +
  coord_flip() +
  theme_horizon()

# plot everything together
figS1 <- ggarrange(career_p + ggtitle("a. Career stage"),
                   gender_p + ggtitle("b. Gender"),
                   geographic_p+ ggtitle("c. Geographic scope"),
                   ncol = 3, nrow = 1)


## 2.2 co-author list ----
## load demographic data of co-authors
coauthors <- read_excel("co-author_info.xlsx", sheet = "responses") %>%
                # fix career stage
                mutate(career_stage = case_when(career_stage %in% c("Recently established researcher (<5 years from obtaining a position)",
                                                            "Recently established researcher (<5 years in a research or management position since receiving the highest educational degree)",
                                                            "Post-doctoral fellow", "Teaching associate (similar in stage to post-doc)") ~ "young researcher",
                                                career_stage %in% c("Senior researcher (5 or more years in a research or management positionsince receiving the highest educational degree)",
                                                                    "Professor", "Research Professor", "Senior Research Associate, so soft money but senior?",
                                                                    "emeritus professor") ~ "senior researcher",
                                                TRUE ~ career_stage)) %>% 
                # fix focus into two categories (nobody said management alone)
                mutate(focus = case_when(focus %in% c("Mainly research", "Research and administrative",
                                                      "Teaching", "Research and teaching") ~ "research",
                                        TRUE ~ "both research and management")) %>% 
                # fix country of residence
                mutate(country_residence = case_when(country_residence %in% c("USA", "United States/Sweden") ~ "United States",
                                            country_residence %in% c("Norway/Bulgaria") ~ "Norway",
                                            country_residence %in% c("UK", "Scotland") ~ "United Kingdom",
                                            country_residence %in% c("The Netherlands") ~ "Netherlands",
                                            country_residence %in% c("sweden") ~ "Sweden",
                                            TRUE ~ country_residence)) %>%
                # simplify classes and reorder
                mutate(career_stage = factor(career_stage, levels = c("MSc student", "PhD student",
                                                                      "young researcher", "senior researcher")))   

### 2.2.1 career stage ----
career.stage <- coauthors %>% count(career_stage) %>% 
                  mutate(percentage = round(n/sum(n)*100,1))

### 2.2.2 gender ----
gender <- coauthors %>% count(gender) %>% 
                  mutate(percentage = round(n/sum(n)*100,1))

### 2.2.3 focus ----
focus <- coauthors %>% count(focus) %>% 
                  mutate(percentage = round(n/sum(n)*100,1))

### 2.2.4 country of residence ----
country_residence <- coauthors %>% count(country_residence) %>% 
                  mutate(percentage = round(n/sum(n)*100, 1))




# 3. list of questions -------------------------------------------------------
# NEED TO LOAD THESE for analysing survey data in the next section :)

# load questions for each survey separately
questions_A <- read_excel("Arctic Herbivory Horizon Scan_data.xlsx", sheet = "questions_to_rank") %>% 
                filter(survey == "A") %>% 
                # create variables to merge
                mutate(question_long = question,
                       question_nr = as.character(random)) %>% 
                select(question_nr, question_long) %>% 
                  mutate(question = paste(question_nr,"A", sep = ""))
questions_B <- read_excel("Arctic Herbivory Horizon Scan_data.xlsx", sheet = "questions_to_rank") %>% 
                filter(survey == "B") %>% 
                # create variables to merge
                mutate(question_long = question,
                       question_nr = as.character(random)) %>% 
                select(question_nr, question_long) %>% 
                  mutate(question = paste(question_nr,"B", sep = ""))

question_list <- rbind(questions_A, questions_B)


# 4. survey data -------------------------------------------------------------
# compile scoring of responses from the two scoring surveys
# and calculate average scores
survey_A <- read_excel("Arctic herbivory horizon scan - step 2 (A)(1-32).xlsx") %>% 
                # remove the columns we don't need
                select(-"Start time", -"Completion time", 
                       -"Email", -"Name", -"Last modified time") %>% 
                pivot_longer(cols = -ID, names_to = c("criterion", "question"),
                             names_sep = " relevance",
                             values_to = "score") %>% 
                mutate(question_nr = case_when(question == "" ~ "1",
                                            TRUE ~ question),
                       score_num = case_when(score == "Not at all" ~ 0,
                                             score == "Little relevant" ~ 1,
                                             score == "Relevant" ~ 2,
                                             score == "Very relevant" ~ 3)) %>% 
               left_join(questions_B, by = "question_nr") %>% 
               mutate(survey = "A", 
                      question = paste(question_nr,"A", sep = ""), 
                      respondent = paste(ID,"A", sep = ""))

survey_B <- read_excel("Arctic herbivory horizon scan - step 2 (B)(1-31).xlsx") %>% 
                # remove the columns we don't need
                select(-"Start time", -"Completion time", 
                       -"Email", -"Name", -"Last modified time") %>% 
                pivot_longer(cols = -ID, names_to = c("criterion", "question"),
                             names_sep = " relevance",
                             values_to = "score") %>% 
                mutate(question_nr = case_when(question == "" ~ "1",
                                            TRUE ~ question),
                       score_num = case_when(score == "Not at all" ~ 0,
                                             score == "Little relevant" ~ 1,
                                             score == "Relevant" ~ 2,
                                             score == "Very relevant" ~ 3)) %>% 
               left_join(questions_B, by = "question_nr") %>% 
               mutate(survey = "B", 
                      question = paste(question_nr,"B", sep = ""), 
                      respondent = paste(ID,"B", sep = ""))

question_scores <- rbind(survey_A, survey_B) %>% 
                    drop_na(score_num) %>%  # drop questions with no scores
                    group_by(question, criterion) %>% 
                      summarize(score = mean(score_num), 
                                median = median(score_num),
                                cv = sd(score_num)/score, 
                                n = n()) %>% 
                    mutate(criterion = factor(criterion, 
                                              levels = c("Scientific", "Management")))

min(question_scores$n); max(question_scores$n)

# mean scores for scientific and management  
question_scores %>% group_by(criterion) %>% summarize(mean = mean(score),
                                                      median = median(score))


# density plot for the two criteria (Supplementary materials)
ggplot(data = question_scores, aes(x = score, group = criterion, fill = criterion)) +
    geom_density(adjust =1.5, alpha = 0.4) +
      scale_fill_manual(values = c("#A2CD5A", "#00688B")) +
    # add vertical lines for top 25%
    geom_vline(xintercept = 2.34, colour = "#A2CD5A", lty = "solid", linewidth = 1) +
    geom_vline(xintercept = 2.15, colour = "#00688B", lty = "solid", linewidth = 1) +
    # add vertical lines for means
    geom_vline(xintercept = 2.13, colour = "#A2CD5A", lty = "dashed", linewidth = 1) +
    geom_vline(xintercept = 1.84, colour = "#00688B", lty = "dashed", linewidth = 1) +
    theme_horizon()

## Figure 2----
# scatterplot of scientific and management scores for each response
criteria <- question_scores %>% select(-cv, -n, -median) %>% 
              pivot_wider(names_from = "criterion", values_from = "score") %>% 
              mutate(priority = case_when(Scientific > 2.34 & Management < 2.15 ~ "Scientific",
                                          Scientific < 2.34 & Management > 2.15 ~ "Management",
                                          Scientific > 2.34 & Management > 2.15 ~ "Both",
                                          TRUE ~ "None")) %>% 
              mutate(priority = factor(priority, 
                                 levels = c("Scientific", "Management", "Both", "None")))

## need to prepare the axes as gradients
gradient_x <- data.frame(axes = c("xaxis"),
                         start.x = c(0),
                         end.x = c(3), 
                         start.y = c(-0.1),
                         end.y = c(0))
gradient_y <- data.frame(axes = c("yaxis"),
                         start.x = c(-0.1),
                         end.x = c(0), 
                         start.y = c(0),
                         end.y = c(3))

  steps <- seq(from = 0, to = 3, length.out = 100 + 1)
  alpha_steps <- seq(from = 0, to = 1, length.out = 100)
  
  rect_grad_x <- data.frame(xmin = steps[-(100 + 1)], 
                            xmax = steps[-1], 
                            alpha = alpha_steps)
      rect_x <- merge(gradient_x, rect_grad_x)
  
  rect_grad_y <- data.frame(ymin = steps[-(100 + 1)], 
                            ymax = steps[-1], 
                            alpha = alpha_steps)
      rect_y <- merge(gradient_y, rect_grad_y)

fig2 <- ggplot(criteria) +
          geom_point(aes(x = Management, y = Scientific, shape = priority, color = priority), 
                       alpha = 0.8, size = 3) +
              scale_color_manual(values = c("#A2CD5A", "#00688B", "#FF7F00", "#BABABA")) +
              scale_shape_manual(values = c(15, 17, 18, 19)) +
          scale_x_continuous(name = "Management relevance", limits = c(-0.1, 3),
                             breaks = c(0, 1, 2, 3), 
                             expand = c(0, 0)) +
          scale_y_continuous(name = "Scientific relevance", limits = c(-0.1, 3),
                             breaks = c(0, 1, 2, 3),
                             expand = c(0, 0)) +
            # add dashed vertical line for average management relevance score 
            geom_vline(xintercept = 1.84, colour = "#BABABA", lty = "dashed", linewidth = 0.5) +
            # add dashed horizontal line for average scientific relevance score 
            geom_hline(yintercept = 2.13, colour = "#BABABA", lty = "dashed", linewidth = 0.5) +
          geom_rect(data = rect_x, aes(xmin = xmin, xmax = xmax, 
                                       ymin = start.y, ymax = end.y, fill = xmin)) +
          geom_rect(data = rect_y, aes(xmin = start.x, xmax = end.x, 
                                       ymin = ymin, ymax = ymax, fill = ymin)) +
          guides(fill = FALSE) +
          scale_fill_gradient(low = "white", high = "#104E8B") +
          annotate("text", x = c(0.1, 2.95, -0.05, -0.05), 
                           y = c(-0.05, -0.05, 0.1, 2.95), 
                           label = c("not relevant", "very relevant", 
                                     "not relevant", "very relevant"), 
                           color = c("black", "white", "black", "white"), 
                           size = 4 , angle = c(0, 0, 90, 90), 
                           fontface = "bold", 
                           hjust = c("left", "right", "left", "right"),
                           vjust = c("middle", "middle", "middle", "middle")) +
          theme_horizon() 


# how many observations that are very relevant to management below average scientific relevance?
criteria %>% filter(priority == "management") %>% 
            group_by(priority) %>% summarize(n = n()) # 34 responses with high management relevance
criteria %>% filter(priority == "management"& Scientific < 2.13) %>% 
            group_by(priority) %>% summarize(n = n()) # 19 responses out of 34 were below average scientific score
(19/34)*100

# how many observations that are very relevant to science below average management relevance?
criteria %>% filter(priority == "scientific") %>% 
            group_by(priority) %>% summarize(n = n()) # 34 responses with high management relevance
criteria %>% filter(priority == "scientific"& Management < 1.84) %>% 
            group_by(priority) %>% summarize(n = n()) # 21 responses out of 38 were below average scientific score
(21/38)*100

# plot the scores for each question, separate graphs for each criterion
question_scores_all <- question_scores %>% select(-n, -cv, -median) %>% 
                        pivot_wider(names_from = criterion,
                                    values_from = score) %>% 
                       left_join(question_list, by = "question") %>% 
                         select(-question_nr)

question_scores_all_cv <- question_scores %>% select(-n, -score, -median) %>% 
                        pivot_wider(names_from = criterion,
                                    values_from = cv) %>% 
                       left_join(question_list, by = "question") %>% 
                         select(-question_nr) %>% 
                        rename(Management_cv = Management,
                               Scientific_cv = Scientific)

question_scores_all_med <- question_scores %>% select(-n, -score, -cv) %>% 
                        pivot_wider(names_from = criterion,
                                    values_from = median) %>% 
                       left_join(question_list, by = "question") %>% 
                         select(-question_nr) %>% 
                        rename(Management_med = Management,
                               Scientific_med = Scientific)

question_scores_all <- question_scores_all %>% 
                        left_join(question_scores_all_cv, by = "question") %>% 
                        select(-question_long.y) %>% 
                        rename(question_long = question_long.x) %>% 
                        left_join(question_scores_all_med, by = "question") %>% 
                        select(-question_long.y) %>% 
                        rename(question_long = question_long.x)

write_xlsx(question_scores_all, "question_scores.xlsx")


# SCIENTIFIC
ggplot(question_scores_all,
       aes(x = reorder(question, Scientific), y = Scientific)) +
          geom_bar(stat = "identity") +
          labs(x = "", y = "score") +
          coord_flip() 
quantile(question_scores_all$Scientific, 0.75) # questions with scientific score > 2.34 should be kept


# MANAGEMENT
ggplot(question_scores_all,
       aes(x = reorder(question, Management), y = Management)) +
          geom_bar(stat = "identity") +
          labs(x = "", y = "score") +
          coord_flip() 
quantile(question_scores_all$Management, 0.75) # questions with management score > 2.15 should be kept



# 5. broad priorities ----------------------------------------------------------
# some data on the grouping into broad priorities
sci <- read_excel("Supplementary_material_S2_full_list_questions.xlsx", 
                  sheet = "scientific_all") %>% 
        # fix issues with comma delimited decimals
        mutate(score = scientific/1000,  
               criterion = "scientific") %>% 
        filter(score > 2.34) %>% # keep only top 25%
        select(-scientific)
man <- read_excel("Supplementary_material_S2_full_list_questions.xlsx", 
                  sheet = "management_all") %>% 
        # fix issues with comma delimited decimals
        mutate(score = management,  
               criterion = "management") %>% 
        filter(score > 2.15) %>% # keep only top 25%
        select(-management)

priorities <- rbind(sci, man) %>% 
                group_by(criterion, new_broad_question) %>% 
                summarize(n = n(), 
                          avg.score = mean(score),
                          max.score = round(max(score), 2)) %>% 
                arrange(desc(criterion)) %>% 
                ungroup() %>% 
                mutate(order = seq(1:16))

##### Figure 4----
fig4 <- ggplot(priorities) +
          geom_bar(aes(x = avg.score, y = reorder(new_broad_question, order, decreasing = T), 
                       fill = criterion), stat = "identity", width = 0.8) +
          scale_fill_manual(values = c("#00688B", "#A2CD5A")) +
          scale_y_discrete(labels = label_wrap(70)) + 
          scale_x_continuous(name = "Average score", limits = c(-0.1, 3),
                     breaks = c(0, 1, 2, 3), 
                     expand = c(0, 0)) +
          labs(y = " ", x = "Average score") +
          geom_text(aes(x = avg.score + 0.2, 
                        y = reorder(new_broad_question, order, decreasing = T),
                        label = paste0(n, " [", max.score, "]")), size = 3) +
          theme_horizon() +
            theme(axis.text.y = element_text(size = 9),
                  legend.position = "none") 


