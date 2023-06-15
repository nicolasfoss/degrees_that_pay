
# Load relevant packages
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(cluster)
library(factoextra)

# Read in the dataset
degrees <- read_csv("datasets/degrees-that-pay-back.csv", 
                    col_names=c("College.Major", 
                                "Starting.Median.Salary",
                               "Mid.Career.Median.Salary",
                                "Career.Percent.Growth",
                                "Percentile.10",
                                "Percentile.25",
                                "Percentile.75",
                                "Percentile.90"
                               ), 
                    skip=1)

# Display the first few rows and a summary of the data frame
head(degrees)
summary(degrees)

# These packages need to be loaded in the first @tests cell. 
library(testthat) 
library(IRkernel.testthat)

sol_degrees <- read_csv('datasets/degrees-that-pay-back.csv', col_names=c('College.Major',
                    'Starting.Median.Salary','Mid.Career.Median.Salary','Career.Percent.Growth',
                    'Percentile.10','Percentile.25','Percentile.75','Percentile.90'), skip=1)

run_tests({
    test_that("packages are loaded", {
        expect_true("tidyr" %in% .packages(), info = "Did you load the tidyverse package?")
        expect_true("dplyr" %in% .packages(), info = "Did you load the tidyverse package?")
        expect_true("readr" %in% .packages(), info = "Did you load the tidyverse package?")
        expect_true("ggplot2" %in% .packages(), info = "Did you load the tidyverse package?")
        expect_true("cluster" %in% .packages(), info = "Did you load the cluster package?")
        expect_true("factoextra" %in% .packages(), info = "Did you load the factoextra package?")
    })
    test_that("columns renamed correctly", {
        expect_identical(colnames(sol_degrees), colnames(degrees), 
                info = "Did you rename the columns as specified? Is there a typo?")
    })
    test_that("degrees data loaded correctly", {
        expect_is(degrees, "tbl_df", info = "Did you read in degrees-that-pay-back with read_csv?")
        expect_equal(degrees, sol_degrees, info = "degrees contains the wrong values. Did you import the correct .csv file?")
    })
})


# Clean up the data
degrees_clean <- degrees %>% 
  mutate_at(vars(Starting.Median.Salary:Percentile.90), function (x) as.numeric(gsub("[\\$,]","",x))) %>%
  mutate(Career.Percent.Growth = Career.Percent.Growth / 100)

sol_degrees_clean <- sol_degrees %>% 
    mutate_at(vars(Starting.Median.Salary:Percentile.90), 
              function(x) as.numeric(gsub("[\\$,]","",x))) %>%
    mutate(Career.Percent.Growth = Career.Percent.Growth/100)
              
run_tests({
    test_that("data types converted to numeric where applicable", {
        expect_identical(sol_degrees_clean$Starting.Median.Salary, degrees_clean$Starting.Median.Salary, 
                info = "Did you strip the $ signs and convert to numeric?")   
    })
    
     test_that("Career.Percent.Growth converted to decimal", {
        expect_identical(sol_degrees_clean$Career.Percent.Growth, degrees_clean$Career.Percent.Growth, 
                info = "Did you divide `Career.Percent.Growth` by  `100`?")   
    })
})
    

# Select and scale the relevant features and store as k_means_data
k_means_data <- degrees_clean %>%
select(Starting.Median.Salary, Mid.Career.Median.Salary, Percentile.10, Percentile.90) %>% 
scale()

# Run the fviz_nbclust function with our selected data and method "wss"
elbow_method <- fviz_nbclust(k_means_data, FUNcluster = kmeans, method = "wss")


# View the plot
elbow_method

sol_k_means_data <- sol_degrees_clean %>%
    select(Starting.Median.Salary, Mid.Career.Median.Salary, 
           Percentile.10, Percentile.90) %>%
    scale()

sol_elbow <- fviz_nbclust(sol_k_means_data, kmeans, method = "wss")

run_tests({
    test_that("selected and scaled columns for k_means_data", {
        expect_identical(sol_k_means_data,k_means_data, 
                         info = "Did you select the correct columns and scale them for k_means_data?")
    })
    test_that("wss method applied correctly", {
        expect_identical(sol_elbow$data, elbow_method$data, 
                         info = "Something isn't right in fviz_nbclust... did you use the wss method?")
    })
})

# Run the fviz_nbclust function with the method "silhouette" 
silhouette_method <- fviz_nbclust(k_means_data, FUNcluster = kmeans, method = "silhouette")

# View the plot
silhouette_method

sol_silhouette <- fviz_nbclust(sol_k_means_data, kmeans, method = "silhouette")

run_tests({
    test_that("silhouette method applied correctly", {
        expect_identical(sol_silhouette$data, silhouette_method$data, 
                         info = "Something isn't right in fviz_nbclust... did you use the silhouette method?")
    })
})

# Use the clusGap function to apply the Gap Statistic Method
gap_stat <- clusGap(k_means_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# Use the fviz_gap_stat function to vizualize the results
gap_stat_method <- fviz_gap_stat(gap_stat)

# View the plot
gap_stat_method

sol_gap_stat <- clusGap(sol_k_means_data, FUN = kmeans, nstart = 25, 
                    K.max = 10, B = 50)

sol_gap_stat_method <- fviz_gap_stat(sol_gap_stat)

run_tests({
    test_that("clusGap applied correctly", {
        expect_identical(sol_gap_stat$gap, gap_stat$gap, 
                         info = "Something is wrong in gap_stat... did you apply the correct parameters to the clusGap() function?")
    })
    test_that("gap_stat graphed correctly", {
        expect_identical(gap_stat_method$data, sol_gap_stat_method$data, 
                         info = "Something is wrong in gap_stat_method... did you graph gap_stat with the fviz_gap_stat function?")
    })
})

# Set a random seed
set.seed(111)

# Set k equal to the optimal number of clusters
num_clusters <- 3

# Run the k-means algorithm 
k_means <- kmeans(k_means_data, 
                  centers = num_clusters, 
                  iter.max = 15,
                 nstart = 25
                 )

# Label the clusters of degrees_clean
degrees_labeled <- degrees_clean %>%
    mutate(clusters = k_means$cluster)

set.seed(111)
sol_k_means <- kmeans(sol_k_means_data, 3, iter.max = 15, nstart = 25)

# Add back the cluster labels to degrees
sol_degrees_labeled <- sol_degrees_clean %>%
    mutate(clusters = sol_k_means$cluster)

run_tests({
    test_that("correct number of clusters", {
        expect_true(num_clusters == 3, 
                    info = "Did you set num_clusters to 3?")
    })
    test_that("k_means applied correctly", {
        expect_identical(sol_k_means, k_means, 
                         info = "Did you use the correct parameters in the kmeans function? Did you set the seed to 111?")
    })
    test_that("clusters columm added to degrees", {
        expect_identical(sol_degrees_labeled$clusters,degrees_labeled$clusters, 
                         info = "Did you add a clusters column to degrees?")
    })
})

# Graph the clusters by Starting and Mid Career Median Salaries
career_growth <- ggplot(degrees_labeled, 
                       aes(x = Starting.Median.Salary, 
                          y = Mid.Career.Median.Salary,
                           color = factor(clusters)
                          )
                       ) +
geom_point() + 
xlab("Starting Median Salary") + 
ylab("Mid-Career Median Salaray") +
ggtitle("Mid-Career Median Salary as a function of Starting Median Salary\nColored by K-means clusters") + 
scale_x_continuous(labels = scales::dollar) + 
scale_y_continuous(labels = scales::dollar) + 
theme_linedraw()

# View the plot
career_growth

sol_career_growth <- ggplot(sol_degrees_labeled, aes(x=Starting.Median.Salary,y=Mid.Career.Median.Salary,
    color=factor(clusters))) +
    geom_point(alpha=4/5,size=6) +
    scale_x_continuous(labels = scales::dollar) +
    scale_y_continuous(labels = scales::dollar) +
    xlab('Starting Median Salary') +
    ylab('Mid Career Median Salary') +
    scale_color_manual(name="Clusters",values=c("#EC2C73","#29AEC7", 
                    "#FFDD30")) +
    ggtitle('Clusters by Starting vs. Mid Career Median Salaries')
run_tests({
    test_that("plot is drawn correctly", {
        expect_s3_class(career_growth,"ggplot")
        expect_identical(sol_career_growth$data, career_growth$data, 
                         info = "The plot data is incorrect. Did you use degrees?")
    })
    test_that("plot mapping is correct", {
        expect_equal(sol_career_growth$mapping$x, career_growth$mapping$x, 
                         info = "The 'x' axis is incorrect. Did you map it to Starting.Median.Salary?")
        expect_equal(sol_career_growth$mapping$y, career_growth$mapping$y,
                         info = "The 'y' axis is incorrect. Did you map it to Mid.Career.Median.Salary?")
        expect_equal(sol_career_growth$mapping$colour, career_growth$mapping$colour,
                         info = "The color is not correct. Did you map it to factor(clusters)?")
    })
    test_that("geom_point used with correct parameters", {
        expect_identical(class(sol_career_growth$layers[[1]]$geom)[1],class(career_growth$layers[[1]]$geom)[1],
                         info = "There is no point layer. Did you use geom_point()?")
    })
    test_that("axis labels scaled as dollar format", {
        expect_identical(ggplot_build(sol_career_growth)$layout$panel_params[[1]]$x.labels, ggplot_build(career_growth)$layout$panel_params[[1]]$x.labels,
                         info = "The x axis labels aren't formatted correctly. Did you use scale_x_continuous()?")
        expect_identical(ggplot_build(sol_career_growth)$layout$panel_params[[1]]$y.labels, ggplot_build(career_growth)$layout$panel_params[[1]]$y.labels,
                         info = "The y axis labels aren't formatted correctly. Did you use scale_y_continuous()?")
    })   
})

# Use the gather function to reshape degrees and 
# use mutate() to reorder the new percentile column
degrees_perc <- degrees_labeled %>%
  select(College.Major, Percentile.10, Percentile.25, Mid.Career.Median.Salary, Percentile.75, Percentile.90, clusters) %>% 
  gather(key = "percentile", value = "salary", -c(College.Major, clusters)) %>%
  mutate(percentile = factor(percentile, levels = c("Percentile.10", "Percentile.25", "Mid.Career.Median.Salary", "Percentile.75", "Percentile.90")))

sol_degrees_perc <- sol_degrees_labeled %>%
    select(College.Major, Percentile.10, Percentile.25, 
           Mid.Career.Median.Salary, Percentile.75, 
           Percentile.90, clusters) %>%
    gather(key=percentile, value=salary, -c(College.Major, clusters)) %>%
    mutate(percentile=factor(percentile,levels=c('Percentile.10','Percentile.25',
            'Mid.Career.Median.Salary','Percentile.75','Percentile.90')))

run_tests({
    test_that("selected and labeled correct columns in gather for degrees_perc", {
    expect_identical(colnames(sol_degrees_perc), colnames(degrees_perc), 
        info = "degrees_perc is not correct. Did you select the correct columns and use gather() to set the key to `percentile` and value to `salary`?")
    expect_identical(sol_degrees_perc$salary, degrees_perc$salary,
        info = "Did you select the correct columns for the gather function?")
    })
    test_that("percentile column reordered correctly", {
    expect_identical(levels(sol_degrees_perc$percentile), levels(degrees_perc$percentile),
        info = "Did you correctly set the order of the factor levels for the percentile column?")
    })
})

# Graph the majors of Cluster 1 by percentile
cluster_1 <- degrees_perc %>% 
  filter(clusters == 1) %>% 
  ggplot(aes(percentile, salary, 
             group = College.Major, 
             color = College.Major
  )) + 
  geom_point(alpha = 0.6) + 
  geom_line(alpha = 0.4) + 
  ggtitle("Cluster 1: The Liberal Arts") + 
labs(x = "Percentile",
    y = "Salary"
    ) + 
theme(axis.text.x = element_text(size = 7, angle = 25))

# View the plot
cluster_1

sol_cluster_1 <- ggplot(sol_degrees_perc[sol_degrees_perc$clusters==1,], aes(x=percentile,y=salary, 
                    group=College.Major, color=College.Major, order=salary)) +
                    geom_point() +
                    geom_line() +
                    ggtitle('Cluster 1:  The Liberal Arts') +
                    theme(axis.text.x = element_text(size=7, angle=25)) 

run_tests({
    test_that("plot is drawn correctly", {
        expect_s3_class(cluster_1,"ggplot")
        expect_equal(sol_cluster_1$data, cluster_1$data, 
                     info = "The plot data is incorrect. Did you filter degrees_perc where the cluster column equals 1?")
    })
    test_that("plot mapping is correct", {
        expect_equal(sol_cluster_1$mapping$x, cluster_1$mapping$x, 
                     info = "The 'x' axis is incorrect. Did you map it to percentile?")
        expect_equal(sol_cluster_1$mapping$y, cluster_1$mapping$y,
                     info = "The 'y' axis is incorrect. Did you map it to salary?")
        expect_equal(sol_cluster_1$mapping$colour, cluster_1$mapping$colour,
                     info = "The color is not correct. Did you map it to College.Major?")
        expect_equal(sol_cluster_1$mapping$group, cluster_1$mapping$group,
                     info = "The group is not correct. Did you map it to College.Major?")
    })
    test_that("geom_point and geom_line applied", {
        expect_identical(class(sol_cluster_1$layers[[1]]$geom)[1],class(cluster_1$layers[[1]]$geom)[1],
                         info = "geoms are not correct. Did you use geom_point() before geom_line()?")
        expect_identical(class(sol_cluster_1$layers[[2]]$geom)[1],class(cluster_1$layers[[2]]$geom)[1],
                         info = "geoms are not correct. Did you use geom_line() after geom_point()?")
    })
    test_that("tick labels customized correctly", {
        expect_identical(sol_cluster_1$theme$axis.text.x$size, cluster_1$theme$axis.text.x$size,
                         info = "The x axis text size is not correct. Did you set `size` to 7?")
        expect_identical(sol_cluster_1$theme$axis.text.x$angle, cluster_1$theme$axis.text.x$angle,
                         info = "The x axis text angle is not correct. Did you set `angle` to 25?")
    })   
})

# Modify the previous plot to display Cluster 2
cluster_2 <- degrees_perc %>% 
filter(clusters == 2) %>% 
  ggplot(aes(percentile, salary, 
             group = College.Major, 
             color = College.Major
  )) + 
  geom_point(alpha = 0.6) + 
  geom_line(alpha = 0.4) + 
ggtitle("Cluster 2: The Goldilocks") +  
labs(x = "Percentile",
    y = "Salary"
    ) + 
theme(axis.text.x = element_text(size = 7, angle = 25))

# View the plot
cluster_2

sol_cluster_2 <- ggplot(sol_degrees_perc[sol_degrees_perc$clusters==2,], aes(x=percentile,y=salary, 
                    group=College.Major, color=College.Major, order=salary)) +
                    geom_point() +
                    geom_line() +
                    ggtitle('Cluster 2:  The Goldilocks') +
                    theme(axis.text.x = element_text(size=7, angle=25)) 

run_tests({
    test_that("plot is drawn correctly", {
        expect_s3_class(cluster_2,"ggplot")
        expect_equal(sol_cluster_2$data, cluster_2$data, 
                     info = "The plot data is incorrect. Did you filter degrees_perc where the cluster column equals 2?")
    })
    test_that("plot mapping is correct", {
        expect_equal(sol_cluster_2$mapping$x, cluster_2$mapping$x, 
                     info = "The 'x' axis is incorrect. Did you map it to percentile?")
        expect_equal(sol_cluster_2$mapping$y, cluster_2$mapping$y,
                     info = "The 'y' axis is incorrect. Did you map it to salary?")
        expect_equal(sol_cluster_2$mapping$colour, cluster_2$mapping$colour,
                     info = "The color is not correct. Did you map it to College.Major?")
        expect_equal(sol_cluster_2$mapping$group, cluster_2$mapping$group,
                     info = "The group is not correct. Did you map it to College.Major?")
    })
    test_that("geom_point and geom_line applied", {
        expect_identical(class(sol_cluster_2$layers[[1]]$geom)[1],class(cluster_2$layers[[1]]$geom)[1],
                         info = "geoms are not correct. Did you use geom_point() before geom_line()?")
        expect_identical(class(sol_cluster_2$layers[[2]]$geom)[1],class(cluster_2$layers[[2]]$geom)[1],
                         info = "geoms are not correct. Did you use geom_line() after geom_point()?")
    })
    test_that("tick labels customized correctly", {
        expect_identical(sol_cluster_2$theme$axis.text.x$size, cluster_2$theme$axis.text.x$size,
                         info = "The x axis text size is not correct. Did you set `size` to 7?")
        expect_identical(sol_cluster_2$theme$axis.text.x$angle, cluster_2$theme$axis.text.x$angle,
                         info = "The x axis text angle is not correct. Did you set `angle` to 25?")
    })   
})

# Modify the previous plot to display Cluster 3
cluster_3 <- degrees_perc %>% 
filter(clusters == 3) %>% 
  ggplot(aes(percentile, salary, 
             group = College.Major, 
             color = College.Major
  )) + 
  geom_point(alpha = 0.6) + 
  geom_line(alpha = 0.4) + 
ggtitle("Cluster 3: The Over Achievers") + 
labs(x = "Percentile",
    y = "Salary"
    ) + 
theme(axis.text.x = element_text(size = 7, angle = 25))

# View the plot
cluster_3

sol_cluster_3 <- ggplot(sol_degrees_perc[sol_degrees_perc$clusters==3,], aes(x=percentile,y=salary, 
                    group=College.Major, color=College.Major, order=salary)) +
                    geom_point() +
                    geom_line() +
                    ggtitle('Cluster 2:  The Over Achievers') +
                    theme(axis.text.x = element_text(size=7, angle=25)) 

run_tests({
    test_that("plot is drawn correctly", {
        expect_s3_class(cluster_3,"ggplot")
        expect_equal(sol_cluster_3$data, cluster_3$data, 
        info = "The plot data is incorrect. Did you filter degrees_perc where the cluster column equals 3?")
    })
    test_that("plot mapping is correct", {
        expect_equal(sol_cluster_3$mapping$x, cluster_3$mapping$x, 
                     info = "The 'x' axis is incorrect. Did you map it to percentile?")
        expect_equal(sol_cluster_3$mapping$y, cluster_3$mapping$y,
                     info = "The 'y' axis is incorrect. Did you map it to salary?")
        expect_equal(sol_cluster_3$mapping$colour, cluster_3$mapping$colour,
                     info = "The color is not correct. Did you map it to College.Major?")
        expect_equal(sol_cluster_3$mapping$group, cluster_3$mapping$group,
                     info = "The group is not correct. Did you map it to College.Major?")
    })
    test_that("geom_point and geom_line applied", {
        expect_identical(class(sol_cluster_3$layers[[1]]$geom)[1],class(cluster_3$layers[[1]]$geom)[1],
                         info ="geoms are not correct. Did you use geom_point() before geom_line()?")
        expect_identical(class(sol_cluster_3$layers[[2]]$geom)[1],class(cluster_3$layers[[2]]$geom)[1],
                         info = "geoms are not correct. Did you use geom_line() after geom_point()?")
    })
    test_that("tick labels customized correctly", {
        expect_identical(sol_cluster_3$theme$axis.text.x$size, cluster_3$theme$axis.text.x$size,
                         info = "The x axis text size is not correct. Did you set `size` to 7?")
        expect_identical(sol_cluster_3$theme$axis.text.x$angle, cluster_3$theme$axis.text.x$angle,
                         info = "The x axis text angle is not correct. Did you set `angle` to 25?")
    })   
})

# Sort degrees by Career.Percent.Growth
degrees_labeled %>% arrange(desc(Career.Percent.Growth))

# Identify the two majors tied for highest career growth potential
highest_career_growth <- c('Math','Philosophy')

run_tests({
    test_that("highest_career_growth is correct", {
        expect_true(setequal(highest_career_growth, c('Philosophy','Math')), 
                    info = "Did you arrange degrees in descending order by Career.Percent.Growth?")
    })
})
