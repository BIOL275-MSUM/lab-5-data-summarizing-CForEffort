# load packages -----------------------------------------------------------

library(tidyverse)

# reformat the iris data set from within the tidyverse package ------------

iris <- as_tibble(iris)
iris

# question 1 --------------------------------------------------------------

new_titles <- rename(iris,
       sepal_length = Sepal.Length,
       sepal_width = Sepal.Width,
       petal_length = Petal.Length,
       petal_width = Petal.Width,
       species = Species
       )
new_titles

# question 2 --------------------------------------------------------------

mm <- mutate(new_titles, sepal_length = sepal_length * 10,
       sepal_width = sepal_width * 10,
       petal_length = petal_length * 10,
       petal_width = petal_width * 10,
       )
mm

# question 3 --------------------------------------------------------------

area <- mutate(mm, sepal_area = sepal_length * sepal_width,
       petal_area = petal_length * petal_width
       )
species_area <- select(area, sepal_area, petal_area, species)
species_area

# question 4 --------------------------------------------------------------

sepal_summary <- summarize(mm, sampl_size = n(),
        max = max(sepal_length),
        min = min(sepal_length),
        range = max - min,
        median = median(sepal_length),
        q1 = quantile(sepal_length, .25),
        q3 = quantile(sepal_length, .75),
        IQR = IQR(sepal_length)
)

sepal_summary

# question 5 --------------------------------------------------------------

species <- group_by(mm, species)
species

new <- summarize(species, sampl_size = n(),
          mean = mean(petal_length),
          sd = sd(petal_length),
          var = var(petal_length),
          se = sd / sqrt(sampl_size),
          UCL = mean + (2*se),
          LCL = mean - (2*se)
)
new


# question 6 --------------------------------------------------------------

ggplot(data = species) +
        geom_jitter(mapping = aes(x = species, y = petal_length))

# question 7 --------------------------------------------------------------

ggplot(data = species) +
        geom_jitter(mapping = aes(x = species, y = petal_length)) +
        geom_crossbar(
                data = new, 
                mapping = aes(x = species, y = mean, ymax = UCL, ymin = LCL),
                color = "red"
        )

# question 8 --------------------------------------------------------------

ggplot(data = species) +
        geom_point(mapping = aes(x = petal_length, y = petal_width, color = species, shape = species))
