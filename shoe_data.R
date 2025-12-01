### Get Functions and Packages

# Get process control functions
# source("functions_process_control.R")


### Known Data

# Approximate Fatigue Curve
# EVA @ 0.23 g/cm^3
# 0.304 (1) -> 0.270 (200000)
# slope = -0.00000017
# EVA @ 0.17 g/cm^3
# 0.304 -> 0.255
# slope = -0.000000245

# Function to estimate number of cycles from EVA data
get_cycles = function(rho,R_E){
  slope = 0.000000075*(rho-0.17)/0.06-0.000000245
  y_int = 0.304
  log_N = y_int*(R_E-1)/slope
  cycles = log_N
  return(cycles)
}

# Density
E_eva = 18 # MPa

# Stride Length
stride = 1.25 # m


### Import Data

# Get Experimental Data
df_shoes = read.csv("Shoe_Data - Experiment (2).csv")

# Get Source Data
df_ref = read.csv("Shoe_Data - RunRepeat (4).csv")


### Make Data Workable

# Keep only shoes with measured data
df_shoes = df_shoes[complete.cases(df_shoes[, c("Avg_Retired_SA")]), ]
df_ref = df_ref[complete.cases(df_ref[, c("New_SA")]), ]

# Percent Change of SA
diff_SA = (df_shoes$Avg_Retired_SA-df_ref$New_SA)/df_ref$New_SA*100

# Remove values with an increase in stiffness
df_shoes <- df_shoes[diff_SA < 0, ]
df_ref <- df_ref[diff_SA < 0, ]

# Restructure data frame into x and y vectors
cols <- c("Retired_SA_1", "Retired_SA_2", "Retired_SA_3", "Retired_SA_4", "Retired_SA_5")
df <- df_shoes[, cols, drop = FALSE]
vec = as.vector(t(as.matrix(df))) 
vec_names <- rep(rownames(df), each = ncol(df)) %>% as.numeric()
df_SA <- data.frame(specimen = vec_names,shore_hardness = vec)
stats = limits_s(df_SA$specimen,df_SA$shore_hardness)


### Analyze

# Approximate Midsole Elastic Modulus from Gent Equation
Eold = 10^(0.0235*df_shoes$Avg_Retired_SA-0.6403)
Enew = 10^(0.0235*df_ref$New_SA-0.6403)

# Percent Change of SA (again)
diff_SA = (df_shoes$Avg_Retired_SA-df_ref$New_SA)/df_ref$New_SA*100

# Ratio of Change of SA
R_SA = df_shoes$Avg_Retired_SA/df_ref$New_SA

# Percent Change of E
diff_E = (Eold-Enew)/Enew*100

# Ratio of Change of E
R_E = Eold/Enew

# Approximate Density
df_shoes = df_shoes %>% mutate(
  rho_shoes = sqrt(Enew/E_eva))

# Approximate Miles
df_shoes = df_shoes %>% mutate(
  miles = get_cycles(df_shoes$rho_shoes,R_E)*2*stride/1609
)

# Statistics of Mileage Across All Shoes
df_shoes = df_shoes %>% mutate(
  mu_miles = mean(df_shoes$miles) %>% as.integer(),
  sd_miles = sd(df_shoes$miles) %>% as.integer()) 

# Cost of Shoes
df_shoes = df_shoes %>% mutate(
  cost = df_ref$Cost
)

# Cost per Mile
df_shoes = df_shoes %>% mutate(
  cost_mile = cost/miles,
  cost_mile_diff = cost/miles-121.1/400
)

# Mileage by Brand
brand_stats = df_shoes %>% group_by(,Shoe_Brand) %>% 
  summarise(
    mu_miles = mean(miles),
    sd_miles = sd(miles),
    nw = n()
  ) %>%
  ungroup()

# ANOVA Test Between Groups
df_shoes2 = df_shoes %>% group_by(Shoe_Brand) %>% filter(n() > 1) %>% ungroup()
anova_result <- aov(miles ~ Shoe_Brand, data = df_shoes2)
TukeyHSD(anova_result)

# Find a Distribution
hist(df_shoes$miles,freq = FALSE,xlab="Accumulated Miles",ylab="Density",main="Distribution of Acculumated Miles")

fit_norm = MASS::fitdistr(df_shoes$miles,"normal")
fit_weib = MASS::fitdistr(df_shoes$miles,"weibull")
fit_gamma = MASS::fitdistr(df_shoes$miles,"gamma")

curve(dnorm(x, 
            mean = fit_norm$estimate["mean"], 
            sd   = fit_norm$estimate["sd"]), 
      add = TRUE, col = "blue", lwd = 2)

curve(dweibull(x, 
               shape = fit_weib$estimate["shape"], 
               scale = fit_weib$estimate["scale"]), 
      add = TRUE, col = "red", lwd = 2)

curve(dgamma(x, 
             shape = fit_gamma$estimate["shape"], 
             rate  = fit_gamma$estimate["rate"]), 
      add = TRUE, col = "darkgreen", lwd = 2)

legend("topright",                               # Position ("topleft", "bottomright", etc.)
       legend = c("Normal", "Weibull", "Gamma"),  # Labels
       col = c("blue", "red", "darkgreen"),          # Colors matching your curves
       lwd = 2)

# Define bins
breaks <- seq(min(df_shoes$miles), max(df_shoes$miles), length.out = 5)  # adjust number of bins
obs <- hist(df_shoes$miles, breaks = breaks, plot = FALSE)$counts

# Bin probabilities
p_norm  <- diff(pnorm(breaks, mean = fit_norm$estimate["mean"], sd = fit_norm$estimate["sd"]))
p_weib  <- diff(pweibull(breaks, shape = fit_weib$estimate["shape"], scale = fit_weib$estimate["scale"]))
p_gamma <- diff(pgamma(breaks, shape = fit_gamma$estimate["shape"], rate = fit_gamma$estimate["rate"]))

nw = df_shoes$miles %>% length()

# Expected counts
exp_norm  <- nw * p_norm
exp_weib  <- nw * p_weib
exp_gamma <- nw * p_gamma

chisq_norm  <- sum((obs - exp_norm)^2 / exp_norm)
chisq_weib  <- sum((obs - exp_weib)^2 / exp_weib)
chisq_gamma <- sum((obs - exp_gamma)^2 / exp_gamma)

k <- length(obs)        # number of bins
p <- 2                  # number of parameters estimated (adjust per distribution)
df <- k - 1 - p

pval_norm  <- 1 - pchisq(chisq_norm, df)
pval_weib  <- 1 - pchisq(chisq_weib, df)
pval_gamma <- 1 - pchisq(chisq_gamma, df)

pval_norm
pval_weib
pval_gamma

# Assume weibull is the best
cost = function(miles){
  cost = 1000/miles*150
}

shoe_cost = data.frame(miles = rweibull(5000,shape=fit_weib$estimate["shape"],scale=fit_weib$estimate["scale"]))
shoe_cost = shoe_cost %>% mutate(
  exp_cost = miles %>% cost
)

# Compute Confidence Intervals Using normal-Distribution
# Get By Brand Stats
df_shoes3 = df_shoes2 %>% group_by(,Shoe_Brand) %>% summarize(
  mean_x = mean(miles),
  sd_x = sd(miles),
  nw = n(),
  alpha = 0.05) %>% ungroup()

# Use normal-distribution
df_shoes3 = df_shoes3 %>% group_by(,Shoe_Brand) %>% mutate(
  z_val = qnorm(1 - alpha/2))
df_shoes3 = df_shoes3 %>% group_by(,Shoe_Brand) %>% mutate(
  error = z_val * sd_x / sqrt(nw))
df_shoes3 = df_shoes3 %>% group_by(,Shoe_Brand) %>% mutate(
  CI_upper = mean_x - error,
  CI_lower = mean_x + error)


### Visualize
library(tidyverse)
library(tidyplots)

# Create a T-Test Plot for Mileage across Brands
df_shoes2 %>%
  tidyplot(x = Shoe_Brand, y = miles, color = Shoe_Brand) %>% 
  add_data_points() %>% 
  add_mean_bar(alpha = 0.4) %>% 
  add_sem_errorbar() %>%
  adjust_title("Comparison of Acculated Mileages Between Brands") %>%
  adjust_x_axis_title("") %>%
  adjust_y_axis_title("Accumulated Mileage (mi)") %>%
  adjust_legend_title(title = "Brand") %>%
  add_test_asterisks(hide_info = TRUE)

# Create a Cost/mile plot between brands
df_shoes2 %>%
  tidyplot(x= Shoe_Brand, y = cost_mile_diff, color = Shoe_Brand) %>%
  add(
    ggplot2::geom_hline(yintercept = 0)
  ) %>%
  add_data_points() %>%
  adjust_legend_title(title = "Brand") %>%
  adjust_y_axis_title(title = "Difference from Expected Cost per Mile ($/mi)") %>%
  adjust_x_axis_title(title = "Acculumated Miles (mi)") %>%
  adjust_title(title = "Comparison of Cost of Shoes Between Brands") %>%
  add_mean_bar(alpha = 0.4) %>% 
  add_sem_errorbar()

# Create Mileage CI Plot
plot_df <- df_shoes3 %>%
  rowwise() %>%
  do({
    x <- seq(.$mean_x - 4*.$sd_x, .$mean_x + 4*.$sd_x, length.out = 200)
    y <- dnorm(x, mean = .$mean_x, sd = .$sd_x)
    data.frame(group = .$Shoe_Brand, x = x, y = y)
  }) %>%
  ungroup()

plot_df <- plot_df %>%
  group_by(group) %>%
  mutate(y_norm = y / max(y)) %>%  # divide by peak
  ungroup()

# Weibull parameters
shape <- fit_weib$estimate["shape"]       # k
scale <- fit_weib$estimate["scale"]  # λ

# Generate x-values
x <- seq(0, 1000, length.out = 500)

# Compute PDF
y <- dweibull(x, shape = shape, scale = scale)

# Create data frame for plotting
weibull_df <- data.frame(x = x, y = y)

# Plot
ggplot() +
  geom_line(data = weibull_df, aes(x = x, y = y),size = 1.2, color = "blue")+
  geom_line(x = seq(0,1000,200),y = dweibull(200,shape = fit_weib$estimate["shape"],scale = fit_weib$estimate["scale"]))+
  geom_line(data = plot_df, mapping = aes(x = x, y = y_norm, color = group),size = 1.2) +
  geom_area(data = plot_df, mapping = aes(x = x, y = y_norm, fill = group),alpha = 0.3,position = "identity",show.legend = FALSE) +
  labs(title = "Normal Distributions of Four Groups",
       x = "Value", y = "Density", color = "Group") +
  theme_classic() +
  scale_colour_manual(values = c("#0072B2", "#56B4E9", "#F5C710","#D55E00"))+
  scale_fill_manual(values = c("#0072B2", "#56B4E9", "#F5C710","#D55E00"))+
  xlim(0,1000)

# Plot
ggplot()+
  #geom_line(data = weibull_df, aes(x = x, y = y),size = 1.2, color = "blue")+
  #geom_vline(xintercept = scale * gamma(1 + 1/shape),color = "blue",linetype = "dashed")+
  geom_vline(xintercept = 400,color = "red",linetype = 4)+
  geom_vline(xintercept = df_shoes3$CI_lower[1],color = "#0072B2")+
  geom_vline(xintercept = df_shoes3$CI_upper[1],color = "#0072B2")+
  geom_vline(xintercept = df_shoes3$mean_x[1],color = "#0072B2",alpha = 0.2,size = 45)+
  geom_vline(xintercept = df_shoes3$CI_lower[2],color = "#56B4E9")+
  geom_vline(xintercept = df_shoes3$CI_upper[2],color = "#56B4E9")+
  geom_vline(xintercept = df_shoes3$mean_x[2],color = "#56B4E9",alpha = 0.2,size = 54)+
  geom_vline(xintercept = df_shoes3$CI_lower[3],color = "#F5C710")+
  geom_vline(xintercept = df_shoes3$CI_upper[3],color = "#F5C710")+
  geom_vline(xintercept = df_shoes3$mean_x[3],color = "#F5C710",alpha = 0.2,size = 82)+
  geom_vline(xintercept = df_shoes3$CI_lower[4],color = "#D55E00")+
  geom_vline(xintercept = df_shoes3$CI_upper[4],color = "#D55E00")+
  geom_vline(xintercept = df_shoes3$mean_x[4],color = "#D55E00",alpha = 0.2,size = 38)+
  xlim(0,1000)+
  ylim(0,0.002)+
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank())+
  xlab("Acculumated Mileage")+
  ggtitle("Confidence Intervals of Expected Life Across Brands")+
  theme(panel.grid = element_blank())+  # removes both major and minor grid lines
  theme(
    plot.title = element_text(
      family = "Arial",      # font family (e.g., "Arial", "Times", "Courier")         # font face: "plain", "bold", "italic", "bold.italic"
      size = 12,             # font size
      hjust = 0.5            # horizontal justification: 0 = left, 0.5 = center, 1 = right
    ))

df_shoes2 %>% group_by(Shoe_Brand) %>% summarize(
  mean(cost)
)
