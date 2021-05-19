if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  dplyr, glue, here, htmltools, leaflet, plotly, RColorBrewer, readr)

prj_sites_csv   <- here("data/project_sites.csv")
prj_times_csv   <- here("data/project_times.csv")
prj_permits_csv <- here("data/project_permits.csv")

prj_sites <- read_csv(prj_sites_csv) 
prj_sites$label_html <- prj_sites$label_html %>% lapply(HTML)
prj_sites$popup_html <- prj_sites$popup_html %>% lapply(HTML)

d_times   <- read_csv(prj_times_csv) 
d_permits <- read_csv(prj_permits_csv) 

#Create factored list for permit type and technology type for plotting symbols/colors
d_permits$permit_type <- factor(d_permits$permit_type, 
                                levels = c("Notice of Intent/Preliminary Permit Application",
                                           "Draft Pilot License App",
                                           "Final Pilot License App",
                                           "Pilot License Issued",
                                           "Draft License App",
                                           "Draft Re-License App",
                                           "Final License App",
                                           "Final Re-License App",
                                           "Environmental Assessment",
                                           "Settlement Agreement",
                                           "Permit Issued"))

d_permits$technology_type <- factor(d_permits$technology_type, 
                                    levels = c('Riverine Energy', 
                                               'Tidal Energy', 
                                               'Wave Energy'))

d_times$technology_type <- factor(d_times$technology_type, 
                                  levels = c('Riverine Energy', 
                                             'Tidal Energy', 
                                             'Wave Energy'))

d_times$project_status <- factor(d_times$project_status, 
                                  levels = c('Active Project', 
                                             'Inactive Project'))

#Create dataframes for riverine, tidal, and wave energy projects
proj.sum <-
  d_times %>%
  group_by(technology_type) %>%
  distinct(project_name)

proj.sum.ord <-
  proj.sum %>%
  pull(project_name)

proj.sum.len <-
  proj.sum %>%
  summarise(n = n())

#Create objects with length of projects in each technology type
riv.n <- proj.sum.len %>%
  filter(., technology_type == "Riverine Energy") %>%
  pull(n)

tid.n <- proj.sum.len %>%
  filter(., technology_type == "Tidal Energy") %>%
  pull(n)

wav.n <- proj.sum.len %>%
  filter(., technology_type == "Wave Energy") %>%
  pull(n)

#Factor project_name by list of ordered projects

d_permits$project_name <- factor(d_permits$project_name,
                                 levels = proj.sum.ord)

d_times$project_name <- factor(d_times$project_name,
                               levels = proj.sum.ord)

#pick the color and shape scale
scale <- RColorBrewer::brewer.pal(n=10, name = 'PiYG') #For permit type
scale <- scale[c(1:5, 5, 7, 7:10)] #Create discrete values for permit type
scale2 <- c("#30A4E1", "#999999", scale) #Concatenate with color scale for Active/Inactive Projects
scale2 <- setNames( #Create named color scale
  scale2, 
  c("Active Project",
    "Inactive Project",
    "Notice of Intent/Preliminary Permit Application",
    "Draft Pilot License App",
    "Final Pilot License App",
    "Pilot License Issued",
    "Draft License App",
    "Draft Re-License App",
    "Final License App",
    "Final Re-License App",
    "Environmental Assessment",
    "Settlement Agreement",
    "Permit Issued"))

shp   <- c(rep('triangle-up', 3), 'triangle-down', 'triangle-up', 'triangle-down', 'triangle-up', 'triangle-down', rep('triangle-up', 3)) #Create shape for permit type symbols
shp2 <- c(rep(NA, 2), shp) #Concatenate with shapes (NA) for Active/Inactive Projects
shp2 <- setNames( #Create named shape scale
  shp2, 
  c("Active Project",
    "Inactive Project",
    "Notice of Intent/Preliminary Permit Application",
    "Draft Pilot License App",
    "Final Pilot License App",
    "Pilot License Issued",
    "Draft License App",
    "Draft Re-License App",
    "Final License App",
    "Final Re-License App",
    "Environmental Assessment",
    "Settlement Agreement",
    "Permit Issued"))
