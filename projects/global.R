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

#Factor project_name by list of ordered projects

d_permits$project_name <- factor(d_permits$project_name,
                                 levels = proj.sum.ord)

d_times$project_name <- factor(d_times$project_name,
                               levels = proj.sum.ord)
