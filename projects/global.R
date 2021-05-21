if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  dplyr, glue, here, htmltools, leaflet, plotly, RColorBrewer, readr, sf)

# p_csvs <- list.files("/share/github/apps/data", "project_.*")
# file.copy(file.path("/share/github/apps/data", p_csvs), file.path("/share/github/apps_dev/data", p_csvs), overwrite = T)
prj_sites_csv        <- here("data/project_sites.csv")
prj_times_csv        <- here("data/project_times.csv")
prj_permits_csv      <- here("data/project_permits.csv")
prj_permit_types_csv <- here("data/project_permit_types.csv")

prj_sites <- read_csv(prj_sites_csv, col_types = cols()) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)

d_times <- read_csv(prj_times_csv, col_types = cols())  %>% 
  arrange(technology_type, project) %>% 
  mutate(
    technology_type = factor(technology_type))
d_times <- d_times %>% 
  mutate(
    # order projects by technology_type, then project
    project = factor(project, levels = d_times$project))
# levels(d_times$project) # Igiugig,...,Yakutat

d_permits <- read_csv(prj_permits_csv, col_types = cols()) %>% 
  left_join(
    d_times %>% 
      select(project, technology_type), 
    by = "project") %>% 
  arrange(technology_type, project)
d_permits <- d_permits %>% 
  mutate(
    # order projects by technology_type, then project
    project = factor(project, levels = distinct(d_permits, project) %>% pull(project)))
# levels(d_permits$project) # Igiugig,...,Yakutat

# order permit_types
permit_types <- read_csv(prj_permit_types_csv, col_types = cols()) %>% 
  pull(permit_types)
permit_types <- permit_types %>% 
  intersect(d_permits$permit_type)
d_permits <- d_permits %>% 
  mutate(
    # order by permit_types
    permit_type = factor(permit_type, levels = permit_types))

prj_sites$label_html <- prj_sites$label_html %>% lapply(HTML)
prj_sites$popup_html <- prj_sites$popup_html %>% lapply(HTML)

# colors & symbols
project_statuses <- unique(d_times$project_status)
cols_type  <- colorRampPalette(brewer.pal(n=11, name = 'PiYG'))(length(permit_types))
cols_status <- c("#30A4E1", "#999999") # Active/Inactive Projects
cols <- setNames(
  c(cols_type, cols_status), 
  c(permit_types, project_statuses))
syms_type  <- c(rep('triangle-up', 3), 'triangle-down', 'triangle-up', 'triangle-down', 'triangle-up', 'triangle-down', rep('triangle-up', 3))
syms_status <- rep(NA, 2)
syms <- setNames(
  c(syms_type, syms_status), 
  c(permit_types, project_statuses))

# technology_type numbers for horizontal line and label placement along y axis
n_tech <- d_times %>% 
  group_by(technology_type) %>% 
  summarize(
    n = n())
n_riv <- n_tech %>% filter(technology_type == "Riverine Energy") %>% pull(n)
n_tid <- n_tech %>% filter(technology_type == "Tidal Energy")    %>% pull(n)
n_wav <- n_tech %>% filter(technology_type == "Wave Energy")     %>% pull(n)
