fluidPage(
  helpText(
    'This page provides an overview of all past and present Marine Energy projects 
    in the United States at different levels of development and provides active links 
    to permitting documents from either the', 
    tags$a(
      "FERC eLibrary", href="https://elibrary.ferc.gov/eLibrary/search", 
      target="_blank"), ' or ', 
    tags$a(
      "Tethys Knowledge Base.", 
      href="https://tethys.pnnl.gov/knowledge-base", target="_blank")),
  helpText(
    'To learn more about a project, select the blue pin on the map with your 
    cursor and a pop up will open with the project’s name, timeline, geographic 
    coordinates and a list of all submitted permitting and licensing materials to 
    date. Where available, the permitting/licensing documents have been linked to 
    a downloadable PDF of the document or to pages with additional information.'),
  
    leafletOutput("map"),
  
  helpText(
    'The figure below shows all past and present Marine Energy projects 
     and the permitting milestones of each over time organized by energy type 
     (riverine, tidal, and wave). Click on the triangles in the plot to zoom the 
    map to the study location of interest. You can access relevant FERC documents 
    per project and permitting milestones by clicking on the study location icon in the map.'),
  
    plotlyOutput("p"),
  
    verbatimTextOutput("click"))

