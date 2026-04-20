# Global Oasis Knowledge Hub  

This is the framework for the **Global Oasis Knowledge Hub**. The initiative aims to support science and policy by effectively leveraging knowledge from literature.  

## Project Structure  
```
/Global_Oasis_Knowledge_Hub
│— app.R           # Main Shiny application script
│— R/            # Static assets (CSS, images, etc.)
│   └— global.R  # R-Script that loads packages and sets global variables
│   └— load_data_from_zenodo.R  # R-Script that downloads the Knowledge Hubs data which gets saved and regularly updated in Zenodo (data-pipeline in seperate Repo)
│   └— generate_overview_plots.R  # R-Script that creates summary statistics of literature corpus 
│— www/            # Static assets (CSS, images, etc.)
│   └— styles.css  # Global stylesheet for UI styling
│— input/          # Folder for data input
│— README.md       # Project documentation
```

## How to Run Locally  
1. Install **R** and **RStudio** (if not already installed).  
2. Install the **shiny** package if you haven't:  
   ```r
   install.packages("shiny")
   ```
3. Clone or download this repository.  
4. Open **`app.R`** in RStudio and run:  
   ```r
   shiny::runApp()
   ```

## 🌐 Live Web Interface  
The compiled web interface can be accessed here:  
🔗 [Global Oasis Knowledge Hub](https://hetzerj.shinyapps.io/Global_Oasis_Knowledge_Hub/)  


---  
📧 *For questions or contributions, please contact the developers (Dr. Jessica Hetzer - jessica.hetzer@senckenberg.de; Dr. Rainer M. Krug - rainer.krug@senckenberg.de, Dr. Aidin Niamir - aidin.niamir@senckenberg.de )*  


