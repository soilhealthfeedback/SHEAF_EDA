SHEAF exploratory data analysis code
=======

This repository is for SHEAF exploratory data analysis, and for exporting a combined dataset for model construction.  Each heading contains functions that produce analysis for a particular dataset.  Each function is named to describe the output it provides, along with the dataset type.


EQIP
=======

EQIP stands for Environmental Quality Incentives Program.  From weather to pests, and from a lack of time to markets, each American farmer faces a unique set of challenges. The Environmental Quality Incentives Program (EQIP) helps agricultural producers confront those challenges – all while conserving natural resources like soil, water and air.

This voluntary conservation programs helps producers make conservation work for them. Together, NRCS and producers invest in solutions that conserve natural resources for the future while also improving agricultural operations. 

Through EQIP, NRCS provides agricultural producers with financial resources and one-on-one help to plan and implement improvements, or what NRCS calls conservation practices. Using these practices can lead to cleaner water and air, healthier soil and better wildlife habitat, all while improving agricultural operations. Through EQIP, you can voluntarily implement conservation practices, and NRCS co-invests in these practices with you.


**SHEAF_eqip_map.R:**  This function maps EQIP farming practices, using leaflet.  The function can be called as such: 

    SHEAF_eqip_map(2014, "Vegetative Barrier")
    
    WHERE: the first input is the year of EQIP, and the second is the farming practice.  A full list of all farming practices are below, 
    and are also included in the header of the function.

    # PRACTICES
    #Forage and Biomass Planting                  Integrated Pest Management (IPM)            
    #Residue Management, No-Till/Strip Till       Terrace                                     
    #Prescribed Grazing                           Conservation Crop Rotation                  
    #Grassed Waterway                             Residue Management, Seasonal                
    #Residue Management, Mulch Till               Riparian Forest Buffer                      
    #Filter Strip                                 Mulching                                    
    #Cover Crop                                   Conservation Cover                          
    #Windbreak/Shelterbelt Establishment          Hedgerow Planting                           
    #Stripcropping                                Stripcropping, Field                        
    #Riparian Herbaceous Cover                    Contour Buffer Strips                       
    #Residue Management, Ridge Till               Transition to Organic Production            
    #Long Term No. Till                           Riparian Buffers - Vegetative               
    #Vegetative Barrier                           Residue and Tillage Management, No-Till     
    #Contour Orchard and Other Perennial Crops    Alley Cropping                              
    #Silvopasture Establishment                   Herbaceous Wind Barriers                    
    #Residue and Tillage Management, Ridge Till   Residue and Tillage Management, Reduced Till
    #Multi-Story Cropping                         Strip - Intercropping                       
    #Restoration of Compacted Soils              


**SHEAF_eqip_barplot_year.R:**  This function creates a barplot of a particular farming practice, by dollars paid, for a state, across all years.  
The function can be used as such:
    
    SHEAF_eqip_barplot_year("Idaho", "Vegetative Barrier")
    
    If a particular practice is not present for the selected state - function will kick out a statement that says: 
    "selected practice has no dollars paid for chosen state"
    

**SHEAF_eqip_barplot_practice.R:**  this function creates a barplot of all farming practices for a state, for a range of years.  
The function is used as such:

    SHEAF_eqip_barplot_practice("Idaho", 2005, 2010)
    
    The first year is the start year, the last year is the end year.  If a user wants only one year, start year and end year are the same.

AGCENSUS
=======

**SHEAF_agcensus_map.R:**

NRI
=======

**SHEAF_NRI_map.R:**


EXPLORATORY DATA ANALYSIS AND DATA EXTRACTION

The function below combines all datasets into one data frame.  datasets that have factors that are pertinent 
(farming practices, types of ag census categories), are transformed into columns.  In this case, there are some values that are NA 
for some factored columns, which means that, for that county/year/state combo, there was no value for that factor.

Running the function generates a dataset that is placed in the /soilsesfeedback-data/Model_data folder.  This folder ONLY contains datasets generated
using this function.  After generation, the dataset can then be called using the SHEAF_SEM model code, for analysis.  It can also be used in the 
SHEAF SEM web tool - which runs an SEM model on a selected dataset (http://soilhealthfeedback.org/dashboards/SEM)

=======

**SHEAF_model_data_creation.R:**







