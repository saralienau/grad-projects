# Final Project in Spatial & GIS Analysis Course

## Assignment Description

#### Geomarketing Analysis in Kansas City, Missouri

A business owner has hired you to help him identify potential locations to build a new gas station. Here are his requirements:
1.	He would prefer it to be in Kansas City, Missouri since he already has business interests there.
2.	He wants an area that has potential for population growth and/or foot traffic.
3.	He wants to know median incomes for proposed areas.
4.	He also wants to know his potential competitors for proposed areas.

Use the provided data sources and the client’s requirements to come up with at least three block groups that the client should research further to find potential sites for a gas station.

#### Resources
- The shapefiles published by the City Manager’s Office of Kansas City, Missouri are available at the link below. 
  - https://data.kcmo.org/dataset/2016-Market-Value-Analysis-MVA-/r85p-5x2c
- The shapefiles contain economic data reported at the block group level from a **Market Value Analysis (MVA) study conducted by the city in 2016**. The data dictionary for the shapefiles is at the link below.
  - https://data.kcmo.org/Neighborhoods/Market-Value-Analysis-Metadata/22hy-4i68/data?firstRun=true
- Furthermore, you know that **OpenStreetMap (OSM)** can find points of interest using tags from crowdsourcing. (For example, you can query all the coffee shops within an area.)
- You are welcome to add in additional data from the **2010 census** to your analysis. 
- You may also want to reference the following **geomarketing case study** for inspiration. https://geocompr.robinlovelace.net/location.html

## Assignment Deliverables

- Presenation with Recommendations
  - [Slides with Descriptive Notes](1a%20-%20Report%20-%20Slide%20Notes.pdf)
  - [Slides - Full Size](1b%20-%20Report%20-%20Slides.pdf)
- Code
  - Markdown Output (as github_document)
    - [Explore Data in Market Value Analysis Study](knitted-markdown/1_Explore-MVA.md)
    - [Gather Census Data](knitted-markdown/2_Gather-CensusData.md) - gather population, income & vehicle data at a census block group or tract level for Clay, Jackson, Platte counties
    - [Process Census Data](knitted-markdown/3_Process-Census.md) - shape data to focus just on Kansas City, Missouri
    - [Gather Existing Gas Stations from OpenStreetMaps](knitted-markdown/4_Gather-OSM.md)
    - [Geomarket Analysis](knitted-markdown/5_Geomarket-Analysis.md) - layer the various factors together to identify favorable locations
  - [Markdown Output (as HTML)](knitted-markdown/html) - Typically requires the files to be downloaded to view.
  - [R Project Files and Data](code-R)
