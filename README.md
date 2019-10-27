
# Tutorial: plotting maps of Spain with ggplot2


After spending the last few days playing with maps, in this tutorial I'll be showing you how to create what I think look like nice maps using just ggplot. While other packages may seem more map-friendly at first glance, I don't think any of the alternatives gives the possibility of personalizing the output as much as ggplot does - and sorry not sorry I love doing that. 

Little disclaimer: a lot of the inspiration for this post comes from the amazing [Beautiful thematic maps with ggplot2](https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/) blogpost, and I don't think my maps bring any added value to it. However, this tutorial is thought to be useful if you're interested in plotting a map of _Spain_, as I'll be discussing some specificities of that.

I'll be "drawing" two maps here: a regional and a local one. Once you get the logic, the process is basically the same, but I think that doing things incrementally may help in understanding the logic. Let's go!

````r
library(tidyverse) #can we actually code without it?
library(rgdal) #to import shapefiles
library(broom) #to convert shapefiles into the data frame structure we need
library(wesanderson) #for some nice color palettes
````

### First things first: we need shapefiles

Maps are drawn in ggplot using either **geom_path()** or **geom_polygon()**. But in order to draw any of these, we first need a shapefile giving us the coordinates of the borders of our map. You can find a lot of shapefiles online, and these are the ones I'll be using for this tutorial.

* [For the regional level](https://www.arcgis.com/home/item.html?id=5f689357238847bc823a2fb164544a77)
* [For the local level](https://opendata.esri.es/datasets/53229f5912e04f1ba6dddb70a5abeb72_0)

Now it's time to (1) read the shapefiles and (2) convert them to a data.frame object, needed for ggplot. For step 2, I'll be using the **tidy()** function from the `broom` package.

```r
#regional shapefile:
sf_regional <- readOGR("Comunidades_Autonomas_ETRS89_30N.shp")

#Converting it to df:
regional_df <- tidy(sf_regional)
```

There's a slight problem here, which is that by doing `tidy()`, we're losing all information identifying our regional units (the "id" column doesn't really tell us which region corresponds to each id). This can be easily fixed by creating a temporary data frame with the name/code of the regions (which is in the original sahapefile) and then joining it to our `regional_df` df.

````r
# Recover row name
temp_df <- data.frame(sf_regional$Texto)

# Create and append id
#If you're wondering why I'm working with id as character, 
#it'll be helpful at the local level because some ids start
# by "0", and I think it's easier to keep consistent coding.
# But if you don't have that problem feel free to go numeric!

temp_df$id <- as.character(seq(0,nrow(temp_df)-1))

#Joining
regional_df2 <- left_join(regional_df, temp_df, by="id")
````

Now time to repeat the same process for the local level. Here, I'll be using the ine_cod (CODIGOINE in the database) variable to identify the municipalities.

```r
#local shapefile:
sf_local <- readOGR("municipios/Municipios_IGN.shp")
#Converting it to df:
local_df <- tidy(sf_local)

# Recover row names
temp_df2 <- data.frame(sf_local$CODIGOINE)
# Create and append id
temp_df2$id <- as.character(seq(0,nrow(temp_df2)-1))
#joining
local_df2 <- left_join(local_df, temp_df2, by="id")%>%
#converting to character to avoid confusion with codes that start with 0
  mutate(ine_cod = as.character(`sf_local.CODIGOINE`))%>%
  select(-`sf_local.CODIGOINE`)
```

## A regional plot of intergenerational inequalities

Now that we have our shapefiles ready to be plotted, we just need some data to represent. Here I'll be using what got me started with this in the first place: the relative income of young people (18-25) as compared to the national average income, by region ("Comunidad Autónoma"). This comes from the Spanish branch of the EU SILC, that is publicly available [here](https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176807&menu=ultiDatos&idp=1254735976608), and I'm using data from 2017. That's what I was working with, but you can use whatever you want.

**Keep in mind** that you'll need to join your data to represent with the shapefile data, so be sure that you have a column in each of those that allows for joining. I'm going to skip the data cleaning here and go directly to the join. I'll do with the id column (this was the easiest as both data frames had the same order).

```r
#importing the data
load("plot_data.RData")

#joining with the regional df
regional_plot <- regional_df2%>%
  left_join(plot_data, by = "id")
````

It's finally time to plot! We'll start with a very basic plot so you can see the kind of issues we weiil be fixing:

````r
regional_plot%>%
#you should be using the following aesthetics for any plot you make:
  ggplot(aes(x=long, y = lat, group = group))+
  #ri_disp is the income variable
  geom_polygon(aes(fill = ri_disp), color = "white")+
  theme_minimal()
````
![](https://i.ibb.co/qrm3C4r/uf.png)

Let's make a short list of things we _should hate_ from this map:

* All that text in the axis. We should say goodbye to it.
* That legend looks really... ugly, right?
* Canary Islands, we love you, but you're sooooo far away. What can we do with them?
* That blues scale is not that bad, but we can probably think of something better.

Let's get to work and do this a bit more appealing.

#### Bringing the Canary Islands closer

If we want to have the Canary Islands in the map, we'll need to bring them closer. I asked on Twitter a couple of days ago whether anyone had a nice, not painful idea on how to achieve this, but most people told me they just normally do two maps and then copy-paste them.

I decided to take a bit of a different approach, which I think is a bit more efficient: changing their coordinates manually to force them to be closer and then drawing a line around (I'm creating a separate df for this).

```r
regional_plot2 <- regional_plot%>%
  mutate(lat_c = ifelse(lat <35e5, lat + 75e4, lat),
         long_c = ifelse(long <(-5e5), (long + 65e4), long))

#Exploring min and max values to know where to draw the line
regional_plot2%>%
  filter(sf_regional.Texto == "Canarias")%>%
  summarize(a = min(lat_c),
            b = max(lat_c),
            c = min(long_c),
            d = max(long_c))


#Creating separate df
canaries_line <- data.frame(long = c(-36e4, 15e4, 15e4),
                            lat = c(405e4, 405e4, 385e4))

````


#### The theme
Now we can move on to making things pretty. The next chunk creates a nice theme for maps. I've done it by mixing some things I usually apply to all my plots with some more map-specific elements taken (and slightly modified) from the Swiss map blogpost I quoted above:

```r
theme_ari_maps <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "ivory1", color = NA),
      panel.background = element_rect(fill = "ivory1", color = NA),
      legend.background = element_rect(fill = "ivory1", color = NA),
      panel.border = element_blank(),
      plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
      plot.caption = element_text(size = 7.5, color = "grey40"),
      legend.title = element_text(color = "grey40", size = 8),
      legend.text = element_text(color = "grey40", size = 7, hjust = 0),
      legend.position = c(0.7, 0.07),
      legend.text.align = 0,
      plot.margin = unit(c(.5,.5,.2,.5), "cm"),
      panel.spacing = unit(c(2,0.2,.2,0.2), "cm"))
}


```

#### The legend

Once again, here I'm taking full inspiration from Timo Grossenbacher's work, and this is mostly his code (again, it's [here](https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/)). The details of the legend that you will see on the final gplot call also come from there. Even if my measure of relative income is continuous, I think the plot will be more intuitive if I turn it into a "fake discrete" by dividing my data into 5 quintiles and converting it into five categories. I do this in the next chunk, and then use the quintiles from the actual data to choose my "pretty breaks" and create the labels. Whether you do this or not, and the breaks you choose will fully depend on your data and personal preferences.


```r
#Getting the quantiles:
quantile(regional_plot2$ri_disp, probs = c(.2,.4,.6,.8), na.rm = TRUE)
#This returns .76, .86, .94, 1.03

#I'm going to slightly change the breaks to make them prettier
#again, this fully depends on your preferences
pretty_breaks <- c(.75,.85,.95,1.05)

# Getting the minimum and maximum value to surround the breaks
minVal <- min(regional_plot2$ri_disp, na.rm = T)
maxVal <- max(regional_plot2$ri_disp, na.rm = T)

#Putting them together:
brks <- c(minVal, pretty_breaks, maxVal)

# Creating labels
labels <- c()
# round the extremes
for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]
```

Now that breaks and labels are created, the next step transforms our `ri_disp` variable and cuts it into the categories created by the breaks. This is what we'll be using from now on for the `fill` aes! 

````r
regional_plot2$brks <- cut(regional_plot2$ri_disp, 
                         breaks = brks, 
                         include.lowest = TRUE, 
                         labels = labels)

brks_scale <- levels(regional_plot2$brks)
labels_scale <- rev(brks_scale)
````
#### Getting there: colors

We're almost there, but first, let's choose a color palette. These days I've been using a lot [Wes Anderson palettes](https://github.com/karthik/wesanderson), and I think the "Zissou1", which ranges from blue to red, is quite appropriate for this kind of incremental variable. We have 5 categories so I'm creatinga  discrete scale with 5 values.

```r
pal <- wes_palette("Zissou1", 5, type = "discrete")
````

#### Time to plot (again)

Now that we have fixed most of our issues with the raw plot, it's time to do the final ggplot call. I've tried to add comments to all the steps which could create confusion.

```r
regional_plot2%>%
  #you should be using the following aesthetics for any plot you make:
  ggplot(aes(x=long_c, y = lat_c, group = group))+
  #we use brks for the fill and resuce the size of the borders
  geom_polygon(aes(fill=brks), color = "white", size = 0.3)+
  #Line to separate the Canary Islands
  geom_path(data = canaries_line, aes(x=long, y = lat, group = NULL), color = "grey40")+
  #Adding the color palette 
  #AND setting how I want the scale to look like
  scale_fill_manual(
    values = rev(pal), #I use rev so that red is for lowest values 
    breaks = rev(brks_scale),
    name = "Renta relativa",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(direction = "horizontal",
                         keyheight = unit(2, units = "mm"),
                         keywidth = unit(50 / length(labels), units = "mm"),
                         title.position = 'top',
                         title.hjust = 0.5,
                         label.hjust = 1,
                         nrow = 1,
                         byrow = T,
                         reverse = T,
                         label.position = "bottom"))+
  labs(title="La brecha territorial generacional, por territorios",
       subtitle="Nivel de renta de los jóvenes (18-25) relativo a la media de la población en España, 2017",
       caption = "Ariane Aumaitre - Datos: ECV")+
  theme_ari_maps()+
  ggsave("regions.png", height = 5, width = 6)

````
![](https://i.ibb.co/KDrpQ1n/regions.png)

## Average income at the local level

Last but not least, we'll be applying all the logic from above to create a map at the local level. I'll be using data on **average income**  (variable: `renta`) that can be downloaded from [here](https://www.ine.es/experimental/atlas/exp_atlas_tab.htm). The steps to be followed are the same:

* Importing the data (I'm skipping the data cleaning again)
* Joining it with the shapefile data frame (by variable `ine_cod`)
* Bring the Canary Islands closer (you'll see I need to change a few things here as shapefiles differ)
* Playing around a bit to get nice legend breaks
* Plotting!

```r
#Loading data
load("income_data.RData")

#Joining and editing the Canary Islands position
local_plot <- local_df2%>%
  left_join(income_data, by = "ine_cod")%>%
    mutate(renta = as.numeric(renta), #was imported as character
      lat = 1e5*lat, long = 1e5*long, #to make changes easier
           lat_c = ifelse(lat <30e5, lat + 7e5, lat),
         long_c = ifelse(long <(-10e5), (long + 65e4), long)) 

#New df for the separating geom_path
canaries_line2 <- data.frame(long = c(-12e5, -68e4, -68e4),
                            lat = c(365e4, 365e4, 347e4))

#Getting quintiles to decide on breaks
quantile(local_plot$renta, probs = c(.2,.4,.6,.8), na.rm = TRUE)
#This returns 8309, 9538, 10674, 12083

#Now I repeat the whole exact break and labels process again
#See regional plot for explanation
pretty_breaks <- c(8500,9500,10500,12000)
minVal <- min(local_plot$renta, na.rm = T)
maxVal <- max(local_plot$renta, na.rm = T)

#All together:
brks <- c(minVal, pretty_breaks, maxVal)

#Labels
labels <- c()
# round the extremes
for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]

#Bringing into df
local_plot$brks <- cut(local_plot$renta, 
                         breaks = brks, 
                         include.lowest = TRUE, 
                         labels = labels)

brks_scale <- levels(local_plot$brks)
labels_scale <- rev(brks_scale)
```

And time to create the final plot!!

```{r}
local_plot%>%
  #you should be using the following aesthetics for any plot you make:
  ggplot(aes(x=long_c, y = lat_c, group = group))+
  geom_polygon(aes(fill=brks), color = "white", size = 0.1)+
  #Line to separate the Canary Islands
  geom_path(data = canaries_line2, 
            aes(x=long, y = lat, group = NULL), 
            color = "grey40", alpha = 0.7)+
  #Adding the color palette 
  #AND setting how I want the scale to look like
  scale_fill_manual(
    values = rev(pal), #I use rev so that red is for lowest values 
    breaks = rev(brks_scale),
    name = "Renta media (€)",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(direction = "horizontal",
                         keyheight = unit(2, units = "mm"),
                         keywidth = unit(50 / length(labels), units = "mm"),
                         title.position = 'top',
                         title.hjust = 0.5,
                         label.hjust = 1,
                         nrow = 1,
                         byrow = T,
                         reverse = T,
                         label.position = "bottom"))+
  labs(title="La brecha territorial en España",
       subtitle="Renta relativa por municipio con respecto a la renta media nacional, 2016",
       caption = "Ariane Aumaitre - Datos: INE")+
  theme_ari_maps()+
  ggsave("local.png", height = 5, width = 6)
```

![](https://i.ibb.co/WWWRKDK/local.png)
