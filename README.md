# STAT 545 Homework 05: Factor and figure management

## The goals of the HW05:

- Reorder a factor in a principled way based on the data and demonstrate the effect in arranged data and in figures.
- Write some data to file and load it back into R.
- Improve a figure (or make one from scratch), using new knowledge, e.g., control the color scheme, use factor levels, smoother mechanics.
- Make a plotly visual.
- Implement visualization design principles.

## What I did for HW05:

- Part 1: Factor management
  + In the gapminder data set, I dropped Oceania. Then, I reordered the levels of country or continent. I also experimented with the arrange() function.
- Part 2: File I/O
  + I experimented with `write_csv()/read_csv()`.
- Part 3: Visualization design
  + I remade a figure from HW03, in light of something I learned about visualization design and color. Also, I used gganimate to spice the plot up a bit.
  + I played with plotly and experimented on how to display the data in a similar way as when I used gganimate.
- Part 4: Writing figures to file
  + I used ggsave() to explicitly save a plot to file. Then, I used `![Alt text](/path/to/img.png)` to load and embed it in your report. 
  + I played around with the arguments of ggsave(), such as width, height, resolution or text scaling. Also, I looked at various options for various graphics devices, e.g. a vector vs. raster format. Finally, I looked at when to use `gsave(..., plot = p)`.

## Important links for HW05:


|               | Quick and important links|
| ------------- |-------------|
|  :heavy_check_mark: | [STAT545-HW05-factors-and-figures.Rmd](https://github.com/STAT545-UBC-students/hw05-rachlobay/blob/master/STAT545-HW05-factors-and-figures.Rmd)| 
|  :heavy_check_mark: | [STAT545-HW05-factors-and-figures.md](https://github.com/STAT545-UBC-students/hw05-rachlobay/blob/master/STAT545-HW05-factors-and-figures.md)| 
