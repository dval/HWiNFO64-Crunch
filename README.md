# HWiNFO64-R-Graphs
RStudio project for publishing plots from HWiNFO64 CSV files

Also an excersise in multithreading in R using the 'doSNOW' and 'Parallel' packages.


### Overview:

The script
* reads HWiNFO64 log file (csv) into a dataset.
* It parses the 'date' and 'time' columns into a single POSIX datetime.
* Sets up a local cluster.
* Farms out 1 job for each Regex.
* Each job :
    * uses the regex to create smaller more coherent datasets based on individual components in the PC
    * creates a plot for each group
    * writes all plots to image files.
* Stops the cluster.



 __Goals__:
* Break readCSV into chunks using row count / core count to create a `chunkSize`.
* Improve Regex groups
* Test breaking up the plots into time chunks vs component chunks. *(1)
* better grouping so that means of groups can be plotted against each other. *(2)
* make faster *(3)



__Regex Examples__:

Each of these overly large plots has their timelines aligned. Noon and 3PM are marked as 12:00 and 15:00 respectively.

* `"^GPU.*C\\]"`		[GPU Temperatures in °C ](https://www.dropbox.com/s/gu2vn0tq8pe6q57/GPU%20Temperatures.png?dl=0&raw=1)
* `"^GPU.*Core*"`		[GPU Core Info ](https://www.dropbox.com/s/knva2b1p1c8ozj7/GPU%20Core.png?dl=0&raw=1)
* `"^GPU.*Mem*"`		[GPU Memory Info](https://www.dropbox.com/s/3lkwem8xhk5aqyv/GPU%20Memory.png?dl=0&raw=1)
* `"^GPU.*MHz*"`		[GPU Clock Speeds](https://www.dropbox.com/s/lgkgnxeqaqqyoel/GPU%20Clock%20Speed.png?dl=0&raw=1)
* `"^(?!.*GPU).*(Mem|Page).*"`	[System Memory (Not GPU Memory)](https://www.dropbox.com/s/22n78xxq7iddv5j/System%20Memory.png?dl=0&raw=1)
* `"^(?!.*GPU).*Core.*Use*"`	[System Core Use (Not GPU Core)](https://www.dropbox.com/s/9hrht5biybcjfe3/System%20Core%20Use.png?dl=0&raw=1)
* `"^(?!.*(?:Core|CPU|Activity|Page|Mem)).*\\[%\\]"`	[GPU Utilization as % (Not any other %)](https://www.dropbox.com/s/deiywbns1xyttoj/GPU%20Utilization.png?dl=0&raw=1)
* `"^.*(Core|CPU).*\\[%\\]"`	[System Utilization as % ](https://www.dropbox.com/s/gsly4esywampz0p/System%20Utilization.png?dl=0&raw=1)
* `"^.*Activity.*\\[%\\]"`		[Disk Utilization as %](https://www.dropbox.com/s/obdogdq9s9fl2xw/Disk%20Utilization.png?dl=0&raw=1)
* `"^(?!.*RPM).*CPU*"`	[CPU Info (Not CPU Fan info)](https://www.dropbox.com/s/38xn12fijgci1k3/CPU%20Info.png?dl=0&raw=1)



__Notes__:

*(1) Currently the plotting is multi-threaded vertically. If a plot has 12 different component groups, each group uses a single thread to plot the entire timeline. Then the groups (component chunks) are just stacked on top of each other to make the plot.
I want to know if its faster to break up the plots into chunks based on time. Then individual threads could plot all components for that period. Then each of the periods (time chunks)  would be stacked next to each other to make the plot.

*(2) It would make more sense instead of looking at 16 core speeds, two different temperatures, and fan speeds, to look at the mean core speed vs mean core temp vs CPU fan speed. Much easier to read 3 lines instead of 19 lines all plotted over each other.

*(3) Always.