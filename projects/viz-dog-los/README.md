# Mind the Mix - Manage Length of Stay

## Description

**Length of stay** is an important metric to manage an animal shelter.  It is analogous to inventory turn in a retail business.  The goal is to move the inventory – get animals adopted and out of the shelter quickly.

Not all animals progress through the shelter at the same rate.  Cute puppies and kittens fly out the door, but it can take months to find a home for older dogs, less desirable breeds or animals with behavioral or medical issues.

A shelter does not have infinite capacity.  For dogs that might stay about a week in the shelter, one kennel can help 52 dogs in a year.  For dogs that reside in the shelter longer, say 2 months, that one kennel can only help 6 dogs in a year.

It is important to monitor the length of stay of the animals and try to avoid filling up with too much slow-moving inventory which reduces the shelter’s overall lifesaving capacity.  Unfortunately, shelters usually do not have a lot of control of the animals coming into the shelter.  But they can control their efforts to get animals adopted, such as off-site adoption events, reduce adoption fees (i.e., have a sale), behavioral training to make dogs more appealing (obedience, boost confidence of shy dogs, etc.) as well as a variety of marketing and promotion activities (e.g., social media, spots on the local news).  Monitoring the length of stay helps identify when some of these activities need to ramp up.

For a visualization assignment, I created a dashboard to stay on top of length of stay to manage shelter capacity using data available about dogs entering and leaving the Austin Animal Center.  

## Deliverables

- [Dashboard Image](Dashboard.pdf)
- [Interactive Dashboard on Tableau Public](https://public.tableau.com/profile/sara.lienau#!/vizhome/AAC_Dog_LOS/Dashboard) - Explore the mix of dogs and shelter capacity from Jan 1, 2016 to Oct 31, 2020.  Plus lots of tooltips with more information.

- Code
  - [Tableau (.twbx) and Data](code-tableau).


### Some Additional Comments on the Dashboard

#### Dogs in Shelter
This chart shows on the selected date the number (and proportion) of dogs in the shelter broken out by the length of stay categories. 

If you think about all the dogs that come through the shelter the length of stay categories logically is funnel shaped.  Every dog is counted in the first bucket at some point, but many leave within the first week.  Some dogs stay a bit longer and a smaller percentage end up as long-time residents. But on any given day, the mix of dogs in the shelter is rarely a funnel shape.  

There are no hard and fast rules about the ideal proportion in each category as long as the shelter can achieve its overall lifesaving goals (Happy Endings).  However, when the chart looks more like a pyramid with a large chunk of the shelter’s capacity consumed with long timers, you need to be thinking about how to get more dogs adopted.

#### Happy Endings
It is important to take stock in the positive impact the shelter is having – dogs being adopted!  Typically, no matter the time horizon you consider (e.g., month-to-date or year-to-date) the largest volume of lifesaving comes from the animals that move through the shelter quickly.  It is helpful to monitor if the mix of length of stay is helping or hindering the ability to achieve the number of happy endings needed in the community.

#### Boxplot
This chart is a way to explore the long-time residents – the dogs in shelter for 4+ months – and emphasize the number of dogs (dots) and range of time at the shelter (months to years).  An average length of stay just never adequately characterizes the true distribution.

I don’t have a lot of dimensions in the data to really characterize the dogs and explain why they might be difficult to adopt.  But a breakout by gender and size of dog was the most interesting.  Given the typical number of long-timers, I was able to include all the data points without it being too overwhelming.  In the tooltip, I included information about the breed of the dog, its color, age and length of time at the shelter.  *Note: The age of the dog is as of intake.  More data wrangling is needed to display the current age for dogs that spend years at the shelter.*

For the outliers and dogs above the median length of stay, you would really want to focus on and have some plan of action to help these dogs find a home.


#### 2020 Results
*Austin’s dogs in the shelter is very pyramid shaped for most of 2020 because of the pandemic.  There just were not the normal activities to promote adoptions with stay-at-home orders.  It was a scramble to figure out normal operations to care for the animals and keeping the human population safe.  So, the dogs mainly stayed put and aged in place expanding the bottom tiers of the length of stay categories.*




