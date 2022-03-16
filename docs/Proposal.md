### Section 1: Motivation and Purpose


> Our role: Data scientist For Educational Advisory Firm
>
> Target audience: Parents or their children who want to pursue higher Education in US
>
> In United States, we have lot of parents who are worried about the future prospects of their children especially after high school or undergrad in terms of university selection to pursue higher studies. But for them it is very difficult to decide which institutions to prefer because of lesser knowledge and complex factors involved. As a result, they eventually take the services of a very famous Educational Advisory firm situated in New York which helps them in selecting institutions/universities based on various factors like tuition fees, region, salary after graduation, type etc. The firm has hired us as a Data Scientists of team of three to come up with an overall visualizations and Dashboard providing some quality Analysis/patterns based on which they can provide some useful and quality suggestions for their clients.


### Section 2: Description of the Dataset

> We will be visualizing a dataset of approximately 3,000 colleges & universities across the United States. Each university has a number of variables that describe the state where the school is located (`state`, `state_code`), the `type` of school (public, private, for-profit), the `degree_length` of the program (4-year, 2-year), and the cost of the program (`room_and_board`, `in_state_tuition`, `in_state_total`, `out_of_state_tuition`, `out_of_state_total`). We will also integrate another dataset that contains the salary potential for about 1,000 schools. We will join these 2 datasets together by school name and use the joined dataset for the visualization. 

### Section 3: Research questions and usage scenarios

> Josh is a high school student and he wants to select a school in United States to continue his study. Tuition costs and salary of graduates are the two things he cares most about. For making the decision, he wants to understand what factors impact tuition cost and investigates on tuition cost and salary of graduates of each school. He wants to be able to explore a dataset to compare the tuition cost and salary of graduates and select the most suitable school. When Josh logs on to the “School Tuition and Salary app”, he will see a list that contains all the schools in the dataset, with information about the state, school type, room costs and tuition costs. He can filter out variables for comparisons, and rank schools according to their tuition costs. When he does so, Josh may select a school from the list to compare its tuition cost and salary of graduates with the state average and national average on the bar plots. Through comparisons, he might select the top 5 suitest schools that match his expectation of tuition cost and salary of graduates and decides to conduct a follow-on investigation since majors and course content information are not captured in the current dataset.
