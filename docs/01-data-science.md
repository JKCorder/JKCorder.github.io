# Data science, political science, and public policy

This course links social science practice and undergraduate instruction in two ways – by demonstrating how social scientists use tools of data analysis to study politics, and by showing how the tools of data analysis are being used to improve program evaluation and policy analysis.

What tools do social scientists use to reach the conclusions you might find in a textbook? If you opened a basic introductory text on American government, you would see a variety of findings about the behavior of voters, the decisions of judges and the actions of members of Congress. These findings are empirical – based on observation – and many involve survey research and statistical analysis. You might not see the statistical work in the text, but the findings are typically based on research published in a more extensive form in journal articles, in places like the *American Political Science Review* or the *American Journal of Political Science*. Look over a recent issue of these journals and you will see exactly how much statistical methods have proliferated in political science.

How do we use the tools of data analysis to improve public policy? Have you seen the movie *Moneyball*? The idea of the film (Brad Pitt stars!) is that a baseball franchise with few resources uses statistical models to identify players who are likely to perform well but are currently not in lucrative contracts. The manager composes a team of inexpensive overlooked talent that becomes highly competitive. The data scientists – statisticians – make judgments about player potential that are superior to the instincts or intuitions of experienced scouts and coaches. This approach is now used throughout Major League Baseball and has extended to other professional sports. For an NBA example, see @silver_nba. Federal executives have advocated that a similar approach inform the allocation of tax dollars to programs. In a recent book, *Moneyball for Government*, Jim Nussle and Peter Orzag argue that federal agencies should invest substantial resources in program evaluation, in order to ensure that scarce tax dollars are used in the most effective way. The tools of data science and social science can be used to identity federal programs that offer effective solutions to pressing problems [@nussle].

These types of insights – from data and models – are also creeping in to journalism – especially coverage of campaigns and elections. About fifteen years ago, political scientists studying voting behavior devoted an entire book, *Unconventional Wisdom*, to outlining discrepancies between the way that journalists cover and talk about political campaigns and the ways that political scientists understand how campaigns work [@ucw1]. But, this is changing. In 2014, Ezra Klein (now at the *New York Times*), published a short piece on the ways that political science has influence political journalism: “How political science conquered Washington” [@klein]. Some of the enduring lessons or structural features of American politics – which political scientists teach and study – are now integrated with political journalism. The *Washington Post* even hosted a blog, the *Monkey Cage*, that published readable summaries of often complex statistical or other research by political scientists. One of my favorites entries is “Was women’s suffrage a failure? What new evidence tells us about the first women voters” [@corder_mc].

## The role of theory and the role of observation

How do we do “science”?  And is social science really a science?

Science is hypothesis testing.  It is important to distinguish between empirical claims (correlation) and theoretical claims (causality). We all have ideas about why people take particular actions – why some people choose to attend college, why people pick particular fields or professions, why people choose to live in a particular city or neighborhood, or how people pick from a crowded field of potential presidential candidates. All of these ideas are theories and a typical theory contains some specific empirical expectations. Those expectations - often formally labeled as a “hypothesis” - can be tested by observing the actions and choices of people.

Science is falsifiable.  Another feature of science: you should be able to disprove a theory (falsify it) by observing something in the world. This is different from saying or claiming that we have proven something. If you don’t disprove your theory, you have learned something and others can build on it. An experiment or an observation does not prove that you are correct, only that you are not wrong. There is a subtle difference.

Science is can be replicated. You should also be able to reproduce an empirical finding, which implies we also think about science as a procedure.

Social science pays attention to all of these - theory, hypothesis testing with data, and replication.

### An example of theory-testing: does your personal financial situation influence your vote choice? {-}

Consider the decision to pick a presidential candidate. What is the link between your personal finances and your choice in presidential elections? You might expect that voters will evaluate candidates based on the likely effect the candidate will have on their personal finances. It seems reasonable that you would want a candidate who would make you better off rather than worse off. There could be other reasons to support a candidate and you might even be purely altruistic in your choice, so you might support a candidate that makes others better off. But a theory that suggests voters pay attention to personal finances suggests a very specific empirical implication. Put simply, rich voters should support candidates that act or promise to lower future tax burdens. A poor voter would not have the same incentive and might prefer a candidate who raises taxes on rich others to pay for services that are targeted to the poor. So the empirical implication is simple: high income voters should support the party that consistently offers lower taxes – the Republican Party. If high income voters do not support the Republican Party, then the theory is simply wrong. Either we don’t understand what people want, people don’t understand that Republican candidates promise to lower taxes, or some combination of both.

In the most recent presidential election, the American National Election Study asked questions about presidential vote and about household income in 2020. Household income is not a perfect proxy or measure of personal finances, but it is probably a good indicator of what we are interested in – are you rich or poor? We can use a simple table to compare the presidential vote of the rich and poor. What percentage of lower income households (annual income of less than $25,000) voted for the Donald Trump? What percentage of rich households (annual income exceeding $150,000) voted for Donald Trump? 



**Table \@ref(tab:table11)** Votes for Donald Trump, by income level


Table: (\#tab:table11)

|Total household income |  %   |
|:----------------------|:----:|
|Under $25,000          | 35.5 |
|$25,000 to $150,000    | 46.7 |
|Over $150,000          | 40.1 |



The results in Table \@ref(tab:table11) don't seem to confirm our expectations: rich households were more likely to vote for Trump than poor households, but not by very much and it turns out that the households closer to the middle of the income distribution were most likely to vote for Donald Trump. This is definitely not consistent with our expectations. Our theory about voting and personal finances doesn't hold up to empirical scrutiny.  (This is new, by the way. This result did hold up before 2020 - not a powerful predictor but definitely higher income more Republican than other income groups.)

## Big data in business and social science

In one important sense the proliferation of statistical models and the emergence of data science are very recent. The statistical theory or knowledge that we need to exploit data has been around for decades. The computational power that we need to solve many problems has also been available on the desktop for more than twenty years. What is new is the ability to easily and cheaply gather and store huge amounts of information.

So where do we find data? Surveys are a common tool for observation of individuals and the only form of data we analyze in this class. The dataset we use for this class is a large survey (4,000 respondents or more). The Census Bureau conducts a monthly survey od 50,000 people (the Current Population Survey) to, among other things, estimate the unemployment rate. A survey of this form – large, typically face-to-face – can be very expensive –on the order of millions of dollars to administer, organize, and publish. A less ambitious survey can be quite inexpensive. I conducted a survey of 100 Kalamazoo county human service agency CEOs that cost less than $500 – mainly printing and postage costs [@corder_aas]. If a survey won’t work – due to costs or access to subjects – we can exploit data from official government sources: tax return data, election returns, or, particularly for public policy research, other administrative data (What is the average fine levied by the Occupational Safety and Health Administration? How many people were audited by the IRS? Which school districts have a high graduation rate?).

The internet and electronic media (and automated data retrieval) have made data access very inexpensive. Automated scripts can scrape or harvest data from any publicly accessible website. Monitoring and collection of data by firms – retail and social media - has generated enormous amounts of data about consumers and voters – this “Big Data” is a richer source of information than small surveys. Social scientists are adopting these tools and approaches too.

Interested in details? Check out the 2007 book *Super Crunchers* by economist and law professor Ian Ayres. We will read a chapter as we learn about statistical models [@ayres].

## What is regression and why do I need to understand it? 

The course is designed to make you somewhat comfortable with a statistical modeling approach called regression or ordinary least squares regression, sometimes abbreviated as OLS. The idea is that we identify something we are interested in learning about or understanding, maybe why some people turn out to vote, or why they select a particular candidate, or whether or not they buy a product, or if they are diagnosed with lung cancer. We then select a number of things that we think we could use to predict these outcomes – a theory drives these choices. For turnout: is the person a partisan, are they educated? For a diagnosis of lung cancer:  are they a smoker, are they older or younger? We then use data to test the link between the factors we think matter – the predictors – and the outcome of interest. OLS is the technique that we use to determine which factors do or do not matter.

Why do I need to understand this? Regression is widely used in a variety of contexts – from program evaluation to marketing to campaigns. If you work in an organization that routinely collects and analyzes data, someone in the organization will be using statistical models to make sense of that data. You will have a better understanding of what they are doing and how they do it.

## We have data. Why do we learn statistics?

### Inference{-}

Surveys are usually samples from a broader population: Learning about populations from samples is called inference and that is the primary reason we use statistics in political science – we rely less on big data and more on smaller surveys. We want to be precise and careful about what we can infer from these limited samples. 

### Reporting and precision{-}

A second reason we rely on statistics is that we need to summarize large amounts of data – hundreds or thousands of survey responses – with a single or small set of numbers. Figures can do the same thing (and often visuals are better) but statistical descriptions of data can also be short and easy to understand. If you want to understand the age of the people enrolled in a program, you could meaningfully communicate that information with one number – the average age of program participants was 22.

## Two caveats: data are not perfect and nothing is deterministic

### Data are not perfect{-}

Data can be expensive and challenging to collect.  There are a lot of places where you are likely to find high quality data. Sports franchises and even universities invest an enormous amount of effort collecting individual performance data on athletes. A university could invest the same type of resources collecting individual performance data on faculty (What percentages of students who take my class in the first year ultimately complete a degree at WMU? Is my DCA (degree completion average) higher or lower than other instructors?). Businesses can be similarly metric and data-driven – government is moving in the same direction. But it takes resources, time and money, to systematically collect, organize, report and exploit data. Smart organizations capture data – huge amounts data – as a matter of routine business (Facebook, for instance). Less nimble organizations have data that are stored in separate systems, rely on data entry, and are otherwise less able to capture data. Or, as is the case with academic research, an investigator must invent and deploy expensive survey or experimental research designs constrained by a fairly limited budget.

Social scientists typically have limited resources and those limitations, coupled with understandable privacy concerns, mean that we rely on relatively small samples and self-reported information (rather than extensive surveillance or monitoring).

Besides the simple lack of resources, there are a couple of other reasons that make collection of social scientific data somewhat complicated. First, simple sampling strategies have become challenging due to cell phones and survey fatigue. Cell phones make it easier to block survey calls and less likely that people will participate. There are so many groups doing surveys that, during a tight election or in a visible race, individuals may stop participating. For a review of these technical issues, see @zukin. 

The end result is that samples may be small and unrepresentative and people may answer survey questions in a cavalier way. On top of this, our tools to record responses (or available responses for the respondent) may be crude – a few categories of age or household income or a few categories to capture your party identification or social class. The bottom line is that measurement is typically imprecise and we understand that measurement error may be a problem. The alternative to relying on poor quality or sparse data is to simply guess. And that is rarely the best option.

### Observed relationships are not deterministic{-} 

One knee jerk reaction to statistical models of human behavior is to claim that the process of reducing a person to a series of numbers or a small set of attributes is crude, demeaning, and counterproductive. This is not an entirely naïve criticism. Human behavior is complex. But social scientists do not think of the world as deterministic – we think and describe our findings probabilistically. We are confident in saying that 85 percent of a group should turn out to vote. And that implies that probability that any person in the group will vote is 85 percent. But we won’t be surprised when some people (the 15 percent) do not vote.

## Speaking the language of social science

Talking about data requires you to understand a couple of basic terms: datasets, observations, variables, and codebooks. 

### Dataset{-}

A *dataset* is an electronic file, organized as rows and columns of numbers (occasionally with text), analogous to a large spreadsheet

### Observation{-}

Each row in the dataset describes a single observation – an individual, family, senator, judge, state or country depending on the “level of analysis.” A lower case n typically refers to the number of observations (n=1412 designates 1412 observations).

### Variable{-}

Each column in the dataset describes one characteristic of every observation. A column could record data about age, income, or vote choice in an election.

### Codebook{-}

A *codebook* is a document that explains, in words, the meaning of numbers in the dataset. e.g. 7=”Extremely Conservative” political views. 


## A note about statistical software

For this text I chose to focus on statistics and
interpretation of output, rather than learning a particular piece of statistical software. 

What is statistical software? There are a number of statistical packages – software that we us to analyze data – available on the market and used by social scientists. SPSS, STATA, R and SAS are the most commonly used packages. You can do statistical work in EXCEL, but it is not designed for statistical analysis and you can't even estimate the complex models that we will be using by the end of this course. Each piece of statistical software has a particular market niche – SPSS is very elementary and supported by IBM, but there is not much detail available about the underlying algorithms used to generate estimates. SAS is widely used in industry and supports a production environment – a situation where you need to frequently generate the same reports from changing data – like bank statements or performance updates. SAS requires programming skills – the SAS language is complex and the software is expensive.

R is free but, like SAS, you need to learn programming language to run scripts or enter commands line by line. Many social scientists like the fact that R is open source and R relies on the statistical and data science community to produce free specialized software (packages) to tackle complex statistical problems. 

STATA is widely used in social sciences and life sciences. While somewhat expensive it supports both command line and scripts and can generate very nice graphics. 

This document and all of the output that you create in this course is generated using R and RMarkdown. If you want to land a job doing data science work, you will need to take courses that teaches you how to use some or several types of statistical software.  One of these courses should introduce you to R.

## What’s next?

The next chapter introduces you to some basic numbers we use to describe the distribution of variables in a dataset. The substantive focus is how people identify with the major political parties. What is party identification? Who identifies with the Republican Party? Who identifies with the Democratic Party? Who are the Independents? We will use the 2020 American National Election Survey to get a handle on these questions and learn how to interpret statistical output to convey what we know.
