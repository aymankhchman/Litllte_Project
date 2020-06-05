library('tidytext')

job_1 <- c("Duties:
. Determine the development team's needs at an analytical level;
. Define the specializations of the information collected;
. Provide support to the developer when implementing the data collection system;
. Participate in the verification process of the data quality in collaboration with the QA team;
. Create analytical models and personalized dashboards;
. Generate personalized analytical reports;
. Present in a story telling format the insights generated through analyzes, to all types of audiences;
. Provide operational recommendations in terms of design and monetization;
. Participate in the continuous improvement of decision-making processes within the production team.

Experience and qualifications:
. Experience with analytical platforms such as deltaDNA or Amplitude;
. Pertinent experience with reporting tools such as Looker, PowerBI or Tableau;
. Competent at a statistical and data mining level;
. Capacity to generate advanced knowledge from multidimensional game data;
. Good understanding of Game design concepts, Economic Game design and monetization for a mobile game;
. Experience in SQL programming: an important asset;
. Knowledge of other programming languages such as R and Python are a plus.

Interpersonal qualities:
. Ability to explain complex and interrelate phenomena in a clear and concise manner to any type of audience (English and French);
. Focused on the operational impact, resourceful and able to take initiative;
. Excellent interpersonal skills, able to collaborate with different stakeholders that have different objectives and that are potentially contradictory;
. Demonstrate pragmatism and rigor in methodological approaches;
. Able to adapt in an environment where priorities can regularly change;
. Able to be humble and help the team to move forward")


library('stringr')
library('wordcloud')
library('tidyverse')
library('ggwordcloud')
library('fishualize')
df <- data.frame(company = "compagnie 1 ", posted = as.character(job_1),stringsAsFactors = F)

str(df)

df %>%
  unnest_tokens(word,posted)%>%
  count(word,sort = TRUE)%>%
  anti_join(stop_words)%>%
  with(wordcloud(word,n,random.order = F,min.freq = 2))

df %>%
  unnest_tokens(word,posted)%>%
  count(word,sort = TRUE)%>%
  anti_join(stop_words)%>%
  filter(n>1)%>%
  ggplot(mapping = aes(label= word,size = n ,color = word))+
  geom_text_wordcloud()+
  scale_size_area(max_size = 10)+
  scale_color_fish_d(option = "Balistapus_undulatus")+
  theme_minimal()




job_2 <- c("Job Description Tell us your story. Dont go unnoticed. Explain why youre a winning candidate.
           Think TD if you crave meaningful work and embrace change like we do.
           We are a trusted North American leader that cares about people and inspires them to grow and move forward.Stay current and competitive. Carve out a career for yourself. Grow with us. Department Overview Tell us your story. Dont go unnoticed. Explain why youre a winning candidate.
           Think TD if you crave meaningful work and embrace change like we do. We are a trusted North American leader that cares about people and inspires them to grow and move forward. Stay current and competitive.
           Carve out a career for yourself. Grow with us. Heres our story: jobs.td.com Building a World-Class Team at TD We cant afford to be boring. Neither can you. The scale and scope of what TD does may surprise you. The rapid pace of change makes it a business imperative for us to be smart and open-minded in the way we think about technology TDs technology and business teams become more intertwined as new opportunities present themselves. This new era in banking does not equal boring. Not at TD, anyway.
           Our Leaders are committed to people development, career advancement and value diversity and inclusion. The Analytics & Data Science Associate Program is a 12-month enhanced leadership development opportunity in analytics and data. The Analytics & Data Science Associate will perform a variety of activities, which may include: Data Science/Advanced Analytics: Provide subject matter expertise and create a broad range of data driven solutions by applying a variety of techniques to build out complex models. Business Insight Analyst: Responsible for providing subject matter expertise, analytics and insights to recommend actions that will enable optimal business decisions and/or resolve complex business problems. Identify opportunities to drive business growth and value.
           Visualization: Provide data visualization expertise and interpretation of data into meaningful insights or visuals to help facilitate business understanding of outcomes and recommendations. 
           Business Intelligence: Responsible for the design and development of complex reports, dashboards and scorecards to support business needs, ensure alignment of metrics across the organization. Data Management: Participating in projects by analyzing approaches, processes and tools to support the business data strategy. 
           Data Governance: Responsible for improving the trust in our data through the management of integration, availability, usability, quality, integrity and security of bank data. Job Requirements Currently enrolled in or recently completed a Graduate degree in Computer Science, Mathematics, Engineering, Management and/or Data Analytics or related. 
           Strong statistical and analytical skills and interest are necessary. Experience with analyzing large data sets. Experienced in one or more of:Big Data - Predictive Analytics, Optimization, Regression, Logit Modeling, Discreet Choice, Simulation, Propensity Modeling.Machine Learning - Data/Text Mining, Programming, Algorithm Development, Automation Data Visualization, Natural Language Processing. Strong technical knowledge and experience with R, Python, SQL, SAS, Java, C/C++, Hadoop, Spark and Cloud. Experience with Data Management, Analytics, and Project Management is a plus.
           Must have strong leadership qualities including demonstrating initiative, establishing priorities and using strong communication, analytical and problem-solving skills to add value to each initiative, project or team assigned to. Able to participate as part of a team with targeted deliverables, working on initiatives designed to improve decisions, processes, systems, and applications which support our key businesses. Remain current with emerging trends and technologies in this field. Additional Information Please ensure your application includes: Resume (maximum 2 pages) Unofficial university transcript This position will commence in July 2020 . HOURS Monday-Friday, standard business hours. Inclusiveness At TD, we are committed to fostering an inclusive, accessible environment, where all employees and customers feel valued, respected and supported. We are dedicated to building a workforce that reflects the diversity of our customers and communities in which we live and serve. If you require an accommodation for the recruitment/interview process (including alternate formats of materials, or accessible meeting rooms or other accommodation), please let us know and we will work with you to meet your needs. 
           Job Family Campus Job Category - Primary Campus Program Job Category(s) Campus Program Hours TD Bank: 37.5 and TD Insurance: 35 Business Line Corporate Time Type Full Time Employment Type Regular Country Canada **Province/State (Primary) Quebec City (Primary) Montreal Work Location 1350 Rene-Levesque Blvd Corporate, 50 Boulevard Cremazie West")

df <- data.frame(company = c("compagnie 1" , "compagnie 2"), textjob = c(job_1,job_2),stringsAsFactors = F)

str(df)

df %>%
  mutate(textjob = str_replace(textjob,"[:punct:]",""),
         textjob = str_trim(textjob))%>%
  unnest_tokens(word,textjob)%>%
  group_by(company)%>%
  count(word,sort = TRUE)%>%
  anti_join(stop_words)%>%
  filter(n>1)%>%
  ggplot(mapping = aes(label = word , size = n ,color = word))+
  geom_text_wordcloud()+
  scale_size_area(max_size = 15)+
  facet_wrap(~company)+
  scale_color_fish_d(option = "Acanthurus_leucosternon")+
  theme(
    panel.background = element_rect(fill = "#f2f3f4" ),
    strip.background = element_rect(fill = "#5499c7"),
    strip.text = element_text(color = "white",size= 13),
    plot.title = element_text(size = 15 , hjust = 0.5)
  )+
  labs(title = "Mot-clés sur lequels ces deux compagnies accentuent")


