## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
mpg

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
mpg%>%mutate(cyl=as.character(cyl))%>%
  ggplot(mapping=aes(x=displ,y=hwy,shape=cyl,color=class))+
  geom_point()+
  geom_jitter()

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
diamonds%>%ggplot()+
  geom_bar(mapping=aes(x=cut,color=clarity),position="identity",alpha=.3,fill=NA)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
gss_cat


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
gss_cat%>%
  count(age)%>%left_join(gss_cat)%>%
  filter(partyid !="Don't know", partyid !="No answer", partyid != "Other party", partyid != "Independent")%>%
  mutate(Party= factor(partyid))%>%
  mutate(Party= fct_collapse(partyid, "Ind" = c("Other party", "Independent")))%>%
  mutate(Party= fct_relevel(Party, "Strong democrat", "Not str democrat","Ind, near dem", "Ind","Ind,near rep", "Not str republican", "Strong republican"))%>%
  mutate(Party=as.numeric(Party))%>%
  filter(!is.na(age))%>%
  group_by(age)%>%
  summarize(Party=mean(Party),avg=mean(tvhours,na.rm=TRUE),Number=n)%>%
  ggplot(mapping=aes(x=age, y=avg))+
  geom_point(mapping=aes(color=Party,size=Number))+
  scale_color_gradient(high="red",low="blue")+
  geom_smooth(color="black")+
  labs(x="Age",y="Hours",title="Average TV hours per day by age and political affiliation")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
tvwatching_byraceandreligion <- gss_cat%>%
  mutate(relig = fct_lump(relig, n = 5)) %>%
  group_by(race,relig)%>%
  summarize(avg_tvhours=mean(tvhours, na.rm= TRUE))%>%
  mutate(race=fct_relevel(race,"Black","White","Other"))%>%
  ggplot()+
  geom_col(mapping=aes(x=relig,y=avg_tvhours,fill = race), position= "dodge")+
  labs(x="Religion",y="Hours/Day", title="TV watching by race and religion")+
  theme_minimal()
tvwatching_byraceandreligion 


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
gss_cat%>%
  mutate(race=fct_relevel(race, "Black", "White", "Other"))%>%
  ggplot()+
  geom_density(aes(age,fill=race, color=race),alpha=0.5)+
  labs(x="Age",y="Population", title="Population density by age and race")+
  theme_minimal()


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
gss_cat%>%
  filter(relig !="Don't know", relig !="No answer", relig !="Other")%>%
  count(relig)%>%
  mutate(relig=fct_reorder(relig, n))%>%
  ggplot()+
  geom_bar(aes(x = "relig", y = n, fill = relig),
           stat = "identity", width = 0.4) +
  coord_polar("y")+
  labs(title="GSS Religions")+
  theme_void()

