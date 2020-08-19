## Momentum ID
# Title: Frog in the Pan Hypothesis: Information Discreteness for Return Continuation

Momentum_ID.Notes.1 <- "
Motivated by the notion that a series of gradual changes attracts less attention than
sudden dramatic changes, Da, Garun and Warachka (2014) developed and tested a
frog-in-the-pan (FIP) hypothesis that originates from limited investor attention. This
hypothesis predicts that investors are less attentive to information arriving
continuously in small amounts than to information with the same cumulative stock price
implications that arrives in large amounts at discrete timepoints."
Momentum_ID.Notes.2 <- "
The information discreteness metric, ID, is calculated as follows:
ID=(Cumulative 12mo return)  x (%negative days-%positive days)"
Momentum_ID.Notes.3 <- "
According to the ID equation, past winners with a high cumulative return and a high
percentage of positive returns (%pos > %neg) imply that the cumulative return is
formed by a large number of small positive returns. This will yield a low value
for ID and corresponds to continuous information. The correlation between ID and
return continuation is –0.65 for past winners and –0.67 for past losers. Therefore,
as predicted by the FIP hypothesis, more continuous information (low ID) is associated
with greater return continuation."
Momentum_ID.Notes.4 <- "The hypothesis was modeled on all companies within the S&P 500.
The three companies with the lowest ID score are BLL, MKC and CHD. The study implies
these stocks should see the greatest return continuation over the next 8 months given
limited investor attention."

# CHD -0.10935
# MKC -0.11136
# BLL -0.13516

#CHD
CHD <- getSymbols("CHD", from = start_ccy_date, to = end_ccy_date, auto.assign = F)
CHD <- na.locf(CHD)
colnames(CHD) <- c("Open", "High", "Low", "Close", "Vol", "Adj")

CHD_plot <- ggplot() +
  geom_line(data = CHD['2018/'],
            aes(x = Index, y = Close)) +
  xlab("") + ylab(" ") +
  labs(title="CHD - Church & Dwight Co.") +
  scale_y_continuous(position = c("left")) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "1 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("blue","red","black")) +
  theme(
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="cm"),
    legend.text = element_text(family = "Palatino",size=10,
                               margin = margin(l=2, r=10, unit = "pt")),
    plot.margin = margin(t = 8, r = 5, b = 5, l = 3, "pt"),
    plot.title=element_text(
      family = "Palatino",
      color = "black",
      size=14,
      hjust = 0),
    axis.text.y=element_text(
      color = "black",
      family = "Palatino"),
    axis.title.y=element_text(
      color = "black",
      family = "Palatino",
      size=10),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

#MKC
MKC <- getSymbols("MKC", from = start_ccy_date, to = end_ccy_date, auto.assign = F)
MKC <- na.locf(MKC)
colnames(MKC) <- c("Open", "High", "Low", "Close", "Vol", "Adj")

MKC_plot <- ggplot() +
  geom_line(data = MKC['2018/'],
            aes(x = Index, y = Close)) +
  xlab("") + ylab(" ") +
  labs(title="MKC - McCormick & Company") +
  scale_y_continuous(position = c("left")) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "1 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("blue","red","black")) +
  theme(
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="cm"),
    legend.text = element_text(family = "Palatino",size=10,
                               margin = margin(l=2, r=10, unit = "pt")),
    plot.margin = margin(t = 8, r = 5, b = 5, l = 3, "pt"),
    plot.title=element_text(
      family = "Palatino",
      color = "black",
      size=14,
      hjust = 0),
    axis.text.y=element_text(
      color = "black",
      family = "Palatino"),
    axis.title.y=element_text(
      color = "black",
      family = "Palatino",
      size=10),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

#BLL
BLL <- getSymbols("BLL", from = start_ccy_date, to = end_ccy_date, auto.assign = F)
BLL <- na.locf(BLL)
colnames(BLL) <- c("Open", "High", "Low", "Close", "Vol", "Adj")

BLL_plot <- ggplot() +
  geom_line(data = BLL['2018/'],
            aes(x = Index, y = Close)) +
  xlab("") + ylab(" ") +
  labs(title="BLL - Ball Corporation") +
  scale_y_continuous(position = c("left")) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "1 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("blue","red","black")) +
  theme(
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="cm"),
    legend.text = element_text(family = "Palatino",size=10,
                               margin = margin(l=2, r=10, unit = "pt")),
    plot.margin = margin(t = 8, r = 5, b = 5, l = 3, "pt"),
    plot.title=element_text(
      family = "Palatino",
      color = "black",
      size=14,
      hjust = 0),
    axis.text.y=element_text(
      color = "black",
      family = "Palatino"),
    axis.title.y=element_text(
      color = "black",
      family = "Palatino",
      size=10),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

