# Statistical Rethinking Chapter 2, sections 2.1 - 2.3

Name: Nicole Soltis

## 2E1
1. the probability of rain on Monday: Pr(rain)
## 2E2
3. Pr(Monday|rain) : The probability that it is Monday, given that it is raining.
## 2E3
The probability that it is Monday, given that it is raining:
Pr(Monday|rain)
## 2E4
In reality, the globe has a set proportion of water, with complete certainty. But when an observer wants to predict the presence or absence of water based on a small sample of points on the globe, probability describes the uncertainty in whether water is expected for a random sample or not.

## 2M3
Pr(Earth) = 0.5
Pr(Mars) = 0.5
Pr(land|Mars) = 1.0
Pr(land|Earth) = 0.7
Pr(land) = Pr(land|Mars)xPr(Mars) + Pr(land|Earth)xPr(Earth) = (1.0)x(0.5) + (0.7)x(0.5) = 0.85

Pr(Earth|land) = Pr(land|Earth) x Pr(Earth) / Pr(land) = (0.7)x(0.5)/(0.85) = 0.41


Answer: Pr(Earth|land) = 0.23
## 2M4
Deck with 3 cards, 2 sides per card, B,W.
card 1: B,B
card 2: B,W
card 3: W,W
shuffle cards, draw black side

Ways to draw black:
1Ba, 1Bb
1Bb, 1Ba
2B, W

Answer: P(other side black| drew black) = 2/3
** use counting method in 2.2
