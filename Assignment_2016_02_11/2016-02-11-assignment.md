# Statistical Rethinking Chapter 2, sections 2.1 - 2.3

Name: Nicole Soltis

## 2E1
1. the probability of rain on Monday: Pr(rain)

## 2E2
3. Pr(Monday|rain) : The probability that it is Monday, given that it is raining.

## 2E3
1 and 4 are equivalent and both indicate the probability that it is Monday, given that it is raining:
Pr(Monday|rain) = Pr(r|M) x Pr(M) / Pr(r)

## 2E4
In reality, the globe has a set proportion of water, with complete certainty. But when an observer wants to predict the presence or absence of water based on a small sample of points on the globe, probability describes the uncertainty in whether water is expected for a random sample or not.

## 2M3
Pr(Earth) = 0.5

Pr(Mars) = 0.5

Pr(land|Mars) = 1.0

Pr(land|Earth) = 0.3

Pr(land) = Pr(land|Mars) x Pr(Mars) + Pr(land|Earth) x Pr(Earth) = (1.0) x (0.5) + (0.3) x (0.5) = 0.65

Pr(Earth|land) = Pr(land|Earth) x Pr(Earth) / Pr(land) = (0.3) x (0.5)/(0.65) = 0.23

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

Answer: Pr(other side black| drew black) = 2/3

## 2M5
B/B, B/W, W/W, B/B

Ways to draw black:

1Ba, 1Bb

1Bb, 1Ba

2B, W

4Ba, 4Bb

4Bb, 4Ba

Answer: Pr(other side black| drew black) = 4/5

## 2M6
For every one way to pull a B/B card, there are 2 ways to pull a B/W card and 3 ways to pull a W/W card. 

Ways to draw black:

1Ba, 1Bb (1/3)

1Bb, 1Ba (1/3)

2B, W (2/3)

Pr (2nd side B | drew B) = (1/3) + (1/3) / (1/3) + (1/3) + (2/3) = 2/4

Answer: Pr(other side black| drew black) = 0.5

## 2M7
Draw B card, draw W card. 

Cards: 1 BB, 2 BW, 3 WW

1Ba, 1Bb, 2W

1Bb, 1Ba, 2W

1Ba, 1Bb, 3Wa

1Bb, 1Ba, 3Wa

1Ba, 1Bb, 3Wb

1Bb, 1Ba, 3Wb

2B, 2W, 3Wa

2B, 2W, 3Wb

Answer: Pr (2nd side B| drew B and drew W) = 6/8 = 0.75

## 2H1
Pr(Species A) = 0.5

Pr(Sp B) = 0.5

Pr(Twin|Sp A) = 0.10

Pr(Twin|Sp B) = 0.20

Pr(Twin) = Pr(Twin|SpA)Pr(SpA) + Pr(Twin|SpB)Pr(SpB) = 0.5 x 0.10 + 0.5 x 0.20 = 0.15

Pr (Twins again | Twins now)

Pr(Sp A | Twin) = Pr(Twin|SpA)Pr(SpA)/Pr(Twin) = 0.10 x 0.5 / 0.15 = 0.33 = Pr(update SpA)

Pr(SpB|Twin) = Pr(Twin|SpB)Pr(SpB)/Pr(Twin) = 0.20 x 0.5 / 0.15 = 0.66 = Pr(up SpB)

Pr(Twin again) = Pr(Twin|SpB)Pr(up SpB) + Pr(Twin|SpA)Pr(up SpA) = 0.20 x 0.66 + 0.10 x 0.33 = 0.132 + 0.033 = 0.165

## 2H2
Pr(Sp A | Twin) = Pr(Twin|SpA)Pr(SpA)/Pr(Twin) = 0.10 x 0.5 / 0.15 = 0.33
## 2H3
Pr(Sp A | Twin then singleton)
## 2H4

