# tackle

Repository for the paper "Fractional Tackles: Leveraging Player Tracking Data for
Within-Play Tackling Evaluation in American Football"

## Code

First, see the `data` folder for instructions on how to get the data.

To reproduce the results in the paper, run the scripts in the `manuscript` folder.

## Abstract

Tackling is a fundamental defensive move in American football, with the main
purpose of stopping the forward motion of the ball-carrier. However, current tackling
metrics are manually recorded outcomes that are inherently flawed due to their discrete
and subjective nature. Using player tracking data, we present a novel framework for
assessing tackling contribution in a continuous and objective manner. Our approach
first identifies when a defender is in a "contact window" of the ball-carrier during a
play, before assigning value to each window and the players involved. This enables
us to devise a new metric called fractional tackles, which credits defenders for halting
the ball-carrier’s forward motion toward the end zone. We demonstrate that fractional
tackles overcome the shortcomings of traditional metrics such as tackles and assists,
by providing greater variation and measurable information for players lacking recorded
statistics like defensive linemen. We view our contribution as a significant step forward
in measuring defensive performance in American football and a clear demonstration of
the capabilities of player tracking data.

## Links

* [arXiv preprint](https://arxiv.org/abs/2403.14769)
* [Kaggle notebook](https://www.kaggle.com/code/tindata/momentum-based-fractional-tackles) ([*2024 NFL Big Data Bowl Finalist*](https://www.nfl.com/news/nfl-announces-finalists-for-sixth-annual-big-data-bowl))
