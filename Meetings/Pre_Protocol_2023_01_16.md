# Pre-Protocol Meeting 16.01.2023

## Things to discuss

- Finding suitable predictors
  - I tried out the following:
  -   Distance to nest
    - Residence time at the nest
    - Revisits to the nest
    - KDE area (95% and 50%) (daily, 7-days, 5-days, 3-day, difference to previous five days)
  - With different variations, the accuracy of the multinomial model was always around 60% with a Kappa of 20-40%
- A possible problem of the model could be that there is no ground truth for the nest building phase. As there are already e.g. revisits to the nest, longer residence times and a smaller area but still it is classified as non-breeding, this might confuse the model.



## List of bird IDs with no territory but incubation:
- 2016:
  - 137, 138, 139, 145, 150, 154, 156, 157, 161, 163, 164, 165, 172, 175, 180
- 2017:
  - 31, 71, 83, 98, 116, 122, 128, 140, 145
- 2018:
  - 175, 256, 267, 285, 316, 322, 325, 329, 337, 347, 350, 376, 387, 391, 396, 397, 399, 401, 402, 403, 407, 411, 421, 422, 425, 426, 429, 430, 431, 433, 437, 438
- 2019:
  - 36, 322, 347, 350, 355, 399, 407, 485, 489, 497, 499, 508, 510, 514, 515, 518, 520, 528
- 2020:
  - 499, 509, 520, 708
- 2021:
  - 486, 509, 520, 747, 788, 791, 792, 830, 832, 833, 851, 862



## List of bird IDs with no hatchlings but hatching date:
- 2016:
  - 24, 61, 79, 89, 94, 96, 113, 117, 121, 122, 128, 150, 162, 172, 187
- 2017:
  - 31, 96, 117, 128, 187, 246, 256, 259, 261, 274, 283, 284, 307, 314, 315, 317, 323, 358, 363
- 2018:
  - 391, 401, 418, 421, 425, 431, 439, 442, 455, 459, 460, 471
- 2019:
  - 114, 163, 350, 355, 432, 448, 459, 491, 512, 513, 514, 515, 516
- 2021:
  - 846


