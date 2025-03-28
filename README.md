
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Probability.Sampling

<!-- badges: start -->
<!-- badges: end -->

The goal of Probability.Sampling is to analyze the results of NYS random
probabilistic sampling and contextualize them within the EPA’s National
Lake Assessment results. All scripts use EPA’s spsurvey package.

## Required packages

-   spsurvey:
    <https://cran.r-project.org/web/packages/spsurvey/index.html>
-   tidyverse (dplyr, ggplot, lubridate): <https://www.tidyverse.org>
-   huxtable: <https://hughjonesd.github.io/huxtable/>

## List of files

The results are knitted from Rmarkdowns into HTML files

-   <b>Probability.Results.2021</b>: NYS results estimations across NYS
    nutrient thresholds, CSLAP thresholds, NLA thresholds and comparison
    against LMAS results.

-   <b>Probability.Results.NLA</b>: NLA Northern Appalachian region
    results estimations across NYS nutrient thresholds and NLA
    thresholds, compared against NY probability sampling and LMAS
    results.

-   <b>Probability.Results.NLA.NYS.2021</b>: NLA national results
    estimations across NYS nutrient thresholds and NLA thresholds,
    compared against Northern Applachian and NY probability sampling
    results.

-   <b>Probability.Results.Excursions</b>: NYS probability sampling
    results estimate of “excursions” from NY StayCALM water quality
    standards for fishing use (ammonia, dissolved oxygen, nitrite, pH).

## Thresholds

### NYS nutrient criteria

In mg/L

-   <b>Phosphorus</b>
    -   &lt;0.01: Oligotrophic

    -   0.01-0.02: Mesotrophic

    -   0.02: Eutrophic
-   <b>Chlorophyll a</b>
    -   &lt;2: Oligotrophic

    -   2-8: Mesotrophic

    -   8: Eutrophic
-   <b>Secchi</b>
    -   &lt;2: Oligotrophic

    -   2-5: Mesotrophic

    -   5: Eutrophic
-   <b>NP ratio</b>
    -   &lt;1.5: N-limited

    -   1.5: P-limited
-   <b>Zebra mussels</b>
    -   &lt;10: Not susceptible

    -   10-20: May be susceptible

    -   20: Highly susceptible

### EPA NLA

The National Lakes Assessment (NLA) is a statistical survey of the
condition of the nation’s lakes, ponds, and reservoirs. In mg/L.

-   <b>Phosphorus</b>
    -   &lt;0.016: Good

    -   0.016-0.0279: Fair

    -   0.0279: Poor
-   <b>Nitrogen</b>
    -   &lt;0.0428: Good

    -   0.0428-0.655: Fair

    -   0.0655: Poor
-   <b>Chlorophyll a</b>
    -   &lt;4.52: Good

    -   4.52-8.43: Fair

    -   8.43: Poor
-   <b>Microcystin</b>
    -   NA: Non-detect

    -   &lt;8: Microcystin detected

    -   8: Most disturbed
-   <b>Dissolved oxygen</b> (average of top 2m)
    -   &lt;3: Poor

    -   3-5: Fair

    -   5: Good

### CSLAP

The Citizens Statewide Lake Assessment Program (CSLAP) is a volunteer
lake monitoring and education program that is managed cooperatively by
DEC and New York State Federation of Lake Associations (NYSFOLA).

-   <b>Conductance</b> (epilimnetic average)
    -   &lt;125: Softwater

    -   125-250: Average

    -   250: Hardwater
-   <b>Color</b>
    -   &lt;10: Uncolored

    -   10-30: Weakly colored

    -   30: Colored
-   <b>pH</b> (epilimnetic average)
    -   &lt;6.5: Acidic

    -   6.5-7.5: Circumneutral

    -   7.5-8.5: Slightly alkaline

    -   8.5: Highly alkaline

### Leech et al.

Leech, D. M., Pollard, A. I., Labou, S. G., & Hampton, S. E. (2018).
Fewer blue lakes and more murky lakes across the continental U.S.:
Implications for planktonic food webs. Limnology and Oceanography,
63(6), 2661–2680. <https://doi.org/10.1002/lno.10967>

-   <b>Lake nutrient-color status</b>
    -   Blue: Color &lt; 20 PCU & total phosphorus &lt; 30 µg/L
    -   Green: Color &lt; 20 PCU & total phosphorus &gt; 30 µg/L
    -   Brown: Color &gt; 20 PCU & total phosphorus &lt; 30 µg/L
    -   Murky: Color &gt; 20 PCU & total phosphorus &gt; 30 µg/L
