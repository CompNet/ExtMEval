# ExtMeasEval
Characterizing and Comparing External Measures for the Assessment of Cluster Analysis and Community Detection

## Description

In the context of cluster analysis and graph partitioning, many external evaluation measures
have been proposed in the literature to compare two partitions of the same set. This makes the task of
selecting the most appropriate measure for a given situation a challenge for the end user. This is why we propose to solve this issue through a new empirical evaluation framework. For a collection of candidate measures, it
first consists in describing their behavior by computing them for a generated dataset of partitions, obtained
by applying a set of predefined parametric partition transformations. Second, our framework performs a
regression analysis to characterize the measures in terms of how they are affected by these parameters and
transformations. This allows both describing and comparing the measures. 

This set of `R` scripts corresponds to the following paper: [Arınık'21]

## Data

We generate our data in a fully parametric way in order to get a greater control. See *[Arınık'21]* for more details.

Our data, as well as our results and figures, are publicly available on [FigShare](https://doi.org/10.6084/m9.figshare.13109813)

## Available evaluation measures

Note that unnormalized measures, such as MI and VI, should not be considered in our evaluation framework. Moreover, we slightly adjust the calculation of ARI in order that it gives a non-negative value in the range of [0,1]. Note that In the context of our experiments, it was always positive (no additional change was required). In practice it is very rare to get negative values for ARI, though.

* Information theoretical measures (See *[Vinh'09]* and *[Meilă'15]* for more details)
  * Mutual Information (*MI*)
  * Variation of Information (*VI*) and its normalized version (NVI)
  * Several variants of Normalized Mutual Information (*NMIsum*, *NMIsqrt*, *NMIjoint*, *NMImax*, *NMImin*, *rNMI*)
  * Several variants of Adjusted Mutual Information (*AMIsqrt*, *AMIsum*, *AMImin*, *AMImax*)
* Pair-counting measures  (See *[Meilă'15]* for more details)
  * Rand Index (RI)
  * Adjustd Rand Index (ARI)
  * Jaccard Index (JI)
  * Mirkin Metric (MM)
  * Fowlkes-Mallows Index (FMI)
* Set-matching measure  (See *[Artiles'07]*, [Meilă'15] and *[Rezaei'16]* for more details)
  * Split Join, also called Van Dongen Index (SJ)
  * F-measure (Fm)
  * Pair Sets Index (PSI)

## Organization

Here are the folders composing the project:
* Folder `src`: contains the source code (R scripts).
* Folder `out`: contains the folders and files produced by our scripts. See the *Use* section for more details.

## Installation

1. Install the [`R` language](https://www.r-project.org/)
2. Install the following R packages:
   * [`igraph`](http://igraph.org/r/) Tested with the version 1.2.6.
   * [`latex2exp`](https://cran.r-project.org/web/packages/latex2exp/index.html) Tested with the version 0.5.0.
   * [`ade4`](https://cran.r-project.org/web/packages/ade4/) Tested with the version 1.7.
   * [`genieclust` ](https://cran.r-project.org/web/packages/genieclust/)Tested with the version 1.0.0.
   * [`plot.matrix` ](https://cran.r-project.org/web/packages/plot.matrix/)Tested with the version 1.6.
   * [mclustcomp](https://cran.r-project.org/web/packages/mclustcomp/) Tested with the version 0.3.3.
   * [`clues`](https://cran.r-project.org/web/packages/clues/) Tested with the version 0.6.2.2.
   * [`NMF` ](https://cran.r-project.org/web/packages/NMF/)Tested with the version 0.23.0.
   * [`entropy`](https://cran.r-project.org/web/packages/entropy/) Tested with the version 1.3.0.

## Use

1. Open the `R` console.
3. Set the current directory as the working directory, using `setwd("<my directory>")`.
4. Run the main script `src/main.R`.

The script will produce the following subfolders in the folder `out`:
* `partitions`: Folder containing all obtained transformed partitions.
* `evaluations`: Folder containing all obtained evaluation results.
* `data-frames`: Folder containing all csv files. There are as many csv files as the number of evaluation measures. These files contain the evaluation results.
* `regression-results`: Folder containing all the results regarding regression and relative importance analysis.
* `plots`: Folder containing all plots.

## References

* **[Arınık'21]** N. Arınık, V. Labatut and R. Figueiredo, "*Characterizing and Comparing  External Measures for the Assessment of Cluster Analysis and Community  Detection*", in *IEEE Access*, vol. 9, pp. 20255-20276, 2021, doi: [10.1109/ACCESS.2021.3054621](https://doi.org/10.1109/ACCESS.2021.3054621).
* **[Artiles'07]** J. Artiles, J. Gonzalo, and S. Sekine, "*The SemEval-2007 WePS evaluation: Establishing a benchmark for the Web people search task*", in Proc. 4th Int. Workshop Semantic Eval. (SemEval). Stroudsburg, PA, USA: Association for Computational Linguistics, 2007, pp. 64–69.
* **[Vinh'09]** N. X. Vinh, J. Epps, and J. Bailey, "*Information theoretic measures for clusterings comparison: Is a correction for chance necessary?*" in Proc. 26th Annu. Int. Conf. Mach. Learn. (ICML). New York, NY, USA: ACM, 2009, pp. 1073–1080.
* **[Meilă'15]** M. Meilă, "*Criteria for comparing clusterings*", in Handbook of Cluster Analysis, 1st ed., C. Hennig, M. Meila, F. Murtagh, and R. Rocci, Eds. Boca Raton, FL, USA: CRC Press, 2015, ch. 27, pp. 619–635.
* **[Rezaei'16]** M. Rezaei and P. Franti, "*Set matching measures for external cluster validity*", IEEE Trans. Knowl. Data Eng., 2016., vol. 28, no. 8, pp. 2173–2186.
