# Research Repository

**Title:** Tackling uncertainty in the spatial density estimation from Mobile Network Operator data

**Author:** Marco Ramljak

**Abstract:** The processing pipeline from raw Mobile Network Operator (MNO) data to the final spatial density map requires modeling the (approximate) spatial footprint of cells – a task called "cell geo-location." Recent work has shown that, with appropriate estimation methods based on probabilistic models, the utilization of more detailed cell footprint information improves the final estimate's spatial accuracy considerably, compared with the simpler methods relying on Voronoi tessellations. However, such results were obtained (i) under the assumption of perfect cell footprint knowledge and (ii) limited to a single scenario characterized by a dense multi-layer coverage pattern with a high degree of cell overlapping. In this work, we investigate through simulations the robustness of probabilistic estimators to uncertainties and inaccuracies in the model input parameters, namely (i) the matrix of emission probabilities and (ii) prior information. To this aim, we develop parametric techniques that purposefully introduce inaccuracies into the estimation model with tunable magnitude. Also, we consider distinct prior information vectors with varying levels of informativeness. To substantiate our findings, we research the estimators' sensitivity towards different network scenarios. Our results indicate that probabilistic estimators are robust towards inaccuracies in the emission probabilities. We find that probabilistic estimators deliver more accurate results than the Voronoi methods in all scenarios, even when confronted with extremely mismatched estimation models. For iterative estimators, we observe divergence, which occurs in some special cases under severe mismatching conditions, pointing to the need to improve further the numerical methods adopted by probabilistic estimators. We expect our results to encourage further research on the probabilistic framework and novel estimation strategies.

**Full article (Preprint):** <...>

**Supplementary Material (network scenarios)** <...>

**Selected ressources:**

-   *mobloc*: This R-package is used to model cell footprints [1].

-   *SpatialKWD*: This R-package is used to approximate the Kantorovich-Wasserstein distances, comparing the spatial density estimations with the ground truth spatial density [2].

-   *MNO-simulator workflow*: To conduct our experiments and mimic MNO-like data, we use the MNO-simulator workflow [3].

|           |                                                                                                                                                             |
|-----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------|
| *authors* | Ramljak M.                                                                                                                  |
| *version* | 1.0                                                                                                                                                         |
| *status*  | 2021 - closed                                                                                                                                               |
| *license* | [EUPL](https://joinup.ec.europa.eu/sites/default/files/custom-page/attachment/eupl_v1.2_en.pdf) *(concerning the source code, please cite this repository)* |

**References:**

-   [1] Tennekes M. (2017): [**R package for mobile location algorithms and tools**](https://github.com/MobilePhoneESSnetBigData/mobloc).

-   [2] Bassetti F., Gualandi S., Veneroni M. [**On the computation of Kantorovich-Wasserstein distances between 2D-histograms by uncapacitated minimum cost flows**](https://epubs.siam.org/doi/abs/10.1137/19M1261195). SIAM J. Optim., 30(3), 2441–2469, 2020. Preprint on arXiv: [1804.00445](https://arxiv.org/abs/1804.00445).

-   [3] Ramljak M. (2021): [**MNO-simulator workflow within R**](...).
