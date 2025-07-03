Statistical Model
================

We have weekly sero-prevalence data from $t_s$ to $t_f$, the first and
last dates for which such data are available. We solve the ODE system
(TODO: insert equations and reference) using fourth-order Runge–Kutta
integration with daily time steps from $t_s - \text{offset}$ to $t_f$,
where $\text{offset}$ is the length of a warm-up period in days. This
warm-up allows the system to evolve from a small number of infectious
individuals at time $t_s - \text{offset}$ to a distribution across
compartments that is consistent with the model dynamics by time $t_s$.
We used $\text{offset} = 150$ days (TODO: justify or explore
alternatives).

We assume that the transmission rate, $\beta$, is a smooth function of
time that can be represented as.

$$
\beta(t) = \exp\left(b_0 + \sum_{i=1}^{N_B} b_iB_i(t)\right)
$$

where $t$ is the number of days since $t_s - \text{offset}$, the
$B_i(\cdot)$ are $N_B$ natural splines with one boundary knots at $t_s$
and $t_f$, and the $b_i$, $i = 0, ..., N_B$, are parameters to be
estimated that determine the shape of the time-variation function. The
prior distribution of this function is determined by the following
distributions.

$$
\log(b_i) \sim
\begin{cases}
\mathcal{N}\left(\log\left(\frac{1}{4}\right), 1\right) & \text{if } i = 0 \\
\mathcal{N}(0, 1) & \text{if } i = 1, \ldots, N_B
\end{cases}
$$

We also place priors on dynamical parameters other than the transmission
rate.

To derive priors for rate parameters, $\gamma_a$, $\gamma_i$, and
$\sigma$, we assumed the following plausible ranges for the inverses of
these quantities (TODO: justify these and/or modify them in
`scripts/params.R`).

    ##           lower upper
    ## 1/gamma_a     8    11
    ## 1/gamma_i     6     8
    ## 1/sigma       2     4

The justification for eliciting prior information on the inverses is
that the inverses are the expected number of days in the source
compartment associated with these rates.

We constructed prior distributions on the log of the rate parameters
that placed 95% probability of being between these lower and upper
values. The mean and sd of these distributions is given by:

For $\theta$ given by $\gamma_i$, $\gamma_a$, and $\sigma$ we have.

$$
\log(\theta) \sim \mathcal{N}\left(
  -\frac{\log(\text{lower}) + \log(\text{upper})}{2},
  \frac{\log(\text{upper}) - \log(\text{lower})}{2 \times 1.96}
\right)
$$

For parameters that are naturally expressed as probabilities, we have
the following table.

    ##        lower upper
    ## kappa2  0.85  0.95
    ## kappa3  0.20  0.40

For $\theta$ given by $\kappa_2$ and $\kappa_3$ we have the following
Gaussian prior on the logit scale.

$$
\log(\theta) \sim \mathcal{N}\left(
  \frac{\text{logit}(\text{lower}) + \text{logit}(\text{upper})}{2},
  \frac{\text{logit}(\text{upper}) - \text{logit}(\text{lower})}{2 \times 1.96}
\right)
$$
