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

We used a likelihood function that models the logit of observed
sero-prevalence as Gaussian. Specifically, for each observation, the
likelihood is based on a normal distribution for the logit-transformed
sero-prevalence, with mean equal to $\text{logit}(R/N)$, where $R$ is
the number of individuals in the recovered compartment and $N$ is the
total population size. The standard deviation of this distribution was
treated as an unknown parameter and estimated on the log scale to ensure
positivity. This likelihood function had several other parameters as we
describe below.

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

To specify priors for the rate parameters $\gamma_a$, $\gamma_i$, and
$\sigma$, we assumed plausible ranges for their reciprocals, which
correspond to the expected number of days individuals remain in the
corresponding source compartment:

    ##   Parameter Lower (days) Upper (days)
    ## 1 1/gamma_a            8           11
    ## 2 1/gamma_i            6            8
    ## 3   1/sigma            2            4

The justification for eliciting prior information on the inverses is
that the inverses are the expected number of days in the source
compartment associated with these rates.

We specified Gaussian priors on the logarithm of each rate parameter,
such that 95% of the prior mass lies between the reciprocals of the
lower and upper bounds. That is, for any such parameter $\theta$, we
set:

$$
\log(\theta) \sim \mathcal{N}(\mu,\sigma^2)
$$

where

$$
\mu = -\frac{\log(\text{lower}) + \log(\text{upper})}{2},
\sigma = \frac{\log(\text{upper}) - \log(\text{lower})}{2 \times 1.96}
$$

These priors reflect uncertainty on the time scale associated with each
rate.

For parameters that are naturally expressed as probabilities, we assumed
the following bounds:

    ##   Parameter Lower Upper
    ## 1    kappa2  0.85  0.95
    ## 2    kappa3  0.20  0.40

We then specified Gaussian priors on the logit scale, again ensuring
that 95% of the prior mass lies between the stated bounds. That is, for
such a parameter $\theta$, we used:

$$
\log(\theta) \sim \mathcal{N}(\mu,\sigma^2)
$$

$$
\mu = \frac{\text{logit}(\text{lower}) + \text{logit}(\text{upper})}{2},
\sigma = \frac{\text{logit}(\text{upper}) - \text{logit}(\text{lower})}{2 \times 1.96}
$$

We fitted this model by maximizing the posterior distribution using
`macpan2`, a tool that facilitates fitting compartmental models with the
Template Model Builder (TMB) package. `macpan2` uses the delta method to
compute confidence intervals, which we applied to parameter estimates,
state trajectories, and fitted sero-prevalence.
