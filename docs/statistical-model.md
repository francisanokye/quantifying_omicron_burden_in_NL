Statistical Model
================

We used weekly sero-prevalence data from $t_s$ to $t_f$, the first and
last dates for which such data were available. To generate model
trajectories, we solved the ODE system (TODO: insert equations and
reference) using fourth-order Runge–Kutta integration with daily time
steps from $t_s - \text{offset}$ to $t_f$. The warm-up period of length
$\text{offset} = 150$ days (TODO: justify or explore alternatives)
allows the model to evolve from an initial state with a small number of
infectious individuals to a compartmental distribution consistent with
model dynamics by time $t_s$.

The likelihood function models the logit of observed sero-prevalence as
Gaussian. Specifically, for each observation, we assume:

$$
\text{logit}(\text{observed sero-prevalence}) \sim \mathcal{N}(\mu, \sigma^2)
$$

where the mean is given by

$$
\mu = \text{logit}(R/N),
$$

with $R$ denoting the number of recovered individuals and $N$ the total
population size. The standard deviation $\sigma$ was treated as an
unknown parameter and estimated on the log scale to ensure positivity.
This likelihood also involved several other parameters, as described
below.

We modeled the time-varying transmission rate $\beta(t)$ as a smooth
function represented using natural splines:

$$
\beta(t) = \exp\left(b_0 + \sum_{i=1}^{N_B} b_i B_i(t)\right),
$$

where $t$ is the number of days since $t_s - \text{offset}$, the
$B_i(t)$ are $N_B$ natural spline basis functions with boundary knots at
$t_s$ and $t_f$, and the $b_i$ are parameters to be estimated. The prior
distributions on the log-scale spline coefficients are given by:

$$
\log(b_i) \sim
\begin{cases}
\mathcal{N}\left(\log\left(\frac{1}{4}\right), 1\right) & \text{if } i = 0 \\
\mathcal{N}(0, 1) & \text{if } i = 1, \ldots, N_B
\end{cases}
$$

We also placed priors on dynamical parameters other than the
transmission rate.

To specify priors for the rate parameters $\gamma_a$, $\gamma_i$, and
$\sigma$, we assumed plausible ranges for their reciprocals,
corresponding to the expected number of days individuals remain in the
associated source compartments:

    ##   Parameter Lower (days) Upper (days)
    ## 1 1/gamma_a            8           11
    ## 2 1/gamma_i            6            8
    ## 3   1/sigma            2            4

We specified Gaussian priors on the logarithm of each rate parameter
such that 95% of the prior mass lies between the reciprocals of the
lower and upper bounds. That is, for a parameter $\theta$, we set:

$$
\log(\theta) \sim \mathcal{N}(\mu, \sigma^2),
$$

where

$$
\mu = -\frac{\log(\text{lower}) + \log(\text{upper})}{2}, \quad
\sigma = \frac{\log(\text{upper}) - \log(\text{lower})}{2 \times 1.96}
$$

These priors reflect uncertainty about the time scale associated with
each rate.

For parameters naturally expressed as probabilities, we used the
following bounds:

    ##   Parameter Lower Upper
    ## 1    kappa2  0.85  0.95
    ## 2    kappa3  0.20  0.40

We specified Gaussian priors on the logit scale, again ensuring that 95%
of the prior mass lies between the stated bounds. That is, for a
parameter $\theta$, we used:

$$
\text{logit}(\theta) \sim \mathcal{N}(\mu, \sigma^2),
$$

where

$$
\mu = \frac{\text{logit}(\text{lower}) + \text{logit}(\text{upper})}{2}, \quad
\sigma = \frac{\text{logit}(\text{upper}) - \text{logit}(\text{lower})}{2 \times 1.96}
$$

We fitted the model by maximizing the posterior distribution using
`macpan2`, a tool that facilitates fitting compartmental models with the
Template Model Builder (TMB) package. `macpan2` uses the delta method to
compute confidence intervals, which we applied to parameter estimates,
state trajectories, and fitted sero-prevalence.
