Statistical Model
================

We have weekly sero-prevalence data from $t_s$ to $t_f$, the first and
last dates for which sero-prevalence data are available. We solve the
ODE (TODO: give equations and add reference to it) using Runge Kutta 4
every day between $t_s - \text{offset}$ and $t_f$, where $\text{offset}$
is the number of days in a warm-up period. The purpose of the warm up
period is to start at very low numbers of infectious individuals at
$t_s - \text{offset}$ so that we have a distribution of individuals
among state variables that is consistent with the model. We used
$\text{offset} = 150$ days (TODO: why? should we try other values?).

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

We also place priors on dynamical parameters … TODO
