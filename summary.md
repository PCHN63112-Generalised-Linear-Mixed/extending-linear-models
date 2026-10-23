# Summary

In this lesson we have discussed all the pieces needed for understanding the Generalised Linear Model framework. These discussions have necessarily been quite generic. As such, it might be difficult to understand or fully appreciate all these topics before we have seen them in practice. Next week, we will see our first example of a GLM in the form of *logistic regression*. This will provide an opportunity to apply all of these ideas and the nature of the discussions in this lesson should become clearer.

The main ideas to take away at present are that:
- Distributions exist beyond the normal distribution that are often more appropriate as models of different types of data.
- GLMs allow us to take the linear model framework and adapt it to non-normal distributions.
- This is achieved by using:
    - A distribution from the exponential family, such as the Bernoulli, binomial, Poisson or Gamma.
    - A link function to transform the regression prediction into a suitable value for that distribution.
- GLMs are then estimated using iterative maximum likelihood, rather than least-squares.
- A hypothesis testing framework can then be applied using the Wald test with asymptotic $p$-values.
- A model comparison framework can also be applied using deviance and the likelihood ratio test, also with asymptotic $p$-values. This is known as the *Analysis of Deviance*.
- In both cases, the asymptotic nature of the $p$-values means that the test are less trustworthy under smaller samples.
- Finally, assumptions can be assessed in a similar fashion to the normal linear model. However, a choice exists in terms of the type of residual that is used. Furthermore, not all diagnostics will be equally relevant or useful across all GLMs and so the specific *type* of model always needs to be considered.
