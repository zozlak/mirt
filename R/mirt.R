#' Full-Information Item Factor Analysis (Multidimensional Item Response
#' Theory)
#'
#' \code{mirt} fits an unconditional maximum likelihood factor analysis model
#' to dichotomous and polytomous data under the item response theory paradigm.
#' Fits univariate and multivariate Rasch, 2-4PL, graded, (generalized) partial credit,
#' nominal, graded rating scale, Rasch rating scale, nested logistic,
#' and partially compensatory models using the traditional EM algorithm or Cai's (2010)
#' Metropolis-Hastings Robbins-Monro (MH-RM) algorithm. User defined item classes
#' can also be defined using the \code{\link{createItem}} function. Models may also contain 'explanatory'
#' person or item level predictors, though these can only be included by using the
#' \code{\link{mixedmirt}} function. Tests that form a two-tier or bi-factor structure should
#' be estimated with the \code{\link{bfactor}} function, which uses a dimension reduction EM
#' algorithm for modeling item parcels. Multiple group analyses (useful for DIF testing) are
#' also available using the \code{\link{multipleGroup}} function.
#'
#' \code{mirt} follows the item factor analysis strategy by marginal maximum
#' likelihood estimation (MML) outlined in Bock and Aiken (1981), Bock,
#' Gibbons and Muraki (1988), and Muraki and Carlson (1995).
#' Nested models may be compared via the approximate
#' chi-squared difference test or by a reduction in AIC/BIC values (comparison
#' via \code{\link{anova}}).
#'
#' \code{summary} and \code{coef} allow
#' for all the rotations available from the \code{GPArotation} package (e.g., \code{rotate = 'oblimin'})
#' as well as a \code{'promax'} rotation. Using \code{plot} will plot the test information function
#' or the test standard errors
#' for 1 and 2 dimensional solutions, or all item trace lines if only 1 dimensional the test is only
#' dichotomous items. To examine
#' individual item plots use \code{\link{itemplot}}. Residuals are
#' computed using the LD statistic (Chen & Thissen, 1997) in the lower
#' diagonal of the matrix returned by \code{residuals}, and Cramer's V above
#' the diagonal.
#'
#' @section Confirmatory and Exploratory IRT:
#'
#' Specification of the confirmatory item factor analysis model follows many of
#' the rules in the SEM framework for confirmatory factor analysis. The
#' variances of the latent factors are automatically fixed to 1 to help
#' facilitate model identification. All parameters may be fixed to constant
#' values or set equal to other parameters using the appropriate declarations.
#' If the model is confirmatory then the returned class will be 'ConfirmatoryClass'. Confirmatory
#' models may also contain 'explanatory' person or item level predictors, though including predictors
#' is limited only to the \code{\link{mixedmirt}} function.
#'
#' When specifying a single number as the second input to mirt an exploratory IRT model is estimated and
#' can be viewed as a stochastic analogue of the EM algorithm, with much of the same behaviour and
#' specifications. Rotation and target matrix options will be used in this subroutine and will be
#' passed to the returned object for use in generic functions such as \code{summary()} and
#' \code{fscores}. Again, factor means and variances are fixed to ensure proper identification.
#' If the model is confirmatory then the returned class will be 'ExploratoryClass'.
#'
#' Estimation often begins by computing a matrix of quasi-tetrachoric correlations. A
#' factor analysis with \code{nfact} is then extracted and item parameters are
#' estimated by \eqn{a_{ij} = f_{ij}/u_j}, where \eqn{f_{ij}} is the factor
#' loading for the \emph{j}th item on the \emph{i}th factor, and \eqn{u_j} is
#' the square root of the factor uniqueness, \eqn{\sqrt{1 - h_j^2}}. The
#' initial intercept parameters are determined by calculating the inverse
#' normal of the item facility (i.e., item easiness), \eqn{q_j}, to obtain
#' \eqn{d_j = q_j / u_j}. A similar implementation is also used for obtaining
#' initial values for polytomous items.
#'
#' Note that internally, the \eqn{g} and \eqn{u} parameters are transformed using a logit
#' transformation (\eqn{log(x/(1-x))}), and can be reversed by using \eqn{1 / (1 + exp(-x))}
#' following convergence. This also applies when computing confidence intervals for these parameters,
#' and is done so automatically if \code{coef(mod, rawug = FALSE)}.
#'
#' @section Convergence for quadrature methods:
#'
#' Unrestricted full-information factor analysis is known to have problems with
#' convergence, and some items may need to be constrained or removed entirely
#' to allow for an acceptable solution. As a general rule dichotomous items with
#' means greater than .95, or items that are only .05 greater than the
#' guessing parameter, should be considered for removal from the analysis or
#' treated with prior parameter distributions. The same type of reasoning is
#' applicable when including upper bound parameters as well. Also, increasing the
#' number of quadrature points per dimension may help to stabilize the estimation process
#' in higher dimensions. Finally, solutions that are not well defined also will have difficulty
#' converging, and can indicate that the model has been misspecified (e.g., extracting too many
#' dimensions).
#'
#' @section Convergence for MH-RM method:
#'
#' For the MH-RM algorithm, when the number of iterations grows very high (e.g., greater than 1500) or
#' when \code{Max Change = .2500} values are repeatedly printed
#' to the console too often (indicating that the parameters were being constrained since they are naturally
#' moving in steps greater than 0.25) then the model may either be ill defined or have a
#' very flat likelihood surface, and genuine maximum-likelihood parameter estimates may be difficult to find.
#' Additionally, it is recommended that at least 400 cycles are run through to approximate the obsevered information
#' matrix accurately, which can be accomplished either by decreasing the \code{TOL} criteria or setting
#' \code{SE = TRUE}.
#'
#' @section IRT Models:
#'
#' The parameter labels use the follow convention, here using two factors and \eqn{k} as the number
#' of categories.
#'
#' \describe{
#' \item{Rasch}{
#' Only one intercept estimated, and the latent variance of \eqn{\theta} is freely estimated. If
#' the data have more than two categories then a partial credit model is used instead (see 'gpcm' below).
#' \deqn{P(x = 1|\theta, d) = \frac{1}{1 + exp(-(\theta + d))}}
#' }
#' \item{2-4PL}{
#' Depending on the model \eqn{u} may be equal to 1 and \eqn{g} may be equal to 0.
#' \deqn{P(x = 1|\theta, \psi) = g + \frac{(u - g)}{1 + exp(-(a_1 * \theta_1 + a_2 * \theta_2 + d))}}
#' }
#' \item{graded}{
#' The graded model consists of sequential 2PL models, and here \eqn{k} is
#' the predicted category.
#' \deqn{P(x = k | \theta, \psi) = P(x \ge k | \theta, \phi) - P(x \ge k + 1 | \theta, \phi)}
#' }
#' \item{grsm}{
#' A more constrained version of the graded model where graded spacing is equal accross item blocks
#' and only adjusted by a single 'difficulty' parameter (c) while the latent variance
#' of \eqn{\theta} is freely estimated. Again,
#' \deqn{P(x = k | \theta, \psi) = P(x \ge k | \theta, \phi) - P(x \ge k + 1 | \theta, \phi)}
#' but now
#' \deqn{P = \frac{1}{1 + exp(-(a_1 * \theta_1 + a_2 * \theta_2 + d_k + c))}}
#' }
#' \item{gpcm/nominal}{For the gpcm the \eqn{d} values are treated as fixed and orderd values
#' from 0:(k-1) (in the nominal model \eqn{d_0} is also set to 0). Additionally, for identification
#' in the nominal model \eqn{ak_0 = 0}, \eqn{ak_{(k-1)} = (k - 1)}.
#' \deqn{P(x = k | \theta, \psi) = \frac{exp(ak_{k-1} * (a_1 * \theta_1 + a_2 * \theta_2) + d_{k-1})}
#' {\sum_1^k exp(ak_{k-1} * (a_1 * \theta_1 + a_2 * \theta_2) + d_{k-1})}}
#'
#' For partial credit model (when \code{itemtype = 'Rasch'}; unidimensional only) the above model
#' is further constrained so that \eqn{ak = (0,1,\ldots, k-1)}, \eqn{a_1 = 1}, and the latent
#' variance of \eqn{\theta_1} is freely estimated.
#'
#' In the nominal model this parametrizations helps to identify the empirical ordering of the
#' categories by inspecting the \eqn{ak} values. Larger values indicate that the item category is
#' more positively related to the latent trait(s) being measured. For instance, if an item was
#' truly ordinal (such as a Likert scale), and had 4 response categories, we would expect
#' to see \eqn{ak_0 < ak_1 < ak_2 < ak_3} following estimation. If on the other hand \eqn{ak_0 > ak_1}
#' then it would appear that the second category is less related to to the trait than the first, and
#' therefore the second category should be understood as the 'lowest score'.
#'
#' NOTE: The nominal model can become numerical unstable if poor choices for the high and low values
#' are chosen, resulting in \code{ak} values greater than \code{abs(10)} or more. It is recommended to choose
#' high and low anchors that cause the estimated parameters to fall between 0 and the number of categories - 1
#' either by theoretical means or by re-estimating the model with better values following convergence.
#'
#' }
#' \item{rsm}{
#' A more constrained version of the partial credit model where the spacing is equal
#' accross item blocks and only adjusted by a single 'difficulty' parameter (c). Note that this is
#' analogous to the relationship between the graded model and the grsm (with an additional
#' constraint regarding the fixed discrimination parameters; the discrimination constraint can,
#' however, be relaxed by adjusting the starting values specifications manually and applying
#' additional equality constraints).
#' }
#' \item{partcomp}{Partially compensatory models consist of the products of 2PL probability curves.
#' \deqn{P(x = 1 | \theta, \psi) = g + (1 - g) (\frac{1}{1 + exp(-(a_1 * \theta_1 + d_1))} *
#' \frac{1}{1 + exp(-(a_2 * \theta_2 + d_2))})}
#' }
#' \item{2-4PLNRM}{Nested logistic curves for modeling distractor items. Requires a scoring key.
#' The model is broken into two components for the probability of endorsement. For successful endorsement
#' the probability trace is the 1-4PL model, while for unsuccessful endorsement:
#' \deqn{P(x = 0 | \theta, \psi) = (1 - P_{1-4PL}(x = 1 | \theta, \psi)) * P_{nominal}(x = k | \theta, \psi)}
#' which is the product of the compliment of the dichotomous trace line with the nominal
#' response model. In the nominal model, the slope parameters defined above are constrained to be 1's,
#' while the last value of the \eqn{ak} is freely estimated.
#' }
#' }
#'
#' @aliases mirt
#' @param data a \code{matrix} or \code{data.frame} that consists of
#'   numerically ordered data, with missing data coded as \code{NA}
#' @param model an object returned from \code{mirt.model()} declaring how
#'   the factor model is to be estimated, or a single numeric value indicating the number
#'   of exploratory factors to estimate. See \code{\link{mirt.model}} for
#'   more details
#' @param itemtype type of items to be modeled, declared as a vector for each item or a single value
#'   which will be repeated globally. The NULL default assumes that the items follow a graded or 2PL structure,
#'   however they may be changed to the following: 'Rasch', '2PL', '3PL', '3PLu',
#'   '4PL', 'graded', 'grsm', 'gpcm', 'rsm', 'nominal', 'PC2PL', 'PC3PL', '2PLNRM', '3PLNRM', '3PLuNRM',
#'   and '4PLNRM', for the Rasch/partial credit, 2 parameter logistic,
#'   3 parameter logistic (lower or upper asymptote upper), 4 parameter logistic, graded response model,
#'   rating scale graded response model, generalized partial credit model, Rasch rating scale model,
#'   nominal model, 2-3PL partially compensatory model, and 2-4 parameter nested logistic
#'   models, respectively. User defined item classes
#'   can also be defined using the \code{\link{createItem}} function
#' @param method a character object specifying the estimation algorithm to be used. The default is \code{'EM'},
#'   for the standard EM algorithm with fixed quadrature. The option \code{'MHRM'} may also be passed to
#'   use the MH-RM algorithm
#' @param grsm.block an optional numeric vector indicating where the blocking should occur when using
#'   the grsm, NA represents items that do not belong to the grsm block (other items that may be estimated
#'   in the test data). For example, to specify two blocks of 3 with a 2PL item for the last item:
#'   \code{grsm.block = c(rep(1,3), rep(2,3), NA)}. If NULL the all items are assumed to be within the same
#'   group and therefore have the same number of item categories
#' @param rsm.block same as \code{grsm.block}, but for \code{'rsm'} blocks
#' @param key a numeric vector of the response scoring key. Required when using nested logit item types, and
#'   must be the same length as the number of items used. Items that are not nested logit will ignore this vector,
#'   so use \code{NA} in item locations that are not applicable
#' @param SE logical; estimate the standard errors? See \code{SE.type} for the type of estimates available. Using
#'   \code{SE = TRUE} when \code{method = 'MHRM'} will force the estimation to terminate no earlier than 400 
#'   iterations to ensure that the information matrix is well approximated
#' @param SE.type type of estimation method to use for calculating the parameter information matrix for computing 
#'   standard errors and \code{\link{wald}} tests. Can be \code{'MHRM'} for stochastic approximation, 
#'   \code{'BL'} for the Bock and Lieberman approach (numerical evaluation of observed Hessian), 
#'   \code{'Fisher'} for the expected information, \code{'complete'} for information based on the 
#'   complete-data Hessian used in EM algorithm (EM only), \code{'SEM'} for the supplemented EM 
#'   (disables the \code{accelerate} option; EM only), \code{'crossprod'}
#'   for standard error computations based on the variance of the Fisher scores, \code{'Louis'} 
#'   for Louis' (1982) computation of the observed information matrix, 
#'   and \code{'sandwich'} for the sandwich covariance estimate.
#'   
#'   Note that for \code{'SEM'} and \code{'MHRM'} option increasing the number of iterations 
#'   (\code{NCYCLES} and \code{TOL}, see below)  will help to improve the accuracy, and will be 
#'   run in parallel if a \code{\link{mirtCluster}} object has been defined.
#'   Bootstrapped and profiled-likelihood standard errors are also possible, but must be run with the 
#'   \code{\link{boot.mirt}} and \code{\link{PLCI.mirt}} functions, respectively
#' @param guess fixed pseudo-guessing parameters. Can be entered as a single
#'   value to assign a global guessing parameter or may be entered as a numeric
#'   vector corresponding to each item
#' @param upper fixed upper bound parameters for 4-PL model. Can be entered as a single
#'   value to assign a global guessing parameter or may be entered as a numeric
#'   vector corresponding to each item
#' @param accelerate logical; use a general acceleration algorithm described by Ramsey (1975)? Default
#'   is \code{TRUE}
#' @param rotate type of rotation to perform after the initial orthogonal
#'   parameters have been extracted by using \code{summary}; default is \code{'oblimin'}.
#'   If \code{rotate != ''} in the \code{summary}
#'   input then the default from the object is ignored and the new rotation from the list
#'   is used instead. 
#'   
#'   Rotations currently supported are: promax, oblimin, varimax, quartimin,
#'   targetT, targetQ, pstT, pstQ, oblimax, entropy, quartimax, simplimax, bentlerT, bentlerQ,
#'   tandemI, tandemII, geominT, geominQ, cfT, cfQ, infomaxT, infomaxQ, mccammon, bifactorT, bifactorQ
#' @param Target a dummy variable matrix indicting a target rotation pattern
#' @param constrain a list of user declared equality constraints. To see how to define the
#'   parameters correctly use \code{pars = 'values'} initially to see how the parameters are labeled.
#'   To constrain parameters to be equal create a list with separate concatenated vectors signifying which
#'   parameters to constrain. For example, to set parameters 1 and 5 equal, and also set parameters 2, 6, and 10 equal
#'   use \code{constrain = list(c(1,5), c(2,6,10))}. Constraints can also be specified using the
#'   \code{\link{mirt.model}} syntax
#' @param parprior a list of user declared prior item probabilities. To see how to define the
#'   parameters correctly use \code{pars = 'values'} initially to see how the parameters are labeled.
#'   Can define either normal (e.g., intercepts, lower/guessing and upper bounds),
#'   log-normal (e.g., for univariate slopes), or beta prior probabilities.
#'   To specify a prior the form is c('priortype', ...), where normal priors
#'   are \code{parprior = list(c(parnumbers, 'norm', mean, sd))},
#'   \code{parprior = list(c(parnumbers, 'lnorm', log_mean, log_sd))} for log-normal, and
#'   \code{parprior = list(c(parnumbers, 'beta', alpha, beta))} for beta. Priors can also be specified
#'   using \code{\link{mirt.model}} syntax
#' @param pars a data.frame with the structure of how the starting values, parameter numbers, estimation
#'   logical values, etc, are defined. The user may observe how the model defines the values by using \code{pars =
#'   'values'}, and this object can in turn be modified and input back into the estimation with \code{pars =
#'   mymodifiedpars}
#' @param calcNull logical; calculate the Null model for fit statics (e.g., TLI)? Only applicable if the
#'   data contains no NA's
#' @param quadpts number of quadrature points per dimension (must be an odd number).
#'   By default the number of quadrature uses the following scheme:
#'   \code{switch(as.character(nfact), '1'=41, '2'=21, '3'=11, '4'=7, '5'=5, 3)}
#' @param TOL convergence threshold for EM or MH-RM; defaults are .0001 and .001. If \code{SE.type = 'SEM'} and this
#'   value is not specified, the default is set to \code{1e-5}. If \code{empiricalhist = TRUE} and \code{TOL} is 
#'   not specified then the default \code{3e-5} will be used
#' @param large a logical, indicating whether the internal collapsed data should be returned,
#'   or list of internally computed mirt parameters containing the data. If \code{TRUE} a list containing
#'   the organized data used prior to estimation is returned. This list object can then be passed back into
#'   \code{large} to avoid reorganizing the data in every estimation (useful when the dataset are very large
#'   and computing the tabularized data is computationally burdensome). 
#'   
#'   The best strategy for large data is to always pass the internal data to the estimation function, shown below:
#'   \describe{
#'   \item{Compute organized data}{e.g., \code{internaldat <- mirt(Science, 1, large = TRUE)}}
#'   \item{Pass the organized data to all estimation functions}{e.g.,
#'   \code{mod <- mirt(Science, 1, large = internaldat)}}
#' }
#' @param empiricalhist logical; estimate prior distribution using an empirical histogram approach.
#'   Only applicable for unidimensional models estimated with the EM algorithm.
#'   The number of cycles, TOL, and quadpts are adjusted
#'   accomodate for less precision during estimation (TOL = 3e-5, NCYCLES = 2000, quadpts = 199)
#' @param nominal.highlow optional matrix indicating the highest (row 1) and lowest (row 2) categories
#'   to be used for the nominal response model. Using this input may result in better numerical stability.
#'   The matrix input should be a 2 by nitems numeric matrix, where each number represents the \emph{reduced}
#'   category representation (mirt omits categories that are missing, so if the unique values for an item
#'   are c(1,2,5,6) they are treated as being the same as c(1,2,3,4). Viewing the starting values will help
#'   to identify the categories)
#' @param draws the number of Monte Carlo draws to estimate the log-likelihood for the MH-RM algorithm. Default
#'   is 5000
#' @param GenRandomPars logical; generate random starting values prior to optimization instead of
#'   using the fixed internal starting values?
#' @param verbose logical; print observed log-likelihood value at each iteration?
#' @param technical a list containing lower level technical parameters for estimation. May be:
#'   \describe{
#'     \item{MAXQUAD}{maximum number of quadratures, which you can increase if you have more than 4GB or RAM on your PC; 
#'       default 10000}
#'     \item{SEtol}{tolerance value used to stop the MHRM estimation when \code{SE = TRUE}
#'     and \code{SE.type = 'MHRM'} and \code{method = 'EM'}. Lower values will take longer but may be more
#'     stable for computing the information matrix. Default is .0001.
#'     
#'     If \code{SE.type = 'SEM'}, this is the tollerance used to terminate the S-EM computations for each parameter,
#'     and if not specified the default is \code{.001}}
#'     \item{NCYCLES}{maximum number of EM or MH-RM cycles; defaults are 500 and 2000}
#'     \item{BURNIN}{number of burn in cycles (stage 1) in MH-RM; default 150}
#'     \item{SEMCYCLES}{number of SEM cycles (stage 2) in MH-RM; default 50}
#'     \item{set.seed}{seed number used during estimation. Default is 12345}
#'     \item{symmetric_SEM}{logical; force S-EM information matrix to be symmetric? Default is TRUE
#'       so that computation of standard errors are more stable. Setting this to FALSE can help
#'       to detect solutions that have not reached the ML estimate}
#'     \item{gain}{a vector of two values specifying the numerator and exponent
#'          values for the RM gain function \eqn{(val1 / cycle)^val2}. Default is \code{c(0.15,0.65)}}
#'     \item{customK}{a numeric value to be used to explicitly declare the number of response categories
#'           for each item. This should only be used when constructing mirt model for reasons other
#'           than parameter estimation (such as to obtain factor scores), and requires that the input data
#'           all have 0 as the lowest category. The format is the same as the
#'           \code{mod@@K} slot in all converged models}
#'     \item{customPriorFun}{a custom function used to determine the normalized density for integration
#'          in the EM algorithm. Must be of the form \code{function(Theta){...}}, and return a numeric vector
#'          with the same length as number of rows in \code{Theta}}
#'     \item{customTheta}{a custom \code{Theta} grid, in matrix form, used for integration. 
#'          If not defined, the grid is determined internally based on the number of \code{quadpts}}
#'     \item{MHcand}{a vector of values used to tune the MH sampler. Larger values will 
#'          cause the acceptance ratio to decrease. Only one value is required for unconditional 
#'          item factor analysis (\code{mixedmirt()} requires additional values for random effect). 
#'          If null, these values are determined internally, attempting to make the draws between
#'          .1 and .4}
#'     \item{Moptim}{Choose which optimizer to use. By default the EM algorithm with use the 
#'          \code{'BFGS'} when there are no upper and lower bounds, and \code{'L-BFGS-B'} when there
#'          are. The \code{'Nelder-Mead'} and \code{'SANN'} estimators may also be used, though 
#'          their routine use generally is not required. The MH-RM algorithm uses the Newton-Raphson by 
#'          default, and currently cannot be changed}
#'   }
#' @param ... additional arguments to be passed
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @seealso \code{\link{anova-method}}, \code{\link{coef-method}}, \code{\link{summary-method}},
#'   \code{\link{residuals-method}}, \code{\link{plot-method}}, 
#'   \code{\link{expand.table}}, \code{\link{key2binary}}, \code{\link{mirt.model}}, \code{\link{mirt}},
#'   \code{\link{bfactor}}, \code{\link{multipleGroup}}, \code{\link{mixedmirt}}, \code{\link{mod2values}},
#'   \code{\link{wald}}, \code{\link{itemplot}}, \code{\link{fscores}}, 
#'   \code{\link{M2}},
#'   \code{\link{extract.item}}, \code{\link{iteminfo}}, \code{\link{testinfo}}, \code{\link{probtrace}},
#'   \code{\link{boot.mirt}}, \code{\link{PLCI.mirt}}, \code{\link{imputeMissing}}, \code{\link{itemfit}},
#'   \code{\link{simdata}}, \code{\link{createItem}}, \code{\link{mirtCluster}}
#'
#' @references
#'
#' Andrich, D. (1978). A rating scale formulation for ordered response categories.
#' \emph{Psychometrika, 43}, 561-573.
#'
#' Bock, R. D., & Aitkin, M. (1981). Marginal maximum likelihood estimation of
#' item parameters: Application of an EM algorithm. \emph{Psychometrika,
#' 46}(4), 443-459.
#'
#' Bock, R. D., Gibbons, R., & Muraki, E. (1988). Full-Information Item Factor
#' Analysis. \emph{Applied Psychological Measurement, 12}(3), 261-280.
#'
#' Bock, R. D. & Lieberman, M. (1970). Fitting a response model for n dichotomously
#' scored items. \emph{Psychometrika, 35}, 179-197.
#'
#' Cai, L. (2010a). High-Dimensional exploratory item factor analysis by a
#' Metropolis-Hastings Robbins-Monro algorithm. \emph{Psychometrika, 75},
#' 33-57.
#'
#' Cai, L. (2010b). Metropolis-Hastings Robbins-Monro algorithm for confirmatory
#' item factor analysis. \emph{Journal of Educational and Behavioral
#' Statistics, 35}, 307-335.
#'
#' Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory
#' Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
#'
#' Chen, W. H. & Thissen, D. (1997). Local dependence indices for item pairs using item
#' response theory. \emph{Journal of Educational and Behavioral Statistics, 22}, 265-289.
#'
#' Lord, F. M. & Novick, M. R. (1968). Statistical theory of mental test scores. Addison-Wesley.
#'
#' Ramsay, J. O. (1975). Solving implicit equations in psychometric data analysis.
#' \emph{Psychometrika, 40}, 337-360.
#'
#' Rasch, G. (1960). Probabilistic models for some intelligence and attainment tests.
#' \emph{Danish Institute for Educational Research}.
#'
#' Muraki, E. (1992). A generalized partial credit model: Application of an EM algorithm.
#' \emph{Applied Psychological Measurement, 16}, 159-176.
#'
#' Muraki, E. & Carlson, E. B. (1995). Full-information factor analysis for polytomous
#' item responses. \emph{Applied Psychological Measurement, 19}, 73-90.
#'
#' Samejima, F. (1969). Estimation of latent ability using a response pattern of
#' graded scores. \emph{Psychometrika Monographs}, 34.
#'
#' Suh, Y. & Bolt, D. (2010). Nested logit models for multiple-choice item response data.
#' \emph{Psychometrika, 75}, 454-473.
#'
#' Sympson, J. B. (1977). A model for testing with multidimensional items.
#' Proceedings of the 1977 Computerized Adaptive Testing Conference.
#'
#' Thissen, D. (1982). Marginal maximum likelihood estimation for the one-parameter logistic model.
#' \emph{Psychometrika, 47}, 175-186.
#'
#' Wood, R., Wilson, D. T., Gibbons, R. D., Schilling, S. G., Muraki, E., &
#' Bock, R. D. (2003). \emph{TESTFACT 4 for Windows: Test Scoring, Item Statistics,
#' and Full-information Item Factor Analysis} [Computer software]. Lincolnwood,
#' IL: Scientific Software International.
#'
#' @keywords models
#' @export mirt
#' @examples
#'
#' \dontrun{
#' #load LSAT section 7 data and compute 1 and 2 factor models
#' data <- expand.table(LSAT7)
#'
#' (mod1 <- mirt(data, 1))
#' coef(mod1)
#' (mod2 <- mirt(data, 1, SE = TRUE)) #standard errors with crossprod method
#' (mod2 <- mirt(data, 1, SE = TRUE, SE.type = 'SEM')) #standard errors with SEM method
#' coef(mod2)
#' (mod3 <- mirt(data, 1, SE = TRUE, SE.type = 'BL')) #standard errors with BL method
#' residuals(mod1)
#' plot(mod1) #test information function
#' plot(mod1, type = 'trace') #trace lines
#'
#' #estimated 3PL model for item 5 only
#' (mod1.3PL <- mirt(data, 1, itemtype = c('2PL', '2PL', '2PL', '2PL', '3PL')))
#' coef(mod1.3PL)
#' #internally g and u pars are stored as logits, so usually a good idea to include normal prior
#' #  to help stabilize the paramteres. For a value around .182 use a mean
#' #  of -1.5 (since 1 / (1 + exp(-(-1.5))) == .182)
#' (mod1.3PL.norm <- mirt(data, 1, itemtype = c('2PL', '2PL', '2PL', '2PL', '3PL'),
#'     parprior = list(c(19, 'norm', -1.5, 3))))
#' coef(mod1.3PL.norm)
#'
#' #could also define priors using mirt.model() syntax
#' model <- mirt.model('F = 1-5
#'                      PRIOR = (5, g, norm, -1.5, 3)')
#' mod1.3PL.norm2 <- mirt(data, model, itemtype = c('2PL', '2PL', '2PL', '2PL', '3PL'))
#' coef(mod1.3PL.norm2)
#' #limited information fit statistics
#' M2(mod1.3PL.norm, calcNull=TRUE)
#'
#' #two factors (exploratory)
#' mod2 <- mirt(data, 2)
#' coef(mod2)
#' summary(mod2, rotate = 'oblimin') #oblimin rotation
#' residuals(mod2)
#' plot(mod2)
#'
#' anova(mod1, mod2) #compare the two models
#' scores <- fscores(mod2) #save factor score table
#' scoresfull <- fscores(mod2, full.scores = TRUE, scores.only = TRUE) #factor scores
#'
#' #confirmatory (as an example, model is not identified since you need 3 items per factor)
#' cmodel <- mirt.model('
#'    F1 = 1,4,5
#'    F2 = 2,3')
#'
#'
#' cmod <- mirt(data, cmodel)
#' coef(cmod)
#' anova(cmod, mod2)
#' #check if identified by computing information matrix
#' (cmod <- mirt(data, cmodel, SE = T))
#'
#' ###########
#' #data from the 'ltm' package in numeric format
#' pmod1 <- mirt(Science, 1)
#' plot(pmod1)
#' summary(pmod1)
# fitIndices(pmod1) #M2 limited information statistic
#'
#' #Constrain all slopes to be equal with the constrain = list() input or mirt.model() syntax
#' #first obtain parameter index
#' values <- mirt(Science,1, pars = 'values')
#' values #note that slopes are numbered 1,5,9,13, or index with values$parnum[values$name == 'a1']
#' (pmod1_equalslopes <- mirt(Science, 1, constrain = list(c(1,5,9,13))))
#' coef(pmod1_equalslopes)
#'
#' # using mirt.model syntax, constrain all item slopes to be equal
#' model <- mirt.model('
#'    F = 1-4
#'    CONSTRAIN = (1-4, a1)')
#' (pmod1_equalslopes <- mirt(Science, model))
#' coef(pmod1_equalslopes)
#'
#' coef(pmod1_equalslopes)
#' anova(pmod1_equalslopes, pmod1) #significantly worse fit with almost all criteria
#'
#' pmod2 <- mirt(Science, 2)
#' summary(pmod2)
#' plot(pmod2)
#' itemplot(pmod2, 1)
#' anova(pmod1, pmod2)
#'
#' #unidimensional fit with a generalized partial credit and nominal model
#' (gpcmod <- mirt(Science, 1, 'gpcm'))
#' coef(gpcmod)
#'
#' #for the nominal model the lowest and highest categories are assumed to be the
#' #  theoretically lowest and highest categories that related to the latent trait(s), however
#' #  a custom nominal.highlow matrix can be passed to declare which item category should be
#' #  treated as the 'highest' and 'lowest' instead
#' (nomod <- mirt(Science, 1, 'nominal'))
#' coef(nomod) #ordering of ak values suggest that the items are indeed ordinal
#' anova(gpcmod, nomod)
#' itemplot(nomod, 3)
#'
#' ###########
#' #empirical dimensionality testing that includes 'guessing'
#'
#' data(SAT12)
#' data <- key2binary(SAT12,
#'   key = c(1,4,5,2,3,1,2,1,3,1,2,4,2,1,5,3,4,4,1,4,3,3,4,1,3,5,1,3,1,5,4,5))
#'
#' mod1 <- mirt(data, 1)
#' mod2 <- mirt(data, 2)
#' #difficulty converging with reduced quadpts, reduce TOL
#' mod3 <- mirt(data, 3, TOL = .001)
#' anova(mod1,mod2)
#' anova(mod2, mod3) #negative AIC, 2 factors probably best
#'
#' #with fixed guessing parameters
#' mod1g <- mirt(data, 1, guess = .1)
#' coef(mod1g)
#'
#' ###########
#' #graded rating scale example
#'
#' #make some data
#' set.seed(1234)
#' a <- matrix(rep(1, 10))
#' d <- matrix(c(1,0.5,-.5,-1), 10, 4, byrow = TRUE)
#' c <- seq(-1, 1, length.out=10)
#' data <- simdata(a, d + c, 2000, itemtype = rep('graded',10))
#'
#' #use much better start values to save iterations
#' sv <- mirt(data, 1, itemtype = 'grsm', pars = 'values')
#' sv[,'value'] <- c(as.vector(t(cbind(a,d,c))),0,1)
#'
#' #also possible to edit start values with a GUI approach with
#' #   sv <- edit(sv)
#'
#' mod1 <- mirt(data, 1)
#' mod2 <- mirt(data, 1, itemtype = 'grsm', pars = sv)
#' coef(mod2)
#' anova(mod2, mod1) #not sig, mod2 should be preferred
#'
#' ###########
#' # 2PL nominal response model example (Suh and Bolt, 2010)
#' data(SAT12)
#' SAT12[SAT12 == 8] <- NA
#' head(SAT12)
#'
#' #correct answer key
#' key <- c(1,4,5,2,3,1,2,1,3,1,2,4,2,1,5,3,4,4,1,4,3,3,4,1,3,5,1,3,1,5,4,5)
#' scoredSAT12 <- key2binary(SAT12, key)
#' mod0 <- mirt(scoredSAT12, 1)
#'
#' #for first 5 items use 2PLNRM and nominal
#' scoredSAT12[,1:5] <- as.matrix(SAT12[,1:5])
#' mod1 <- mirt(scoredSAT12, 1, c(rep('nominal',5),rep('2PL', 27)))
#' mod2 <- mirt(scoredSAT12, 1, c(rep('2PLNRM',5),rep('2PL', 27)), key=key)
#' coef(mod0)$Item.1
#' coef(mod1)$Item.1
#' coef(mod2)$Item.1
#' itemplot(mod0, 1)
#' itemplot(mod1, 1)
#' itemplot(mod2, 1)
#'
#' #compare added information from distractors
#' Theta <- matrix(seq(-4,4,.01))
#' par(mfrow = c(2,3))
#' for(i in 1:5){
#'     info <- iteminfo(extract.item(mod0,i), Theta)
#'     info2 <- iteminfo(extract.item(mod2,i), Theta)
#'     plot(Theta, info2, type = 'l', main = paste('Information for item', i), ylab = 'Information')
#'     lines(Theta, info, col = 'red')
#' }
#'
#' #test information
#' par(mfrow = c(1,1))
#' plot(Theta, testinfo(mod2, Theta), type = 'l', main = 'Test information', ylab = 'Information')
#' lines(Theta, testinfo(mod0, Theta), col = 'red')
#'
#' ###########
#' # using the MH-RM algorithm
#' data(LSAT7)
#' fulldata <- expand.table(LSAT7)
#' (mod1 <- mirt(fulldata, 1, method = 'MHRM'))
#'
#' #Confirmatory models
#'
#' #simulate data
#' a <- matrix(c(
#' 1.5,NA,
#' 0.5,NA,
#' 1.0,NA,
#' 1.0,0.5,
#'  NA,1.5,
#'  NA,0.5,
#'  NA,1.0,
#'  NA,1.0),ncol=2,byrow=TRUE)
#'
#' d <- matrix(c(
#' -1.0,NA,NA,
#' -1.5,NA,NA,
#'  1.5,NA,NA,
#'  0.0,NA,NA,
#' 3.0,2.0,-0.5,
#' 2.5,1.0,-1,
#' 2.0,0.0,NA,
#' 1.0,NA,NA),ncol=3,byrow=TRUE)
#'
#' sigma <- diag(2)
#' sigma[1,2] <- sigma[2,1] <- .4
#' items <- c(rep('dich',4), rep('graded',3), 'dich')
#' dataset <- simdata(a,d,2000,items,sigma)
#'
#' #analyses
#' #CIFA for 2 factor crossed structure
#'
#' model.1 <- mirt.model('
#'   F1 = 1-4
#'   F2 = 4-8
#'   COV = F1*F2')
#'
#'
#' #compute model, and use parallel computation of the log-likelihood
#' mirtCluster()
#' mod1 <- mirt(dataset, model.1, method = 'MHRM')
#' coef(mod1)
#' summary(mod1)
#' residuals(mod1)
#'
#' #####
#' #bifactor
#' model.3 <- mirt.model('
#'   G = 1-8
#'   F1 = 1-4
#'   F2 = 5-8')
#'
#'
#' mod3 <- mirt(dataset,model.3, method = 'MHRM')
#' coef(mod3)
#' summary(mod3)
#' residuals(mod3)
#' anova(mod1,mod3)
#'
#' #####
#' #polynomial/combinations
#' data(SAT12)
#' data <- key2binary(SAT12,
#'                   key = c(1,4,5,2,3,1,2,1,3,1,2,4,2,1,5,3,4,4,1,4,3,3,4,1,3,5,1,3,1,5,4,5))
#'
#' model.quad <- mirt.model('
#'        F1 = 1-32
#'   (F1*F1) = 1-32')
#'
#'
#' model.combo <- mirt.model('
#'        F1 = 1-16
#'        F2 = 17-32
#'   (F1*F2) = 1-8')
#'
#'
#' (mod.quad <- mirt(data, model.quad))
#' (mod.combo <- mirt(data, model.combo))
#' anova(mod.quad, mod.combo)
#'
#' #non-linear item and test plots
#' plot(mod.quad)
#' plot(mod.combo, type = 'SE')
#' itemplot(mod.quad, 1, type = 'score')
#' itemplot(mod.combo, 2, type = 'score')
#' itemplot(mod.combo, 2, type = 'infocontour')
#'
#' ## empical histogram examples (normal, skew and bimodality)
#' #make some data
#' set.seed(1234)
#' a <- matrix(rlnorm(50, .2, .2))
#' d <- matrix(rnorm(50))
#' ThetaNormal <- matrix(rnorm(2000))
#' ThetaBimodal <- scale(matrix(c(rnorm(1000, -2), rnorm(1000,2)))) #bimodal
#' ThetaSkew <- scale(matrix(rchisq(2000, 3))) #positive skew
#' datNormal <- simdata(a, d, 2000, itemtype = 'dich', Theta=ThetaNormal)
#' datBimodal <- simdata(a, d, 2000, itemtype = 'dich', Theta=ThetaBimodal)
#' datSkew <- simdata(a, d, 2000, itemtype = 'dich', Theta=ThetaSkew)
#'
#' normal <- mirt(datNormal, 1, empiricalhist = TRUE)
#' plot(normal, type = 'empiricalhist')
#' histogram(ThetaNormal, breaks=30)
#'
#' bimodal <- mirt(datBimodal, 1, empiricalhist = TRUE)
#' plot(bimodal, type = 'empiricalhist')
#' histogram(ThetaBimodal, breaks=30)
#'
#' skew <- mirt(datSkew, 1, empiricalhist = TRUE)
#' plot(skew, type = 'empiricalhist')
#' histogram(ThetaSkew, breaks=30)
#'
#'
#' }
mirt <- function(data, model, itemtype = NULL, guess = 0, upper = 1, SE = FALSE, SE.type = 'crossprod',
                 method = 'EM', pars = NULL, constrain = NULL, parprior = NULL,
                 calcNull = TRUE, draws = 5000, rotate = 'oblimin',
                 Target = NaN, quadpts = NULL, TOL = NULL, grsm.block = NULL, rsm.block = NULL,
                 key = NULL, nominal.highlow = NULL, large = FALSE, GenRandomPars = FALSE,
                 accelerate = TRUE, empiricalhist = FALSE, verbose = TRUE, technical = list(), ...)
{
    Call <- match.call()
    mod <- ESTIMATION(data=data, model=model, group=rep('all', nrow(data)),
                      itemtype=itemtype, guess=guess, upper=upper, grsm.block=grsm.block,
                      pars=pars, method=method, constrain=constrain, SE=SE, TOL=TOL,
                      parprior=parprior, quadpts=quadpts, rotate=rotate, Target=Target,
                      rsm.block=rsm.block, technical=technical, verbose=verbose,
                      calcNull=calcNull, SE.type=SE.type, large=large, key=key,
                      nominal.highlow=nominal.highlow, accellerate=accelerate, draws=draws,
                      empiricalhist=empiricalhist, GenRandomPars=GenRandomPars, ...)
    if(is(mod, 'ExploratoryClass') || is(mod, 'ConfirmatoryClass'))
        mod@Call <- Call
    return(mod)
}
