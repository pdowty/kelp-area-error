/******************************************************************************
 *
 * rand_weibull)
 *
 * This routine returns a random number drawn from a Weibull distribution.
 *
 * This uses the acceptance-rejection method of von Neumann 1951.
 * see Mooney, 1997, Monte Carlo Simulation.
 *
 * March 2013 Pete Dowty
 *
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "definitions.h"
#include "routines.h"
#define PI 3.14159265

double rand_weibull(double k, double lambda, double max, double min) 
{

   double x,xm,y,p;
   double factor1, factor2; 

   /** enter do loop & calculate variates until one is accepted **/
   do {

       /** get uniformly distributed real random variate on interval (0,max] **/
       /** genrand_real1() returns uniform random real variate on [0,1] **/
       x = genrand_real1();
       xm = x * max;
       if (xm < min)
          xm = min;

       /** evaluate lognormal probability density function at x **/
       factor1 = k / lambda;
       factor2 = xm / lambda; 
       y = factor1 * pow(factor2,k-1) * exp(-pow(factor2,k)); 
 
       /** get uniformly distributed real random variate on interval [0,1] **/
       p = genrand_real1();

   } while (y<p);  /** reject & get new number if y<p **/


   return (xm);
}
