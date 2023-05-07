/* Eratosthenes Sieve Prime Number Program in C */
/* Uses local (automatic) Variables */

/*#include <stdio.h>*/
#define TRUE 1
#define FALSE 0

#define SIZE    8190
#define SIZEP1  8191

char flags[SIZEP1];
main()  {
        int i, prime, k, count, iter;

          /* variables defined here are local */

        printf("10 iterations :");
        for (iter = 1; iter<=10; iter ++) {
                count = 0;
                for (i = 0; i<= SIZE; i++) 
                        flags[i] = TRUE;
                for (i = 0; i<= SIZE; i++) {
                        if (flags[i]) {
                                prime = i + i + 3;
                                k = i + prime;
                                while (k <= SIZE) {
                                        flags [k] = FALSE;
                                        k += prime;
                                        }
                                count = count + 1;
                                }
                        }
                }
printf(" %d primes", count);
}

                        
                                                           
