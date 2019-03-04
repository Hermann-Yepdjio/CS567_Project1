/*-----------------------------------------------------------------------------------*
 *                                 Life Inurance Project                              *
 *                               Central Washington University                        *
 *                            Graduate School of Computer Science                     *
 *                          Chao Huang, Hermann Yepdjio, Lubna Alzamil                *      
 *                                      Supervised By                                 *
 *                               Professor Donald Davendra                            *
 *                                            &                                       *
 *                               Professor Chin-Mei Chuen                             *
 *------------------------------------------------------------------------------------*/

#include<stdio.h>
#include<stdlib.h>
#include<time.h>

/*
 * This program creates a block of 10000 people who are in diffrent ages and want diffrent benefits
 */


/*
 * this function computes the net single premium for a given age and benefit
 * 
 * @params
 * A_x: A_x correspond to a given age in the life table
 * rBenefit: random number between 1000 and 10000 to represent the benefit 
 */
double BussinessBlock(double A_x, int rBenefit)
{
	return A_x * rBenefit;	
}


/*
 * ---------------------------------------- looping 10000 -------------------------------------
 * 
 * @params
 * table: vector containains all the values of the life table
 * dim: contains the number of rows and columns in the life table at indexes 0 and 1 respectively and the number of rows in the lifeTableAges at index 2
 * bAge: lempty list to be filled with randomly generated ages
 * bBen: empty list to be filled with randomly generated benefits
 * bNps: empty list to be filled with computed net single premiums
 * lifeTableAges; list of ages
 * inputNumberClients: an integer value representing the number of clients
 * 
 */

void for_loop(double *table, int* dim, int* bAge, int* bBen, double* bNps, int* bFAge, int* lifeTableAges, int* inputNumberClients)
{
  srand(clock()); // set time as the seed for the random generator
  int i;
  double lifeTable[dim[0]][dim[1]]; // create a 2 * 2 matrix to be filled with the values from the life table
  int j = 0;
  int k = 0;
  
  //reconstruct the lifeTable as a 2D matrix for easy reading
  for (i = 0; i < dim[0] * dim[1]; i ++)
  {
    lifeTable[j][k]  = table[i];
    k ++;
    if(k == dim[1])
    {
      j ++;
      k = 0;
    }
  }

 
  
  for(i = 0; i < *inputNumberClients; i++)
  {
 	    int randomAge = lifeTableAges[rand() % dim[2]];  // randomly pick an age from the lifeTableAges vector
      int randomBenefit = rand()%(10000 + 1 - 1000) + 1000;  //randomly pick a number between 1000 and 10000
      
    	bAge[i] = randomAge; // fill the bAge vector with random ages
    	bBen[i] = randomBenefit; // fill the bBenefit array with random benefits
    
    	bNps[i] = BussinessBlock(lifeTable[randomAge][dim[1]-1], randomBenefit); 	//compute net single premium for a given age and a given benefit
    
    	double pLives[dim[0] - randomAge]; // list of survive years probabilities
    	int pLivesAges[dim[0]- randomAge]; // list of ages for the survive year probabilities
    	if(randomAge > 0) //compute the values for the 2nd up to the last row of pLives and pLivesAges
    	{
    		int tmp=randomAge;
    		int count = 0;
    		for (j = randomAge; j<dim[0]; j++)
    		{
    			pLives[count] = lifeTable[j][dim[1]-4]/lifeTable[tmp - 1][dim[1] - 4];
    			pLivesAges[count] = lifeTable[j][0];
    			count++;
    		}	
    	}
    	else //compute the values for the first row of pLives and pLivesAges
    	{
    		int count = 0;
    		for (j = randomAge; j<dim[0]; j++)
    		{
    			pLives[count] = lifeTable[j][dim[1]-4];
    			pLivesAges[count] = lifeTable[j][0];
    			count++;
    		}		
    	}
    
    
    	//calculate probability t_1_q_x that x survives t years and dies within 1 year
    	int pCurveXRow = sizeof(pLivesAges)/sizeof(int);
    	
    	double tl1_q_x[pCurveXRow];
    	//probability of death based on input age
    	tl1_q_x[0] = 1 - pLives[0]; //first data important .... to avoid bug
    	if(pCurveXRow > 1 ) //avoid enter for loop when pCurveXRow ==0. This occur when randomAge is the last value of the tableLife
    	{
    		for(j = 1; j < pCurveXRow; j++)
    			tl1_q_x[j] = pLives[j-1] - pLives[j]; // i = u in the pdf 1 = t in pdf
    	}
    
    	//generate random dies based on input age
    	int finalAge = 0;
    	if(pCurveXRow > 1) //select a die age randomly based on the probability table
    	{
    		int array_size=0;
    		for(j = 0; j < pCurveXRow; j++)
    		{
    			array_size += 1000 * tl1_q_x[j];
    		}
    		
    		double array[array_size];
    		int count = 0;
    
    		for(j = 0; j < pCurveXRow; j++) 
    		{
    			int k;
    				for(k = 0; k < 1000 * tl1_q_x[j]; k++)
    			{
    				array[count] = pLivesAges[j];
    				count++;
    			}
    		}
    
    		finalAge = array[rand()%array_size];
    	}
    	else
    	{
    		finalAge = pLivesAges[0]; //only has 1 probability
    	}
    	bFAge[i] = finalAge;
    
    	
    //------------end for loop
  }
   
   
  
}

