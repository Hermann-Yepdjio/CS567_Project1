#include<stdio.h>
#include<stdlib.h>
#include<time.h>

double BussinessBlock(double A_x, int rBenefit)
{
	return A_x * rBenefit;	
}

void test(double *a, double *b, double *c)
{
  c[0] = a[0]*b[0];
  
}

void for_loop(double *table, int* dim, int* bAge, int* bBen, double* bNps, int* bFAge, int* lifeTableAges, int* inputNumberClients)
{
  srand(clock());
  int i;
  double lifeTable[dim[0]][dim[1]];
  int j = 0;
  int k = 0;
  
  //reconstruct the lifeTable as a 2D Array for easy reading
  for (i=0; i< dim[0]*dim[1]; i++)
  {
    lifeTable[j][k]  = table[i];
	//printf("%i\n", lifeTableAges_size);
	//printf("%i\n", lifeTableAges_size);
    k++;
    if(k == dim[1])
    {
      j++;
      k = 0;
    }
  }

 
  
  for(i = 0; i < *inputNumberClients; i++)
  {
 	    int randomAge = lifeTableAges[rand()%dim[2]];  
 	    //int randomAge = 0; 
      int randomBenefit = rand()%(10000 + 1 - 1000) + 1000;  //randomly pick a number between 1000 and 10000
      //int randomBenefit = 100;
      
	bAge[i] = randomAge;
	bBen[i] = randomBenefit;

	bNps[i] = BussinessBlock(lifeTable[randomAge][dim[1]-1], randomBenefit); 	

	double pLives[dim[0]-randomAge];
	int pLivesAges[dim[0]- randomAge];
	if(randomAge > 0)
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
	else
	{
		//int tmp=randomAge;
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
int main()
{
  return 0;
}
