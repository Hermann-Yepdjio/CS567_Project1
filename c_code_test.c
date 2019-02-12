#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<time.h>

double BussinessBlock(double A_x, int rBenefit)
{
	return A_x * rBenefit;	
}



void for_loop(double *table, int* dim, double* bAge, int* bBen, double* bNps, double* bFAge, int* lifeTableAges, int* inputNumberClients)
{
  //srand(clock());
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

  
  
	int inputAge = dim[3];
 	int randomAge = lifeTableAges[rand()%dim[2]];  
        int randomBenefit = rand()%(10000 + 1 - 1000) + 1000;  //randomly pick a number between 1000 and 10000
	bAge[i] = randomAge;
	bBen[i] = randomBenefit;
	bNps[i] = BussinessBlock(lifeTable[randomAge][dim[1]-1], randomBenefit); 	

	double pLives[dim[0]-randomAge];
	int pLivesAges[dim[0]- randomAge];
	if(inputAge > 0)
	{
		int tmp=inputAge;
		int count = 0;
		for (j = inputAge; j<dim[0]; j++)
		{
			pLives[count] = lifeTable[j][dim[1]-4]/lifeTable[tmp - 1][dim[1] - 4];
			pLivesAges[count] = lifeTable[j][0];
			count++;
		}	
	}
	else
	{
		int tmp=inputAge;
		int count = 0;
		for (j = inputAge; j<dim[0]; j++)
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
	for(j = 1; j < pCurveXRow; j++)
		tl1_q_x[j] = pLives[j-1] - pLives[j]; // i = u in the pdf 1 = t in pdf
	double perfectData[*inputNumberClients];
	int bIndex = 0; //begin index
	int fIndex = 0; //final index
	int counter = 0;

	for (k = 0; k<pCurveXRow; k++)
	{
		double nRepeat = tl1_q_x[k] * *inputNumberClients;
		if(fmod(nRepeat, 1.0) >= 0.5) //to avoid number of data > pCurveXRow
		{
			if(counter % 2 == 0)
				nRepeat = round(nRepeat);
			else
				nRepeat = trunc(nRepeat);
			counter++;
		}
		else
			nRepeat = trunc(nRepeat);

		fIndex = bIndex + nRepeat - 1;
		int l;
		for(l=bIndex; l<fIndex; l++)
			perfectData[l] <- pLivesAges[k];
		bIndex = bIndex + nRepeat;
	}

	int nFill = *inputNumberClients - fIndex;
	
	//fill residual round values from nData to numberclient

	//generate random dies based on input age
	int finalAge = 0;
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
	for (j = fIndex +1 ; j < *inputNumberClients; j++)
	{
		perfectData[j] = array[rand()%array_size];
	}
	bFAge = perfectData;

  
  
}
void main()
{
}
