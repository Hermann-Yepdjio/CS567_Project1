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

void for_loop_2(double* lifeTable_A_x, int* dim2, int* bdfAge, int* bdfSurviveYears, double* bdfBenefit, int* SurviveYears, double* Reserve, int* NumberPolicies) 
{
  /*
   * This function computes the reserve amount, and generate the survive years and number of policies for each reserve year in order to create a reserve table
   * 
   * @params
   * lifeTable_A_x: contains the A_x values of the life table
   * dim: contains the number of rows of bDataFrame at index 0 and the number of years at index 1
   * bdfAge: list of ages from bDataFrame
   * bdfSurviveYears: list of survive years from bDataFrame
   * bdfBenefits: list of benefits from bDataFrame
   * SurviveYears: empty list to be filled with SurviveYears for each reserve year
   * Reserve: empty list to be filled with Reserve amount for each reserve year
   * NumberPolicies: empty list to be filled with number of policies for each reserve year
   * 
   */
  
  int i, reserveYear; 
  int nYears = dim2[1];
  int nrowbDataFrame = dim2[0];
  
  //This loop computes the survive years, number of policies and reserve amount for each reserve year
  for (reserveYear = 0; reserveYear < nYears; reserveYear++)
  {
    int counter = 0;
    double reserveAmount = 0;
    
    //this for loop this loop computes the reserve amount for a specific reserve year
    for (i = 0; i < nrowbDataFrame; i++)
    {
      if (bdfSurviveYears[i] >= reserveYear + 1)
      {
        counter++;
        int x = bdfAge[i];
        double A_x_t = lifeTable_A_x[(x + 1 + reserveYear)]; //Faster
        double t_V = A_x_t;
        reserveAmount += bdfBenefit[i] * t_V;
      }
    }
    SurviveYears[reserveYear] = reserveYear; // record the survive years foe each reserve year
    NumberPolicies[reserveYear] = counter; // record the number of policies for each reserve year
    Reserve[reserveYear] = reserveAmount; // record the reserve amount for each reserve year
    
  }
  
}