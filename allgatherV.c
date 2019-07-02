#include <mpi.h>
#include <stdio.h>


int main(int argc, char **argv)
{
    const int localSize = 8, nProcess = 8, globalSize = 36;
    int globalData[globalSize], localData[localSize], count[nProcess], disp[nProcess];
    int comRank, comSize, i;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &comRank);
    MPI_Comm_size(MPI_COMM_WORLD, &comSize);    

    if (comRank == 0)            
        for (i = 0; i < globalSize; globalData[i] = i, i++);
    else
        for (i = 0; i < globalSize; globalData[i] = 0, i++);
    for (i = 0; i < localSize; localData[i++] = -1);
    for (i = 0; i < comSize; count[i] = i + 1, i++);
    for (disp[0] = 0, i = 1; i < comSize; disp[i] = disp[i-1] + count[i-1], i++);

    MPI_Scatterv(globalData, count, disp, MPI_INT, localData, count[comRank], MPI_INT, 0, MPI_COMM_WORLD);
    for (i = 0; i < count[comRank]; i++)
        localData[i] += comRank;
    MPI_Allgatherv(localData, count[comRank], MPI_INT, globalData, count, disp, MPI_INT, MPI_COMM_WORLD); 

    for (i = 0; i < globalSize; i++)
        printf("%d ", globalData[i]);
    
    
    printf("\n");

    MPI_Finalize();
    return 0;
}
