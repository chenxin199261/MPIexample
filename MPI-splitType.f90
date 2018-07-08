program test_MPI_RMA                                          
implicit none
include 'mpif.h'
integer, parameter :: m=10
INTEGER i,map(m),comm,ierr,nproc,rank,info, NEWCOMM
REAL*8 A(m),B(m)
                                                              
call MPI_INIT(ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

PRINT*,"nproc=",nproc,"rank=",rank
call MPI_COMM_SPLIT_TYPE(MPI_COMM_WORLD,MPI_COMM_TYPE_SHARED , 0, info, NEWCOMM, ierr)
call dostupid(rank)
call MPI_BARRIER(MPI_COMM_WORLD, ierr)
call MPI_FINALIZE(ierr)


end program test_MPI_RMA

subroutine dostupid(rank)
integer  ::rank

print *,rank
end subroutine

