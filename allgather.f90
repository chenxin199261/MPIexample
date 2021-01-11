!
!
!
!
program allgather
  include "mpif.h"
  integer*4,allocatable :: num_send(:)
  integer*4,allocatable :: num_recv(:) 
  integer*4 :: rank,nprocs,ierr
    
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)

  allocate(num_send(1))
  num_send(1) = rank+1
  print *,num_send
  allocate(num_recv(nprocs))
  num_recv = 0


  call MPI_ALLGATHER(num_send, 1, MPI_INTEGER, num_recv, 1, &
        MPI_INTEGER, MPI_COMM_WORLD, ierr)
  print *,num_recv
  deallocate(num_recv)

  call MPI_FINALIZE(ierr)
end
