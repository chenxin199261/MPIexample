program test

use mpi

integer :: n,m
integer :: ierr
integer :: num_procs
integer :: irank
integer :: comm = MPI_COMM_WORLD
integer(KIND=MPI_ADDRESS_KIND) :: iadd1,iadd2

double precision, allocatable, asynchronous :: bufs(:,:)

call MPI_Init(ierr)
call MPI_Comm_size(comm, num_procs, ierr)
call MPI_Comm_rank(comm, irank, ierr)

print *,num_procs

n=10
m=30
allocate(bufs(n,m))

call mpi_get_address(bufs(1,1),iadd1,ierr)
call mpi_get_address(bufs(9,29),iadd2,ierr)
print *,iadd2-iadd1
deallocate(bufs)
call MPI_Finalize(ierr)

end program test
