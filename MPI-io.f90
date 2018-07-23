program shmem_mpi

   use mpi

   implicit none

   integer, parameter :: nsize = 100
   integer            :: num_procs,sub_procs
   integer            :: ierr,info
   integer            :: irank,comm_rank
   character(40)      :: chare
   integer            :: comm = MPI_COMM_WORLD,newcomm

   call MPI_Init(ierr)
   call MPI_Comm_size(comm, num_procs, ierr)
   call MPI_Comm_rank(comm, irank, ierr)

   open (unit = 2223, file = "testIO.txt")
   read (2223,*) chare
   print *,chare
   print *,"world_rank: ",irank,      " in subsize   : ",sub_procs
   print *,"sub_rank:   "  ,comm_rank," in world size: ",num_procs

   call MPI_Finalize(ierr)

end program
