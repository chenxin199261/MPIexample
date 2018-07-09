program shmem_mpi

   ! this is a hello world example based on code examples
   ! by Jeff Hammond and Jonathan Vincent (big thanks to them)
   ! in this example master allocates and fills an array with "twos" and then
   ! each rank prints the sum of all ements, each rank should print 200.0

   use mpi
   use, intrinsic :: iso_c_binding

   implicit none

   integer, parameter :: nsize = 100
   real(8), pointer   :: array(:)
   integer            :: num_procs,sub_procs
   integer            :: ierr,info
   integer            :: irank,comm_rank
   integer            :: win
   integer            :: comm = MPI_COMM_WORLD,newcomm
   integer            :: disp_unit
   type(c_ptr)        :: cp1
   type(c_ptr)        :: cp2
   integer(MPI_ADDRESS_KIND) :: win_size
   integer(MPI_ADDRESS_KIND) :: segment_size

   call MPI_Init(ierr)
   call MPI_Comm_size(comm, num_procs, ierr)
   call MPI_Comm_rank(comm, irank, ierr)

   disp_unit = sizeof(1.0d0)

   if (irank == 0) then
      win_size = nsize*disp_unit
   else
      win_size = 0
   endif
   call MPI_COMM_SPLIT_TYPE(comm,OMPI_COMM_TYPE_NUMA ,0, MPI_INFO_NULL,newcomm,ierr) 
   call MPI_Win_allocate_shared(win_size, disp_unit, MPI_INFO_NULL,newcomm, cp1, win, ierr)

   call MPI_Comm_size(newcomm, sub_procs, ierr)
   call MPI_Comm_rank(newcomm, comm_rank, ierr)

   print *,"world_rank: ",irank,      " in subsize   : ",sub_procs
   print *,"sub_rank:   "  ,comm_rank," in world size: ",num_procs

   call MPI_Win_free(win, ierr)
   call MPI_Finalize(ierr)

end program
