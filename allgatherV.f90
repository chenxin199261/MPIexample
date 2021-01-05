  MODULE PARALLEL
      use mpi
    INTEGER*4                     :: iproc, nproc, rank, ierr
    INTEGER*4                     :: mylow, myhigh, mysize, ichunk, irem
    DOUBLE PRECISION, ALLOCATABLE :: glob_val(:,:)
    INTEGER*4                     :: newtype
    integer*4                      ::  blockcounts(2),types(2)
    integer(KIND=MPI_ADDRESS_KIND) ::  offsets(2),displs(2)
    integer,ALLOCATABLE            :: isize(:),ilow(:),ihigh(:),idisp(:)

  END MODULE

  PROGRAM MAIN
    USE PARALLEL
    use mpi
    IMPLICIT NONE
    type atom
      integer*4 :: iatom
      real(8)   :: coord(3),charge(3)
    end type atom
  
    integer*4 ::  i, j
    integer*4 ::  num_rows,num_cols
    integer*4 ::  used_rows
    type(atom) :: atoms(3),global_atom(6)
    real(8)    :: A(100,100) 
!    ----setup MPI----
    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
    iproc = rank+1  !rank is base 0, rest of fortran base 1

!   ----setup initial data      
    ALLOCATE (isize(nproc))
    ALLOCATE (idisp(nproc))

    blockcounts(1) = 1
    blockcounts(2) = 6

    Types(1) = MPI_INTEGER
    Types(2) = MPI_DOUBLE_PRECISION
    A = 0
    CALL MPI_GET_ADDRESS(atoms(1)%iatom, displs(1), ierr) 
    CALL MPI_GET_ADDRESS(atoms(1)%coord(1), displs(2),ierr)
    Offsets = displs-displs(1)

    do i=1,nproc  
      isize(i)= i
      idisp(i)= i-1
    end do
    idisp(1) = 0
    do i=2,nproc 
       idisp(i)= idisp(i-1) + isize(i-1)
    end do
    print *,"isize,idisp: ",isize,idisp
    do i=1,isize(rank+1)
       atoms(i)%iatom = rank+i+1
       atoms(i)%coord(1:3) = (rank+1)*1D0*i
       atoms(i)%charge(1:3) = (rank+1)*1D0*i
    enddo

    CALL MPI_TYPE_CREATE_STRUCT(2,blockcounts,Offsets, Types, newtype, IErr)
    call MPI_TYPE_COMMIT(newtype,ierr)

    write(*,*) rank, atoms
    write(*,*) "============================="

    call MPI_ALLGATHERV(atoms,isize(rank+1),newtype, &
                     global_atom,isize,idisp,newtype,&
                     MPI_COMM_WORLD,ierr)      
    write(*,*) rank,global_atom

    call MPI_Finalize(ierr)
  END program
