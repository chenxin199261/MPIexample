program main
implicit none
  include "mpif.h"
  real(8),allocatable :: A(:,:)
  integer(4) :: ierr,iproc,nproc,icomm
  integer :: n,info
  integer :: inv
  integer :: T1,T2,cr,cm
  integer :: p
  real    :: rate
  integer(4) :: nthreads
  integer(4) :: tid
  integer(4) :: omp_get_num_threads
  integer(4) :: omp_get_thread_num
  integer    :: provided,ierror

!  call MPI_Init(ierr)
  call MPI_INIT_THREAD(MPI_THREAD_SERIALIZED, PROVIDED, IERROR)
  icomm = MPI_COMM_WORLD
  call MPI_COMM_SIZE(icomm,nproc,ierr)
  call MPI_COMM_RANK(icomm,iproc,ierr)
  CALL system_clock(count_rate=cr)
  CALL system_clock(count_max=cm)
  rate = REAL(cr)
  n = 6000
  allocate(A(n,n))

!$omp parallel private( tid )
  tid = omp_get_thread_num()
  nthreads = omp_get_num_threads()
  write (6,*) "MPI rank:", iproc, " with thread ID:", tid,"rank: "
!$omp end parallel

  call  RANDOM_NUMBER(A)
  do p =1,8
    CALL SYSTEM_CLOCK(T1)
    call omp_set_num_threads(p)
    call mkl_set_num_threads(p)
    call omp_set_dynamic(0)
    info = inv(A,n)
    CALL SYSTEM_CLOCK(T2)
    print *,p," core(s): ",(T2-T1)/rate
  enddo

  call MPI_FINALIZE(ierr)
end program main


function inv(A,n) result(info)
  real(8), dimension(n,n), intent(in) :: A
  real(8), allocatable:: Ainv(:,:)
  real(8), dimension(size(A,1)) :: work  ! work array for LAPACK
  integer, dimension(size(A,1)) :: ipiv   ! pivot indices
  integer :: n, info

  ! External procedures defined in LAPACK
  external DGETRF
  external DGETRI
  allocate(Ainv(n,n))
  ! Store A in Ainv to prevent it from being overwritten by LAPACK
  Ainv = A
  info = 0
  ! DGETRF computes an LU factorization of a general M-by-N matrix A
  ! using partial pivoting with row interchanges.
  call DGETRF(n, n, Ainv, n, ipiv, info)

  ! DGETRI computes the inverse of a matrix using the LU factorization
  ! computed by DGETRF.
  !call DGETRI(n, Ainv, n, ipiv, work, n, info)

  if (info /= 0) then
     stop 'Matrix inversion failed!'
  end if
end function inv
