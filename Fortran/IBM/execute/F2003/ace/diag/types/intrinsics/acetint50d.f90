!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint50d
!*
!*  DATE                       : 2006-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : incompatible AC in SOURCE of ALLOCATE
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : source, allocate, AC
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Try to allocate several intrinsic arrays with an AC as the source, but
!*  using mismatching type parameters or types.  Scatter a few implied-do's, too.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint50d

  implicit none
  integer (2), allocatable      :: iarr(:)
  real    (4), allocatable      :: rarr(:)
  logical, allocatable          :: larr(:)
  character(3), allocatable     :: carr(:)
  complex (4), allocatable      :: zarr(:)
  double complex, allocatable   :: dzarr(:)
  double precision, allocatable :: dparr(:)

  integer :: i
  common /zort/ i

  allocate (iarr(3), source=[integer(1):: 32767, 0, -20000])
  deallocate(iarr)
  allocate (iarr(3), source=[integer(2):: 32767, 0, -20000])
  deallocate(iarr)
  allocate (iarr(3), source=[integer(4):: (32767, 0, -20000, i=1,1)])
  deallocate(iarr)
  allocate (iarr(3), source=[integer(8):: 32767, 0, -20000])
  deallocate(iarr)
  allocate (iarr(3), source=[integer   :: 32767, 0, -20000])
  deallocate(iarr)
  allocate (iarr(0), source=[integer(1)::])

  allocate (rarr(3), source=[real(4) :: 3.2767, -0.0, 9.87654321e32])
  deallocate (rarr)
  allocate (rarr(3), source=[real(8) :: (3.2767, -0.0, 9.87654321e32, i=1,1)])
  deallocate (rarr)
  allocate (rarr(3), source=[real(16):: 3.2767, -0.0, 9.87654321e32])
  deallocate (rarr)
  allocate (rarr(3), source=[real    :: 3.2767, -0.0, 9.87654321e32])
  deallocate (rarr)
  allocate (rarr(0), source=[real(8) ::])

  allocate (larr(2), source=[logical(1):: .true., .false.])
  deallocate (larr)
  allocate (larr(2), source=[logical(2):: (.true., .false., i=1,1)])
  deallocate (larr)
  allocate (larr(2), source=[logical(4):: .true., .false.])
  deallocate (larr)
  allocate (larr(2), source=[logical(8):: (.true., .false., i=1,1)])
  deallocate (larr)
  allocate (larr(2), source=[logical   :: .true., .false.])
  deallocate (larr)
  allocate (larr(0), source=[logical(1)::])

  allocate (carr(3), source=[character(3):: 'abc', 'de', 'fghi'])
  deallocate (carr)
  allocate (carr(3), source=[character(2):: 'abc', 'de', 'fghi'])
  deallocate (carr)
  allocate (carr(3), source=[character(2):: ('abc', 'de', 'fghi', i=1,1)])
  deallocate (carr)
  allocate (carr(3), source=[character   :: 'abc', 'de', 'fghi'])
  deallocate (carr)
  allocate (carr(0), source=[character(2)::])

  allocate (zarr(1), source=[complex(4) :: (3.2767,-0.0)])
  deallocate (zarr)
  allocate (zarr(1), source=[complex(8) :: ((3.2767,-0.0), i=1,1)])
  deallocate (zarr)
  allocate (zarr(1), source=[complex(16):: (3.2767,-0.0)])
  deallocate (zarr)
  allocate (zarr(1), source=[complex    :: (3.2767,-0.0)])
  deallocate (zarr)

end program acetint50d
