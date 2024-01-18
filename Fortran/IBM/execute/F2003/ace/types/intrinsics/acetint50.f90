!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint50
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-11-16
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : AC in SOURCE of ALLOCATE
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : source, allocate, AC
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Allocate several intrinsic arrays with an AC as the source, printing to
!*  verify the correctness of the content.  Repeat with implied-do's.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


program acetint50

  use, intrinsic :: ieee_arithmetic
  implicit none

  integer (2), allocatable      :: iarr(:)
  real    (4), allocatable      :: rarr(:), rarx(:)
  logical (1), allocatable      :: larr(:)
  character(3), allocatable     :: carr(:)
  character(:), allocatable     :: c2arr(:)
  complex (4), allocatable      :: zarr(:), zarx(:)
  double complex, allocatable   :: dzarr(:), dzarx(:)
  double precision, allocatable :: dparr(:), dparx(:)

  integer :: i
  common /zort/ i

  allocate (iarr(3), source=[integer(2):: 32767, 0, 2000000000])
  print *, iarr
  deallocate (iarr)
  allocate (iarr(3), source=[integer(2):: (32767, 0, 2000000000, i=1,1)])
  print *, iarr

  rarx = [real(4):: 0.0]
  allocate (rarr(5), source=[real(4):: 3.2767, -0.0, 9.87654321e32, &
                                       ieee_value(rarx(1),IEEE_POSITIVE_INF), &
                                       ieee_value(rarx(1),IEEE_QUIET_NAN)])
  print *, rarr
  deallocate (rarr)
  allocate (rarr(5), source=[real(4):: (3.2767, -0.0, 9.87654321e32, &
                                       ieee_value(rarx(1),IEEE_POSITIVE_INF), &
                                       ieee_value(rarx(1),IEEE_QUIET_NAN), i=1,1)])
  print *, rarr

  allocate (larr(2), source=[logical(1):: .true., .false.])
  print *, larr
  deallocate (larr)
  allocate (larr(2), source=[logical(1):: (.true., .false., i=1,1)])
  print *, larr

  allocate (carr(3), source=[character(3):: 'abc', 'de', 'fghi'])
  print *, carr
  deallocate (carr)
  allocate (carr(3), source=[character(3):: ('abc', 'de', 'fghi', i=1,1)])
  print *, carr


  allocate (c2arr(3), source=[character(2):: ('abc', 'de', 'fghi', i=1,1)])
  print *, c2arr
  deallocate (c2arr)
  i = 4
  allocate (c2arr(5), source=[character(i):: 'abc', 'defgh', 'ijkl', 'mnopqr', 'stuv'])
  print *, c2arr

  zarx = [complex(4):: (0.0,0.0)]
  allocate (zarr(3), source=[complex(4):: (3.2767,-0.0), &
                              (9.87654321e32, ieee_value(real(zarx(1)),IEEE_POSITIVE_INF)), &
                              (ieee_value(aimag(zarx(1)),IEEE_QUIET_NAN), ieee_value(aimag(zarx(1)),IEEE_POSITIVE_NORMAL))])
  print *, zarr
  deallocate (zarr)
  allocate (zarr(3), source=[complex(4):: ((3.2767,-0.0), &
                              (9.87654321e32, ieee_value(real(zarx(1)),IEEE_POSITIVE_INF)), &
                              (ieee_value(aimag(zarx(1)),IEEE_QUIET_NAN), ieee_value(aimag(zarx(1)),IEEE_POSITIVE_NORMAL)), i=1,1)])
  print *, zarr

  dzarx = [double complex:: (0.0d0,0.0d0)]
  allocate (dzarr(3), source=[double complex:: (3.2767d300,-0.0d200), &
                               (-9.87654321d32, ieee_value(real(dzarx(1)),IEEE_NEGATIVE_INF)), &
                               (ieee_value(aimag(dzarx(1)),IEEE_QUIET_NAN), ieee_value(aimag(dzarx(1)),IEEE_NEGATIVE_NORMAL))])
  print *, dzarr
  deallocate (dzarr)
  allocate (dzarr(3), source=[double complex:: ((3.2767d300,-0.0d200), &
                               (-9.87654321d32, ieee_value(real(dzarx(1)),IEEE_NEGATIVE_INF)), &
                               (ieee_value(aimag(dzarx(1)),IEEE_QUIET_NAN), ieee_value(aimag(dzarx(1)),IEEE_NEGATIVE_NORMAL)), i=1,1)])
  print *, dzarr

  dparx = [double precision:: 0.0d0]
  allocate (dparr(3), source=[double precision:: ieee_value(real(dparx(1)),IEEE_NEGATIVE_INF), 0.0d300, 1d-9])
  print *, dparr
  deallocate (dparr)
  allocate (dparr(3), source=[double precision:: (ieee_value(real(dparx(1)),IEEE_NEGATIVE_INF), 0.0d300, 1d-9, i=1,1)])
  print *, dparr

  ! Try a couple of empty arrays:
  deallocate (iarr)
  allocate (iarr(0), source=[integer(2):: ])
  print *, iarr

  deallocate (iarr)
  allocate (iarr(0), source=[integer(2):: (i, i=1,0)])
  print *, iarr

  deallocate (zarr)
  allocate (zarr(0), source=[complex(4):: ((real(i),real(i)), i=1,0)])
  print *, zarr

end program acetint50
