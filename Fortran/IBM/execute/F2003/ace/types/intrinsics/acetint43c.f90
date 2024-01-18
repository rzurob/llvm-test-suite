!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint43c
!*
!*  DATE                       : 2006-11-08
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : lookalike function invocations as ac_values
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Like acetint43, we invoke functions with names identical to intrinsic types
!*  and intrinsic functions in ACs.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint43c

  implicit none

  integer(4)     :: iarr(1), iar2(1)
  real(4)        :: rarr(1)
  complex(4)     :: zarr(1), zar2(1)
  logical(4)     :: larr(1)
  character      :: carr(4), car2(4)
  character(3)   :: c3arr(1)

  logical(4)     :: precision_r4, precision_x8

  real(4), parameter :: PI = 3.14152654

  integer(4)     :: i

  print *, [integer(4)],   [integer(4)::   integer(4)],   [integer(4)::   (integer(4),integer(i), i=4,4)]
  print *, [int(1)],       [integer(4)::   int(1)],       [integer(4)::   (int(1),int(i), i=1,1)]
  print *, [real(4)],      [real(4)::      real(4)],      [real(4)::      (real(4),real(i), i=4,4)]
  print *, [complex(4)],   [complex(4)::   complex(4)],   [complex(4)::   (complex(4),complex(i), i=4,4)]
  print *, [cmplx(6.,7.)], [complex(4)::   cmplx(6.,7.)], [complex(4)::   (cmplx(6.,7.),cmplx(real(i),real(i)), i=1,1)]
  print *, [logical(4)],   [logical(4)::   logical(4)],   [logical(4)::   (logical(4),logical(i), i=4,4)]
  print *, [char(65)],     [character(3):: char(65)],     [character(3):: (char(65),char(i), i=65,65)]

  print *, [character(1,1), character(1,1), character(-1,1), character(-1,-1), character(1,-1)],&
           [character(1,1):: character(1,1), character(-1,1), character(-1,-1), character(1,-1)],&
           [character(1,1):: (character(1,1), character(-1,1), character(-1,-1), character(1,-1), i=1,1)],&
           [character(1,1):: (character(i,i), character(-i,i), character(-i,-i), character(i,-i), i=1,1)]
  print *, [character(len=1,kind=1):: character(len=1,kind=1), character(len=-1,kind=1), character(len=-1,kind=-1), character(len=1,kind=-1)], &
           [character(len=1,kind=1):: (character(len=1,kind=1), character(len=-1,kind=1), character(len=-1,kind=-1), character(len=1,kind=-1), i=1,1)], &
           [character(len=1,kind=1):: (character(len=i,kind=i), character(len=-i,kind=i), character(len=-i,kind=-i), character(len=i,kind=-i), i=1,1)]

  iarr  = [integer(4)::   integer(4)]
  iar2  = [integer(4)::   int(1)]
  rarr  = [real(4)::      real(4)]
  zarr  = [complex(4)::   complex(4)]
  zar2  = [complex(4)::   cmplx(6.,7.)]
  larr  = [logical(4)::   logical(4)]
  c3arr = [character(3):: char(65)]
  carr  = [character(1,1):: character(1,1), character(-1,1), character(-1,-1), character(1,-1)]
  car2  = [character(len=1,kind=1):: character(len=1,kind=1), character(len=-1,kind=1), character(len=-1,kind=-1), character(len=1,kind=-1)]

  if (iarr(1) /= 16) stop 2
  if (iar2(1) /= 10) stop 3
  if (.not. precision_r4(rarr(1),sin(PI/4) ** 2)) stop 4
  if (.not. precision_x8(zarr(1),(sin(PI/4) ** 2, cos(PI/4) ** 2))) stop 5
  if (.not. precision_x8(zar2(1),(sin(6.) ** 2, cos(7.) ** 2))) stop 6
  if (.not. larr(1)) stop 7
  if (c3arr(1) /= 'ABD') stop 8
  if (any(carr /= ['a', 'b', 'd', 'c'])) stop 9
  if (any(car2 /= carr)) stop 10

  iarr  = [integer(4)::   (integer(4), i=1,1)]
  iar2  = [integer(4)::   (int(1), i=1,1)]
  rarr  = [real(4)::      (real(4), i=1,1)]
  zarr  = [complex(4)::   (complex(4), i=1,1)]
  zar2  = [complex(4)::   (cmplx(6.,7.), i=1,1)]
  larr  = [logical(4)::   (logical(4), i=1,1)]
  c3arr = [character(3):: (char(65), i=1,1)]
  carr  = [character(1,1):: (character(1,1), character(-1,1), character(-1,-1), character(1,-1), i=1,1)]
  car2  = [character(len=1,kind=1):: (character(len=1,kind=1), character(len=-1,kind=1), character(len=-1,kind=-1), character(len=1,kind=-1), i=1,1)]

  if (iarr(1) /= 16) stop 11
  if (iar2(1) /= 10) stop 12
  if (.not. precision_r4(rarr(1),sin(PI/4) ** 2)) stop 13
  if (.not. precision_x8(zarr(1),(sin(PI/4) ** 2, cos(PI/4) ** 2))) stop 14
  if (.not. precision_x8(zar2(1),(sin(6.) ** 2, cos(7.) ** 2))) stop 15
  if (.not. larr(1)) stop 16
  if (c3arr(1) /= 'ABD') stop 17
  if (any(carr /= ['a', 'b', 'd', 'c'])) stop 18
  if (any(car2 /= carr)) stop 19

  iarr  = [integer(4)::   (integer(i), i=4,4)]
  iar2  = [integer(4)::   (int(i), i=1,1)]
  rarr  = [real(4)::      (real(i), i=4,4)]
  zarr  = [complex(4)::   (complex(i), i=4,4)]
  zar2  = [complex(4)::   (cmplx(real(i),real(i)), i=1,1)]
  larr  = [logical(4)::   (logical(i), i=4,4)]
  c3arr = [character(3):: (char(i), i=65,65)]
  carr  = [character(1,1):: (character(i,i), character(-i,i), character(-i,-i), character(i,-i), i=1,1)]
  car2  = [character(len=1,kind=1):: (character(len=i,kind=i), character(len=-i,kind=i), character(len=-i,kind=-i), character(len=i,kind=-i), i=1,1)]

  if (iarr(1) /= 16) stop 20
  if (iar2(1) /= 10) stop 21
  if (.not. precision_r4(rarr(1),sin(PI/4) ** 2)) stop 22
  if (.not. precision_x8(zarr(1),(sin(PI/4) ** 2, cos(PI/4) ** 2))) stop 23
  if (.not. precision_x8(zar2(1),(sin(sin(PI)**2)**2, cos(sin(PI)**2)**2))) stop 24
  if (.not. larr(1)) stop 25
  if (c3arr(1) /= 'ABD') stop 26
  if (any(carr /= ['a', 'b', 'd', 'c'])) stop 27
  if (any(car2 /= carr)) stop 28

contains

  integer(4) function integer(i)
    integer(4) :: i
    integer = i ** 2
  end function integer

  integer(4) function int(i)
    integer(4) :: i
    int = i * 10
  end function int

  real(4) function real(i)
    integer(4) :: i
    real = (sin(PI/i) ** 2)
  end function real

  complex(4) function complex(i)
    integer(4) :: i
    complex = (sin(PI/i) ** 2, cos(PI/i) ** 2)
  end function complex

  complex(4) function cmplx(r1,r2)
    real(4) :: r1, r2
    cmplx = (sin(r1) ** 2, cos(r2) ** 2)
  end function cmplx

  logical(4) function logical(i)
    integer(4) :: i
    logical = i>0
  end function logical

  character(3) function char(i)
    integer(4) :: i
    char = achar(i) // achar(i+1) // achar(i+3)
  end function char

  character function character(len,kind)
    integer(4) :: len, kind
    character = merge(merge('a','b',len>=0),merge('c','d',len>=0),kind>=0)
  end function character

end program acetint43c
