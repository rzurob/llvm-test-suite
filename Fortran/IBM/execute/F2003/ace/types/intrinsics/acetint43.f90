!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint43
!*
!*  DATE                       : 2006-11-02
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : statement function invocations as ac_values
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : statement function
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Use different statement functions in AC's, verifying the values.
!*  The names for the statement functions are chosen in part to overlap with
!*  intrinsics and type names.
!*  We print and assign the values, to test two different types of use.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint43

  implicit none

  integer(4)     :: integer,   iarr(1), iar2(1), int
  real(4)        :: real,      rarr(1), a, b
  complex(4)     :: complex,   zarr(1), zar2(1), cmplx
  logical(4)     :: logical,   larr(1)
  character      :: character, carr(4), car2(4)
  character(3)   :: char,      c3arr(1)

  logical(4)     :: precision_r4, precision_x8

  real(4), parameter :: PI = 3.14152654

  integer(4)     :: i

  ! The statement functions:
  integer(i) = i ** 2
  int(i) = i * 10
  real(i) = (sin(PI/i) ** 2)
  complex(i) = (sin(PI/i) ** 2, cos(PI/i) ** 2)
  cmplx(a,b) = (sin(a) ** 2, cos(b) ** 2)
  logical(i) = i>0
  character(i) = merge('L','G',i<0)
  char(i) = achar(i) // achar(i+1) // achar(i+3)

  print *, [integer(4)],   [integer(4)::   integer(4)],   [integer(4)::   (integer(4),integer(i), i=4,4)]
  print *, [int(1)],       [integer(4)::   int(1)],       [integer(4)::   (int(1),int(i), i=1,1)]
  print *, [real(4)],      [real(4)::      real(4)],      [real(4)::      (real(4),real(i), i=4,4)]
  print *, [complex(4)],   [complex(4)::   complex(4)],   [complex(4)::   (complex(4),complex(i), i=4,4)]
  print *, [cmplx(6.,7.)], [complex(4)::   cmplx(6.,7.)], [complex(4)::   (cmplx(6.,7.),cmplx(real(i),real(i)), i=1,1)]
  print *, [logical(4)],   [logical(4)::   logical(4)],   [logical(4)::   (logical(4),logical(i), i=4,4)]
  print *, [char(65)],     [character(3):: char(65)],     [character(3):: (char(65),char(i), i=65,65)]
  print *, [character(1)], [character(1):: character(1)], [character(1):: (character(1),character(i), i=1,1)]

  iarr  = [integer(4)::   integer(4)]
  iar2  = [integer(4)::   int(1)]
  rarr  = [real(4)::      real(4)]
  zarr  = [complex(4)::   complex(4)]
  zar2  = [complex(4)::   cmplx(6.,7.)]
  larr  = [logical(4)::   logical(4)]
  c3arr = [character(3):: char(65)]
  carr  = [character(1):: character(-1), character(0), character(1), character(2)]

  if (iarr(1) /= 16) stop 2
  if (iar2(1) /= 10) stop 3
  if (.not. precision_r4(rarr(1),sin(PI/4) ** 2)) stop 4
  if (.not. precision_x8(zarr(1),(sin(PI/4) ** 2, cos(PI/4) ** 2))) stop 5
  if (.not. precision_x8(zar2(1),(sin(6.) ** 2, cos(7.) ** 2))) stop 6
  if (.not. larr(1)) stop 7
  if (c3arr(1) /= 'ABD') stop 8
  if (any(carr /= ['L', 'G', 'G', 'G'])) stop 9

  iarr  = [integer(4)::   (integer(4), i=1,1)]
  iar2  = [integer(4)::   (int(1), i=1,1)]
  rarr  = [real(4)::      (real(4), i=1,1)]
  zarr  = [complex(4)::   (complex(4), i=1,1)]
  zar2  = [complex(4)::   (cmplx(6.,7.), i=1,1)]
  larr  = [logical(4)::   (logical(4), i=1,1)]
  c3arr = [character(3):: (char(65), i=1,1)]
  carr  = [character(1):: (character(-1), character(0), character(1), character(2), i=1,1)]

  if (iarr(1) /= 16) stop 11
  if (iar2(1) /= 10) stop 12
  if (.not. precision_r4(rarr(1),sin(PI/4) ** 2)) stop 13
  if (.not. precision_x8(zarr(1),(sin(PI/4) ** 2, cos(PI/4) ** 2))) stop 14
  if (.not. precision_x8(zar2(1),(sin(6.) ** 2, cos(7.) ** 2))) stop 15
  if (.not. larr(1)) stop 16
  if (c3arr(1) /= 'ABD') stop 17
  if (any(carr /= ['L', 'G', 'G', 'G'])) stop 18

  iarr  = [integer(4)::   (integer(i), i=4,4)]
  iar2  = [integer(4)::   (int(i), i=1,1)]
  rarr  = [real(4)::      (real(i), i=4,4)]
  zarr  = [complex(4)::   (complex(i), i=4,4)]
  zar2  = [complex(4)::   (cmplx(real(i),real(i)), i=1,1)]
  larr  = [logical(4)::   (logical(i), i=4,4)]
  c3arr = [character(3):: (char(i), i=65,65)]
  carr  = [character(1):: (character(-i), character(i-i), character(i), character(i+1), i=1,1)]

  if (iarr(1) /= 16) stop 21
  if (iar2(1) /= 10) stop 22
  if (.not. precision_r4(rarr(1),sin(PI/4) ** 2)) stop 23
  if (.not. precision_x8(zarr(1),(sin(PI/4) ** 2, cos(PI/4) ** 2))) stop 24
  if (.not. precision_x8(zar2(1),(sin(sin(PI)**2)**2, cos(sin(PI)**2)**2))) stop 25
  if (.not. larr(1)) stop 26
  if (c3arr(1) /= 'ABD') stop 27
  if (any(carr /= ['L', 'G', 'G', 'G'])) stop 28

end program acetint43
