!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTIONS               : Testing DIM intrinsic function
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none

type base1(k)
   integer,  kind :: k
   integer(kind=k), allocatable :: int
   real(kind=k+4), pointer:: r
end type

type base2(k)
   integer,  kind :: k
   real(kind=k), allocatable :: r
   integer(kind=k-4), pointer :: int
end type

type(base1(4)) ::  b1
type(base2(8)) :: b2
integer(kind=4), target :: ti
real(kind=8), target :: tr

ti = -11
tr = -1.2341
allocate (b1%int, source = -10_4)
allocate (b2%r, source = -1.234_8)
b1%r=>tr
b2%int=>ti

if (dim(dim(b1%int, b2%int), dim(b2%int, b1%int)) .ne. 1) error stop 1
if (dim(b1%int, b2%int) .ne. fun(b1%int, b2%int)) error stop 2
if (dim(dim(b1%r, b2%r), dim(b2%r, b1%r)) .ne. 0) error stop 3

contains
function fun(x, y)
   integer x, y, fun
   fun = dim(x, y)
end function
end
