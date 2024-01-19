!*  ===================================================================
!*
!*  FUNCTIONAL TESTED          : Argument association combined with
!*                               pointer assignment.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

type dt1(k, d)
  integer, kind :: k
  integer, len  :: d
  integer :: element(d, d)
  integer :: avar
end type

integer num
type(dt1(k=4, d=4)) obj1
type(dt1(4, :)), pointer :: ee
obj1%element = 22

num = 2
call sub1(obj1, num)

! - verify the changes from the subroutine
if (obj1%avar /= 100) error stop 10
if (any(obj1%element(:, 1) .ne. 11)) error stop 11
if (any(obj1%element(:, 2) .ne. 12)) error stop 12
if (any(obj1%element(:, 3) .ne. 13)) error stop 13
if (any(obj1%element(:, 4) .ne. 14)) error stop 14

contains
subroutine sub1(pa, len_tp)
integer len_tp
type(dt1(4, len_tp*len_tp)), target :: pa
type(dt1(4, :)), pointer :: str_aptr
integer, pointer :: int_ptr(:, :)
integer rest(4, 4)

! - verify the length type parameter
if (pa%d /= 4) error stop 21

! - point to the dummy argument
str_aptr => pa
str_aptr%avar = 100

! - point to the array component of dummy argument
int_ptr => str_aptr%element

! - verify the result from pointer assignments
if (any(str_aptr%element .ne. 22)) error stop 22
!if (any(int_ptr .ne. 22)) error stop 23

! - do another array assignment
rest = pa%element

! - verify the result
if (any(rest .ne. 22)) error stop 24

! - modify the value of array component
str_aptr%element(:, 1) = 11
str_aptr%element(:, 4) = 14
int_ptr(:, 2) = 12
int_ptr(:, 3) = 13

end subroutine

end
