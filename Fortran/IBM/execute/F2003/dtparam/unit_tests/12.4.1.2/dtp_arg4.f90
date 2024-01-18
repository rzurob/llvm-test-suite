!*  ===================================================================
!*
!*  FUNCTIONAL TESTED          : dummy arg is an explicit shape array
!*                               with assumed type parameters
!*                                -- check type parameter values
!*                                -- check the component values
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890


type Base(k, d)
  integer, kind :: k
  integer, len  :: d
  integer :: element(d)
  integer :: avar
end type

type(Base(k=4, d=2)), target  :: obj1(4)

! - initialize array obj1
do ii = 1, 4
  obj1(ii)%element = 12*ii
  obj1(ii)%avar = ii
end do

! - pass a non-contiguous array section
call sub1(obj1(1:4:2))

! - verify the changes in the subroutine
if (any(obj1(1)%element .ne. (/120, 120/))) stop 7
if (any(obj1(2)%element .ne. (/24 , 24 /))) stop 8
if (any(obj1(3)%element .ne. (/360, 360/))) stop 9
if (any(obj1(4)%element .ne. (/48 , 48 /))) stop 10

contains
subroutine sub1(pa)
! - dummy arg is an explicit-shape array with assumed type parameter
type(Base(4, *)) :: pa(2)

! - verify the type parameter value
if (pa(1)%d .ne. 2) stop 1

! - verify the bounds expr with length type parameter
if (ubound(pa(2)%element, 1) .ne. 2) stop 2

! - verify the component values
if (any(pa(1)%element .ne. (/12, 12/))) stop 4
if (any(pa(2)%element .ne. (/36, 36/))) stop 5
if (any(pa%avar .ne. (/1, 3/))) stop 6

! - modify the component value of dummy arg
pa(1)%element = 120
pa(2)%element = 360
end subroutine

end
