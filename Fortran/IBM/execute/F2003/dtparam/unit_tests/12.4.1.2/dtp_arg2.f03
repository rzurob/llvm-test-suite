!*  ===================================================================
!*
!*  FUNCTIONAL TESTED          : dummy arg is a pointer or an allocatable
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

module amod
type aa(ln)
 integer, len :: ln
 integer avar(ln)
end type

type(aa(2)), allocatable, target :: aptr
end module

use amod
type(aa(2)), pointer :: pptr
call sub1(aptr, pptr)

! Check if the value of 'pptr' gets updated
if (any(pptr%avar .ne. (/100, 100/))) error stop 4

contains
subroutine sub1(pa, pb)
type(aa(*)), target, allocatable :: pa
type(aa(*)), pointer :: pb

! - Check if dummy args' type parameters inherit from the acutal args.
if (pa%ln .ne. 2) error stop 1
!if (ubound(pb%avar, 1) .ne. 2) error stop 2

allocate(pa)
pa%avar = 100

! - Modify the value of dummy arguments
pb => pa
end subroutine

end
