!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Argument association with DTP
!*                             :
!*  PROGRAMMER                 : Huiwen Li
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  FUNCTIONAL TESTED          : Automatic array whose bounds depend on
!*                               some type parameters.
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

type dt_base(k, d)
  integer, kind :: k
  integer, len  :: d
  integer :: element(d, d+2)
  integer :: avar
end type

integer num
type(dt_base(k=4, d=2)) :: obj1

obj1%avar = 100
obj1%element = 4
num = 2

call sub1(obj1, num)

contains
subroutine sub1(pa, len_tp)
integer len_tp
type(dt_base(4, len_tp)) :: pa
! - declare an automatic array with run-time size. The size is equal
! - to the array pa%element whose bounds depend on type parameters
integer rest(ubound(pa%element, 1), ubound(pa%element, 2))

! - check the bounds of array pa%element
if (any(shape(pa%element) .ne. (/2, 4/))) stop 1

! - check the bounds of array rest
if (any(shape(rest) .ne. (/2, 4/))) stop 2	! - Bounds not set correctly
                                                ! - Refer to defect 321768

! - doing array assignment
rest = pa%element

! - check the value of array rest
if (any(rest .ne. 4)) stop 4
end subroutine

end
