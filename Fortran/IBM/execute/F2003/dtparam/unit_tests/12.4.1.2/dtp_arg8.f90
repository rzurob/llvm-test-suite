!*  ===================================================================
!*
!*  FUNCTIONAL TESTED          : Pass pointer and or allocatable argument
!*                               with deferred type parameters to explicit
!*                               explicit type parameter dummy argument
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

type base_type(k, d)
  integer*1, kind :: k
  integer*2, len  :: d
  integer :: element(d)
  integer :: avar
end type

type(base_type(4, :)), pointer :: aptr
type(base_type(4, :)), allocatable :: avar
type(base_type(4, 4)), target  :: tgt

tgt%avar = 12
tgt%element = tgt%avar - 20

aptr => tgt

call sub1(aptr)

allocate(avar, source=aptr)
call sub1(avar)
contains
subroutine sub1(pa)
type(base_type(4, 4)) :: pa

! - verify the dummy argument's values
if (pa%k /= 4) stop 1
if (pa%d /= 4) stop 2
if (any(pa%element .ne. (-8))) stop 8 ! - failed. Refer to Defect 321635.
if (pa%avar /= 12) stop 12
end subroutine

end
