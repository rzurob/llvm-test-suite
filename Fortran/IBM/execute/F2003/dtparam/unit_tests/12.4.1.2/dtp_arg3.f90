!*  ===================================================================
!*
!*  FUNCTIONAL TESTED          : - type renaming
!*                               - passed object
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
type humongous_matrix(k, d)
  integer, kind :: k
  integer, len  :: d
  integer :: element(d)
  integer :: avar
  contains
  procedure :: prt_tp => hmprint
end type

contains
subroutine hmprint(pa)
class(humongous_matrix(4, *)) :: pa
if (pa%k /= 4) stop 1
if (pa%d /= 10) stop 2
end subroutine
end module

use amod, ww => humongous_matrix
class(ww(4, 10)), pointer :: aptr

call aptr%prt_tp()
end
