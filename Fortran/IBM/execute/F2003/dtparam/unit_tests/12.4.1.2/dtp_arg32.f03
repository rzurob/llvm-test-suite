!*  ===================================================================
!*
!*  DIAGNOSTIC TESTED          : Automatic object array with unknown
!*                               type parameters.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

type dtbase(k, d)
  integer, kind :: k
  integer, len  :: d
  integer :: element(d, d)
end type
integer nn

call sub1(8)
contains
subroutine sub1( mm)
integer mm
type(dtbase(4, mm)) :: pa(2)

if (pa%d /= 8) error stop 1
if (any(ubound(pa(1)%element) .ne. 8)) error stop 2
if (any(ubound(pa(1)%element) .ne. ubound(pa(2)%element))) error stop 4
if (sizeof(pa(2)%element) /= 256) error stop 5

end subroutine

end
