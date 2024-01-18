!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  DIAGNOSTIC TESTED          : Actual argument doesn't match the dummy
!*                               argument.
!*                               - the non-deferred length type parameters
!*                               - need to match.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

type dt_base(k, d, m)
  integer, kind :: k
  integer, len  :: d
  integer, len  :: m
  integer :: element(d, m)
end type

type(dt_base(4, :, 4)), pointer :: aptr

call sub1(aptr)
contains
subroutine sub1(pa)
type(dt_base(4, :, 8)), pointer :: pa
end subroutine

end

