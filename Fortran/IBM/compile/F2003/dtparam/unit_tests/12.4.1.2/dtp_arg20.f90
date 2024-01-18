!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  DIAGNOSTIC TESTED          : Actual argument doesn't match the dummy
!*                               argument.
!*                               - Actual argument and dummy argument need
!*                               - to defer the same type parameters.
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
  integer :: avar
end type

type(dt_base(4, :, 4)), pointer :: aptr

call sub1(aptr, 4)
contains
subroutine sub1(pa, len_p)
integer len_p
type(dt_base(4, :, :)), pointer :: pa
end subroutine

end

