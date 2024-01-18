!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  FUNCTIONAL TESTED          : Finalize the object with type parameter
!*                               - Length type parameter of dummy argument
!*                                 must be assumed.
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
type base_dt(k, d)
  integer, kind :: k
  integer, len  :: d
  integer :: element(d, d)
  integer :: avar
  contains
  final :: colltype
end type

contains
subroutine sub1(pa)
 class(base_dt(4, *)), intent(out), allocatable  :: pa
end subroutine

subroutine colltype(b)
 type(base_dt(4, *)) :: b
 print *, "Finalize Object b"
 print *, "length type parameter = ", b%d
end subroutine
end module

use amod
class(base_dt(4, 4)), allocatable :: aptr
allocate(aptr)

call sub1(aptr)

end

