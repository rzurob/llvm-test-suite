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
!*  DIAGNOSTIC TESTED          : Length type parameter of passed-object
!*                               dummy argument of binding procedure
!*                               must be assumed
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
class(humongous_matrix(4, 10)) :: pa
end subroutine
end module

use amod, ww => humongous_matrix
class(ww(4, 10)), pointer :: aptr

call aptr%prt_tp()
end
