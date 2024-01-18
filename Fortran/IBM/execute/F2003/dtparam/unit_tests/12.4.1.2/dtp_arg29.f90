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
!*  DIAGNOSTIC TESTED          : Dummy argument is a target pointed by
!*                               a pointer.
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
type dt1(k, d)
  integer, kind :: k
  integer, len  :: d
  integer :: element(d, d)
end type
end module

use amod
type(dt1(4, 2)) obj1
obj1%element = 100
call sub1(obj1, 2)
contains
subroutine sub1(pa, len_tp)
integer len_tp
type(dt1(4, len_tp)), target :: pa
type(dt1(4, :)), pointer :: str_aptr

str_aptr => pa

print *, str_aptr%element

end subroutine

end
