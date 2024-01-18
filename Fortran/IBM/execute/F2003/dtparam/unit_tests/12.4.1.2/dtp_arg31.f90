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
!*  DIAGNOSTIC TESTED          : Automatic object which has 'auto' type
!*                               parameters.
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

type humongous_matrix(k, d)
  integer, kind :: k
  integer, len  :: d
  integer :: element(d, d)
  integer :: avar
end type
integer nn

call sub1(2)
contains
subroutine sub1( mm)
integer mm
type(humongous_matrix(4, mm)), pointer :: ptr
type(humongous_matrix(4, mm)), target  :: pa

print *, pa%d, ubound(pa%element)
print *, sizeof(pa%element)

ptr => pa

print *, ptr%d, ubound(ptr%element)
print *, sizeof(ptr%element)

ptr%avar = 100
ptr%element = 20

print *, pa%element
print *, pa%avar

end subroutine

end
