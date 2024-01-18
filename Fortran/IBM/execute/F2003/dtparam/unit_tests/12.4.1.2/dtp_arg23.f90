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
!*  FUNCTIONAL TESTED          : Automatic array has a length type para-  
!*                               meter which is a dummy argument.
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

type(humongous_matrix(4, 2)) :: obj1
obj1%element = 10
obj1%avar = 100

nn = 2
call sub1(obj1, nn)
contains
subroutine sub1(arg, mm)
integer mm
type(humongous_matrix(4, mm)), allocatable :: pa
type(humongous_matrix(4, *)) arg

allocate(pa, source=arg)

end subroutine

end
