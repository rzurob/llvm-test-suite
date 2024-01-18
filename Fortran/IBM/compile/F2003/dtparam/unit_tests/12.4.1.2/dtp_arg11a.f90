!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : ALLOCATE statement
!*                             :
!*  PROGRAMMER                 : Huiwen Li
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  FUNCTIONAL TESTED          : Finalize the object with type parameter
!*                               - Length type parameter of dummy argument
!*                                 must be assumed.
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

module bmod
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
 type(base_dt(4, 2)) :: b
end subroutine

end module
