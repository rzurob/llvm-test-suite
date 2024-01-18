!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint10id
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-08-24
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Intrinsic type specifier with incorrect KIND in assignment (INTEGER)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : intrinsic type, incorrect kind, INTEGER
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Verify that intrinsic type specifiers enforce correct KIND values.
!*  We look at the correctness of the value and that of the dynamic type and
!*  kind of the constructed array in a separate test.
!*  Here we verify that INTEGER is correctly handled.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint10id

  implicit none

  integer :: iarr(3)

  iarr  = (/integer(kind=16):: 1,2,3/)
  iarr  = (/integer(kind=-1):: 1,2,3/)
  iarr  = (/integer(kind=3):: 1,2,3/)
  iarr  = (/integer(kind=9):: 1,2,3/)
  iarr  = (/integer(kind=100):: 1,2,3/)
  iarr  = (/integer(kind='4'):: 1,2,3/)
  iarr  = (/integer(kind=4.4):: 1,2,3/)
  iarr  = (/integer(kind=(4.4,8.8)):: 1,2,3/)
  iarr  = (/integer(kind=.true.):: 1,2,3/)
  iarr  = (/integer(kind=.false.):: 1,2,3/)

  iarr  = (/integer(16):: 1,2,3/)
  iarr  = (/integer(-1):: 1,2,3/)
  iarr  = (/integer(3):: 1,2,3/)
  iarr  = (/integer(9):: 1,2,3/)
  iarr  = (/integer(100):: 1,2,3/)
  iarr  = (/integer('4'):: 1,2,3/)
  iarr  = (/integer(4.4):: 1,2,3/)
  iarr  = (/integer((4.4,8.8)):: 1,2,3/)
  iarr  = (/integer(.true.):: 1,2,3/)
  iarr  = (/integer(.false.):: 1,2,3/)

end program acetint10id
