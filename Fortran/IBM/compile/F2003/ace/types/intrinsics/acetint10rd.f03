!******************************************************************************
!*  ===========================================================================
!*
!*  DATE                       : 2006-08-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Intrinsic type specifier with incorrect KIND in assignment (REAL)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic type, incorrect kind, REAL
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that intrinsic type specifiers enforce correct KIND values.
!*  We look at the correctness of the value and that of the dynamic type and
!*  kind of the constructed array in a separate test.
!*  Here we verify that REAL is correctly handled.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint10rd

  implicit none

  real    :: rarr(3)
  double precision :: darr(3)

  rarr  = (/real(kind=32):: 1,2,3/)
  rarr  = (/real(kind=-1):: 1,2,3/)
  rarr  = (/real(kind=3):: 1,2,3/)
  rarr  = (/real(kind=9):: 1,2,3/)
  rarr  = (/real(kind=100):: 1,2,3/)
  rarr  = (/real(kind='4'):: 1,2,3/)
  rarr  = (/real(kind=4.4):: 1,2,3/)
  rarr  = (/real(kind=(4.4,8.8)):: 1,2,3/)
  rarr  = (/real(kind=.true.):: 1,2,3/)
  rarr  = (/real(kind=.false.):: 1,2,3/)

  rarr  = (/real(32):: 1,2,3/)
  rarr  = (/real(-1):: 1,2,3/)
  rarr  = (/real(3):: 1,2,3/)
  rarr  = (/real(9):: 1,2,3/)
  rarr  = (/real(100):: 1,2,3/)
  rarr  = (/real('4'):: 1,2,3/)
  rarr  = (/real(4.4):: 1,2,3/)
  rarr  = (/real((4.4,8.8)):: 1,2,3/)
  rarr  = (/real(.true.):: 1,2,3/)
  rarr  = (/real(.false.):: 1,2,3/)


  ! Any kind parameter at all is actually illegal.
  darr  = (/double precision(kind=4):: 1.2,2.3,3.4/)
  darr  = (/double precision(kind=8):: 1.2,2.3,3.4/)
  darr  = (/double precision(kind=16):: 1.2,2.3,3.4/)
  darr  = (/double precision(kind=-1):: 1.2,2.3,3.4/)
  darr  = (/double precision(kind=3):: 1.2,2.3,3.4/)
  darr  = (/double precision(kind=9):: 1.2,2.3,3.4/)
  darr  = (/double precision(kind=100):: 1.2,2.3,3.4/)
  darr  = (/double precision(kind='4'):: 1.2,2.3,3.4/)
  darr  = (/double precision(kind=4.4):: 1.2,2.3,3.4/)
  darr  = (/double precision(kind=(4.4,8.8)):: 1.2,2.3,3.4/)
  darr  = (/double precision(kind=.true.):: 1.2,2.3,3.4/)
  darr  = (/double precision(kind=.false.):: 1.2,2.3,3.4/)

  darr  = (/double precision(4):: 1.2,2.3,3.4/)
  darr  = (/double precision(8):: 1.2,2.3,3.4/)
  darr  = (/double precision(16):: 1.2,2.3,3.4/)
  darr  = (/double precision(-1):: 1.2,2.3,3.4/)
  darr  = (/double precision(3):: 1.2,2.3,3.4/)
  darr  = (/double precision(9):: 1.2,2.3,3.4/)
  darr  = (/double precision(100):: 1.2,2.3,3.4/)
  darr  = (/double precision('4'):: 1.2,2.3,3.4/)
  darr  = (/double precision(4.4):: 1.2,2.3,3.4/)
  darr  = (/double precision((4.4,8.8)):: 1.2,2.3,3.4/)
  darr  = (/double precision(.true.):: 1.2,2.3,3.4/)
  darr  = (/double precision(.false.):: 1.2,2.3,3.4/)

end program acetint10rd
