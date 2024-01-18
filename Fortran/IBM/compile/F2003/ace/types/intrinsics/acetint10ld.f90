!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint10ld
!*
!*  DATE                       : 2006-08-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Intrinsic type specifier with incorrect KIND in assignment (LOGICAL)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic type, incorrect kind, LOGICAL
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that intrinsic type specifiers enforce correct KIND values.
!*  We look at the correctness of the value and that of the dynamic type and
!*  kind of the constructed array in a separate test.
!*  Here we verify that LOGICAL is correctly handled.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint10ld

  implicit none

  logical :: larr(2)

  larr  = (/logical(kind=16):: .true., .false./)
  larr  = (/logical(kind=-1):: .true., .false./)
  larr  = (/logical(kind=3):: .true., .false./)
  larr  = (/logical(kind=9):: .true., .false./)
  larr  = (/logical(kind=100):: .true., .false./)
  larr  = (/logical(kind='4'):: .true., .false./)
  larr  = (/logical(kind=4.4):: .true., .false./)
  larr  = (/logical(kind=(4.4,8.8)):: .true., .false./)
  larr  = (/logical(kind=.true.):: .true., .false./)
  larr  = (/logical(kind=.false.):: .true., .false./)

  larr  = (/logical(16):: .true., .false./)
  larr  = (/logical(-1):: .true., .false./)
  larr  = (/logical(3):: .true., .false./)
  larr  = (/logical(9):: .true., .false./)
  larr  = (/logical(100):: .true., .false./)
  larr  = (/logical('4'):: .true., .false./)
  larr  = (/logical(4.4):: .true., .false./)
  larr  = (/logical((4.4,8.8)):: .true., .false./)
  larr  = (/logical(.true.):: .true., .false./)
  larr  = (/logical(.false.):: .true., .false./)

end program acetint10ld
