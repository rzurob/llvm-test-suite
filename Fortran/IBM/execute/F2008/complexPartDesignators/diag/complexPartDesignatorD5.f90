!*  ============================================================================
!*
!*  TEST CASE NAME             : complexPartDesignatorD5.f
!*
!*  DATE                       : 2011-01-17
!*
!*  PRIMARY FUNCTIONS TESTED   : Complex Part Designator
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 383634
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  This program tests the complex part designator:
!*     Checking the level (77, 90, 95, 2003 and 2008) of Fortran language.
!*     Any level below F2008 should return a warning message.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

   real R, I
   complex C

   C = (1.4567, 1.4567)
   R = C%RE                       ! Only valid in F2008+
   I = C%IM                       ! Only valid in F2008+
   C%RE = 1.4567                  ! Only valid in F2008+
   C%IM = 1.4567                  ! Only valid in F2008+

END

