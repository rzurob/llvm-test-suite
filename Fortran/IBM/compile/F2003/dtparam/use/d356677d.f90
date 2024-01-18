!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-10-07
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : IMPORT statement must appear in an interface
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Discovered in DTP and USE: IMPORT statements can only appear in an INTERFACE.
!*  Diagnostic.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

      program d356677d
      import :: tkl
      end program d356677d
