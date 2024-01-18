!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefSELECTED_CHAR_KIND.f
!*
!*  DATE                       : Mar 30, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  a reference to an tranformational intrinsic
!*
!*  - SELECTED_CHAR_KIND
!*  (317648)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefSELECTED_CHAR_KIND
  IMPLICIT NONE
  INTEGER :: I, J, K


  CHARACTER(0),    PARAMETER  :: C0 = ""
  CHARACTER(1025), PARAMETER  :: C1 = " "
  CHARACTER(1025), PARAMETER  :: C2 = " NEVER "

  INTEGER, PARAMETER :: TK1 = SELECTED_CHAR_KIND("ASCII")
  INTEGER, PARAMETER :: TK2 = SELECTED_CHAR_KIND("   ASCII   ")
  INTEGER, PARAMETER :: TK3 = SELECTED_CHAR_KIND("aSCiI  ")
  INTEGER, PARAMETER :: TK4 = SELECTED_CHAR_KIND("ISO_10646")  ! not support now

  INTEGER, PARAMETER :: TK5 = SELECTED_CHAR_KIND(C0)
  INTEGER, PARAMETER :: TK6 = SELECTED_CHAR_KIND(C1)
  INTEGER, PARAMETER :: TK7 = SELECTED_CHAR_KIND(C2)

  IF (TK1                .NE.   1 )                   STOP 11
  IF (TK2                .NE.  -1 )                   STOP 12
  IF (TK3                .NE.   1 )                   STOP 13
  IF (TK4                .NE.  -1 )                   STOP 14
  IF (TK5                .NE.  -1 )                   STOP 15
  IF (TK6                .NE.  -1 )                   STOP 16
  IF (TK7                .NE.  -1 )                   STOP 17

  END



