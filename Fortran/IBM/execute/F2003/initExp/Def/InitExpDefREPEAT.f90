!*********************************************************************
!*  ===================================================================
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
!*  - REPEAT
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpDefREPEAT
  IMPLICIT NONE
  INTEGER :: I, J, K



  INTEGER, PARAMETER :: TK0 = KIND(REPEAT("123", 0))
  INTEGER, PARAMETER :: TK1 = KIND(REPEAT("123", 100000))

  CHARACTER(1025), PARAMETER :: C=REPEAT("?", 1025)
  CHARACTER(LEN(REPEAT("?", 1025))), PARAMETER ::                   &
              CArr(LEN(REPEAT("?", 1026)):LEN(REPEAT("?", 2050)))=  &
              (/(REPEAT("?", 1025), I=1,1025)/)

  CHARACTER(0), PARAMETER                  :: C0=REPEAT("?", 0)
  CHARACTER(LEN(REPEAT(C0, 0))), PARAMETER :: CArr0(128)=REPEAT(C0, 0)
  CHARACTER(LEN(REPEAT(C0, 8))), PARAMETER :: CArr1(128)=REPEAT(C0, 0)
  CHARACTER(KIND(REPEAT(C0, 0))),PARAMETER :: CArr2(128)=REPEAT(C0, 0)

  IF (TK0                .NE.   1 )                   STOP 11
  IF (TK1                .NE.   1 )                   STOP 12

  IF (LEN(C)             .NE.  1025 )                 STOP 13
  IF (C                  .NE.  REPEAT("?", 1025) )    STOP 14

  IF (LEN(CArr)          .NE.  1025 )                 STOP 15
  IF (SIZE(CArr)         .NE.  1025 )                 STOP 16
  IF (ANY(LBOUND(CArr)   .NE.  (/1026/)) )            STOP 17
  IF (ANY(UBOUND(CArr)   .NE.  (/2050/)) )            STOP 18
  IF (ANY(CArr           .NE.   REPEAT("?", 1025)) )  STOP 19

  IF (LEN(C0)            .NE.  0 )                 STOP 21
  IF (LEN(CArr0)         .NE.  0 )                 STOP 22
  IF (LEN(CArr1)         .NE.  0 )                 STOP 23
  IF (LEN(CArr2)         .NE.  1 )                 STOP 24



  END



