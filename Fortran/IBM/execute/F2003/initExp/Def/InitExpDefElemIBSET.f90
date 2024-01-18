!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemIBSET.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 10, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289074 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  a reference to an elemental intrinsic
!* 
!*  - IBSET 
!*  (319007)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemIBSET
  PARAMETER ( IA1=0 )
  PARAMETER ( IA2=3 )
  IMPLICIT INTEGER(IBSET(POS=IA2, I=IA1))(A)

 
  INTEGER :: I, J

  INTEGER(1), PARAMETER :: I11=0
  INTEGER(1), PARAMETER :: I12=0
  INTEGER(KIND(IBSET(POS=I12, I=I11))), PARAMETER :: I13=IBSET(POS=I12, I=I11)

  INTEGER(2), PARAMETER :: I21=0
  INTEGER(1), PARAMETER :: I22(128)=1
  INTEGER(KIND(IBSET(POS=I22, I=I21))), PARAMETER :: I23(128)=IBSET(POS=I22, I=I21)

  INTEGER(4), PARAMETER :: I41(128)=0
  INTEGER(2), PARAMETER :: I42=2
  INTEGER(KIND(IBSET(POS=I42, I=I41))), PARAMETER :: I43(128)=IBSET(POS=I42, I=I41)

  INTEGER(8), PARAMETER :: I81(128)=0
  INTEGER(1), PARAMETER :: I82(128)=3
  INTEGER(KIND(IBSET(POS=I82, I=I81))), PARAMETER :: I83(128)=IBSET(POS=I82, I=I81)

  CLASS(*), POINTER :: IPtr

  IF ( KIND(A)    .NE. 8 )         STOP 10

  IF ( KIND(I13)  .NE. 1 )         STOP 11
  IF (      I13   .NE. 1 )         STOP 12
 
  IF ( KIND(I23)  .NE. 2 )         STOP 21
  IF ( ANY( I23   .NE. 2 ))        STOP 22
 
  IF ( KIND(I43)  .NE. 4 )         STOP 31
  IF ( ANY( I43   .NE. 4 ))        STOP 32

  IF ( KIND(I83)  .NE. 8 )         STOP 41
  IF ( ANY( I83   .NE. 8 ))        STOP 42
 
  ALLOCATE(IPtr, SOURCE=-1_2)

  SELECT TYPE ( As => IPtr)
  TYPE IS (INTEGER(IBSET(POS=0, I=0)))
    STOP 51 
  TYPE IS (INTEGER(IBSET(POS=1, I=0)))
    PRINT*, As 
  TYPE IS (INTEGER(IBSET(POS=2, I=0)))
    STOP 52 
  TYPE IS (INTEGER(IBSET(POS=3, I=0)))
    STOP 53 
  CLASS DEFAULT
    STOP 54
  END SELECT

  END


