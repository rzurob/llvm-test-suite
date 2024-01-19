!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 07, 2006
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
!*  a reference to an elemental intrinsic
!*
!*  - IBCLR
!*  (319007)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemIBCLR
  INTEGER(1), PARAMETER :: IA1=5
  INTEGER(1), PARAMETER :: IA2=0
  IMPLICIT INTEGER(IBCLR(POS=IA2, I=IA1))(A)

  INTEGER :: I, J

  INTEGER(1), PARAMETER :: I11=3
  INTEGER(1), PARAMETER :: I12=1
  INTEGER(KIND(IBCLR(POS=I12, I=I11))), PARAMETER :: I13=IBCLR(POS=I12, I=I11)

  INTEGER(2), PARAMETER :: I21=3
  INTEGER(1), PARAMETER :: I22(128)=1
  INTEGER(KIND(IBCLR(POS=I22, I=I21))), PARAMETER :: I23(128)=IBCLR(POS=I22, I=I21)

  INTEGER(4), PARAMETER :: I41(128)=3
  INTEGER(2), PARAMETER :: I42=1
  INTEGER(KIND(IBCLR(POS=I42, I=I41))), PARAMETER :: I43(128)=IBCLR(POS=I42, I=I41)

  INTEGER(8), PARAMETER :: I81(128)=3
  INTEGER(1), PARAMETER :: I82(128)=1
  INTEGER(KIND(IBCLR(POS=I82, I=I81))), PARAMETER :: I83(128)=IBCLR(POS=I82, I=I81)


   IF ( KIND(A)    .NE. 4 )         ERROR STOP 10

   IF ( KIND(I13)  .NE. 1 )         ERROR STOP 11
   IF (      I13   .NE. 1 )         ERROR STOP 12

   IF ( KIND(I23)  .NE. 2 )         ERROR STOP 21
   IF ( ANY( I23   .NE. 1 ))        ERROR STOP 22

   IF ( KIND(I43)  .NE. 4 )         ERROR STOP 31
   IF ( ANY( I43   .NE. 1 ))        ERROR STOP 32

   IF ( KIND(I83)  .NE. 8 )         ERROR STOP 41
   IF ( ANY( I83   .NE. 1 ))        ERROR STOP 42




  END


