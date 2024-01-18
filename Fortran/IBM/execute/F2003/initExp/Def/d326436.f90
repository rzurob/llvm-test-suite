!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 22, 2006
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
!*  Ref 326436
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM d326436

  TYPE :: DT
    REAL :: x
  END TYPE

  TYPE(DT) :: A(1) = (/ ( DT(I), I = 1, 1 )/)

  PRINT *, A

  IF ( A(1)%X .NE. 1.0 ) ERROR STOP 11

  END


