! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/initExp/Misc/InitExpData.f
! opt variations: -ql

!*********************************************************************
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpData.f
!*  TESTOP CASE TITLE            :
!*
!*  DATE                       : Sept. 11 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Charber 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Data Stmt  - Suggested by Jim
!*
!* (325509)
!   2008/06/11: JX: after reviewing the case, decide to replace the complicated
!   init-expr in DATA statement with something more straightforward.
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpData
  IMPLICIT NONE

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: Subscript(0:127)
  END TYPE

  INTEGER  :: I,J
  TYPE(DT(4)), PARAMETER :: T(0:127)=[(DT(4)(Subscript=[(J,J=0,127)]), I=0,127)]

  INTEGER , PARAMETER :: S(0:127)=[(J,J=0,127)]

  TYPE(DT(4)) :: T1(0:127)
  TYPE(DT(4)) :: T2(0:127)

  DATA (T1(I), I=0,127) /128*DT(4)(Subscript=[(J,J=0,127)])/
  DATA (T2(I), I=0,127) /128*DT(4)(Subscript=[(J,J=0,127)])/

  DO I=0, 127
    IF (ANY(T1(I)%Subscript  .NE. [(J,J=0,127)] )) ERROR STOP 11
    IF (ANY(T2(I)%Subscript  .NE. [(J,J=0,127)] )) ERROR STOP 12
  END DO

  END



