! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/initExp/Misc/InitExpIntrinDefAssgn.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 22, 2006
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
!*  Defined assignment( for DT) shall not have any impact onto entity of DT
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: Id=0
  CONTAINS
    PROCEDURE :: Assign => MyAssign
    GENERIC   :: ASSIGNMENT(=) => Assign
  END TYPE

  CONTAINS

  ELEMENTAL SUBROUTINE MyAssign(Arg1, Arg2)
  CLASS(DT(*,4)), INTENT(INOUT):: Arg1
  TYPE(DT(*,4)),  INTENT(IN)   :: Arg2
    Arg1%Id = - Arg2%Id
  END SUBROUTINE

  END MODULE

  PROGRAM InitExpIntrinDefAssgn
  USE M
  IMPLICIT NONE

  INTEGER  :: I
  TYPE(DT(20,4)) :: T=DT(20,4)(1)
  TYPE(DT(20,4)) :: TArr(128)=(/(DT(20,4)(I), I=0,127)/)


  IF (T%Id        .NE. 1  )               STOP 11
  IF (ANY(TArr%Id .NE. (/(I, I=0,127)/))) STOP 12


  END


