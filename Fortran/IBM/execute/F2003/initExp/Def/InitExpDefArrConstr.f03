!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 17, 2006
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
!*  an array constructor in which each element and implied-do control
!*  expression is an initialization expression
!*
!*  (ice)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpDefArrConstr
  IMPLICIT NONE

  TYPE :: DT
    INTEGER  :: ID(10:19, 1:10)
    PROCEDURE(),NOPASS, POINTER :: ProcPtr
  END TYPE

  INTEGER :: I, J

  TYPE(DT)   :: Arr(10,10)
  PARAMETER  (  Arr=DT(ID=RESHAPE((/((i*j, I=1, 10), J=1, 10)/), (/10,10/)), ProcPtr=NULL())  )

  TYPE :: DT1
    TYPE(DT) :: Arr(10,10)
  END TYPE

  TYPE(DT1) ::  Arr1(10)
  PARAMETER  (  Arr1=(/(DT1(Arr), I=1, 10)/) )

  TYPE, EXTENDS(DT) :: DT2
  END TYPE

  TYPE(DT2), PARAMETER  :: Arr2(10:10)=DT2(DT=Arr1(10)%Arr(1,1))


  IF (ANY(LBOUND(Arr(1,10)%ID)   .NE. (/10,1/)  ))                                        ERROR STOP 11
  IF (ANY(UBOUND(Arr(10,1)%ID)   .NE. (/19,10/) ))                                        ERROR STOP 12
  IF (ANY(Arr(10, 10)%ID         .NE. RESHAPE((/((I*J, I=1,10),J=1,10)/), (/10,10/))  ) ) ERROR STOP 13

  IF (ANY(LBOUND(Arr1(1)%Arr(10,1)%ID )  .NE. (/10,1/) )   )                              ERROR STOP 21
  IF (ANY(UBOUND(Arr1(10)%Arr(1,10)%ID)  .NE. (/19,10/) ) )                               ERROR STOP 22
  IF (ANY(Arr1(1)%Arr(10,10)%ID          .NE. RESHAPE((/((I*J, I=1,10),J=1,10)/), (/10,10/))  ) ) ERROR STOP 23
  IF (ANY(Arr1(10)%Arr(1,1)%ID           .NE. RESHAPE((/((I*J, I=1,10),J=1,10)/), (/10,10/))  ) ) ERROR STOP 24

  IF (ANY(LBOUND(Arr2(10)%ID)  .NE. (/10,1/)) )                                          ERROR STOP 31
  IF (ANY(UBOUND(Arr2(10)%ID)  .NE. (/19,10/)) )                                         ERROR STOP 32
  IF (ANY(Arr2(10)%ID          .NE. RESHAPE((/((I*J, I=1,10),J=1,10)/), (/10,10/))  ) )  ERROR STOP 33
  IF (ANY(Arr2(10)%ID          .NE. RESHAPE((/((I*J, I=1,10),J=1,10)/), (/10,10/))  ) )  ERROR STOP 34

  END

