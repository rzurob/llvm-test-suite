!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefArrConstr1.f
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




  PROGRAM InitExpDefArrConstr1
  IMPLICIT NONE

  INTEGER :: IArr(10)

  TYPE :: DT
    INTEGER  :: ID(SIZE(IArr), SIZE(IArr))=SIZE(IArr)
    PROCEDURE(),NOPASS, POINTER :: ProcPtr
  END TYPE

  INTEGER :: I, J

  TYPE(DT)   :: Arr(SIZE(IArr), SIZE(IArr))
  PARAMETER  (  Arr=DT(ID=RESHAPE((/((i*j, I=KIND(.FALSE._1), SIZE(IArr)), J=1, SIZE(IArr))/), &
                                  (/10,10/)), ProcPtr=NULL())  )

  TYPE :: DT1
    TYPE(DT) :: Arr(SIZE(IArr), SIZE(IArr))
  END TYPE

  TYPE(DT1) ::  Arr1(SIZE(IArr))
  PARAMETER  (  Arr1=(/(DT1(Arr), I=KIND(1_1), SIZE(IArr))/) )

  TYPE, EXTENDS(DT) :: DT2
  END TYPE

  TYPE(DT2), PARAMETER  :: Arr2(SIZE(IArr))=DT2(DT=Arr1(LEN("1"))%Arr(SIZE(IArr), SIZE(IArr)))


  IF (ANY(LBOUND(Arr(1,10)%ID)   .NE. (/1,1/) ) )                                        STOP 11
  IF (ANY(UBOUND(Arr(10,1)%ID)   .NE. (/10,10/) ))                                       STOP 11
  IF (ANY(Arr(10,10)%ID          .NE. RESHAPE((/((I*J, I=1,10),J=1,10)/), (/10,10/))  ) ) STOP 12

  IF (ANY(LBOUND(Arr1(1)%Arr(10,1)%ID)   .NE. (/1,1/) )         )                                 STOP 21
  IF (ANY(UBOUND(Arr1(10)%Arr(1,10)%ID)  .NE. (/10,10/) )       )                                 STOP 22
  IF (ANY(Arr1(1)%Arr(10,10)%ID          .NE. RESHAPE((/((I*J, I=1,10),J=1,10)/), (/10,10/))  ) ) STOP 23
  IF (ANY(Arr1(10)%Arr(1,1)%ID           .NE. RESHAPE((/((I*J, I=1,10),J=1,10)/), (/10,10/))  ) ) STOP 24

  IF (ANY(LBOUND(Arr2(1)%ID)   .NE. (/1,1/)) )                                          STOP 31
  IF (ANY(UBOUND(Arr2(10)%ID)  .NE. (/10,10/)) )                                        STOP 32
  IF (ANY(Arr2(1)%ID           .NE. RESHAPE((/((I*J, I=1,10),J=1,10)/), (/10,10/))  ) ) STOP 33
  IF (ANY(Arr2(10)%ID          .NE. RESHAPE((/((I*J, I=1,10),J=1,10)/), (/10,10/))  ) ) STOP 34

  END


