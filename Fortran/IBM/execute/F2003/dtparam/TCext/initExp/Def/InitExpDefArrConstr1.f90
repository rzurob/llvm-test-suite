! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv /tstdev/F2003/initExp/Def/InitExpDefArrConstr1.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=self

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

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: ID(SIZE(IArr), SIZE(IArr))=SIZE(IArr)
    PROCEDURE(),NOPASS, POINTER :: ProcPtr
  END TYPE

  INTEGER :: I, J

  TYPE(DT(20,4))   :: Arr(SIZE(IArr), SIZE(IArr))
  PARAMETER  (  Arr=DT(20,4)(ID=RESHAPE((/((i*j, I=KIND(.FALSE._1), SIZE(IArr)), J=1, SIZE(IArr))/), &
                                  (/10,10/)), ProcPtr=NULL())  )

  TYPE :: DT1(K2,N2)    ! (4,20)
    INTEGER, KIND   :: K2
    INTEGER, LEN    :: N2
    TYPE(DT(N2,K2)) :: Arr(SIZE(IArr), SIZE(IArr))
  END TYPE

  TYPE(DT1(4,20)) ::  Arr1(SIZE(IArr))
  PARAMETER  (  Arr1=(/(DT1(4,20)(Arr), I=KIND(1_1), SIZE(IArr))/) )

  TYPE, EXTENDS(DT) :: DT2    ! (20,4)
  END TYPE

  TYPE(DT2(20,4)), PARAMETER  :: Arr2(SIZE(IArr))=DT2(20,4)(DT=Arr1(LEN("1"))%Arr(SIZE(IArr), SIZE(IArr)))


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


