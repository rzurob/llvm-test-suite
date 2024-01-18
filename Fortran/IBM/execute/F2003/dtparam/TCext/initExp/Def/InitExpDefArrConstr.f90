! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv /tstdev/F2003/initExp/Def/InitExpDefArrConstr.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self

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

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: ID(10:19, 1:10)
    PROCEDURE(),NOPASS, POINTER :: ProcPtr
  END TYPE

  INTEGER :: I, J

  TYPE(DT(4))   :: Arr(10,10)
  PARAMETER  (  Arr=DT(4)(ID=RESHAPE((/((i*j, I=1, 10), J=1, 10)/), (/10,10/)), ProcPtr=NULL())  )

  TYPE :: DT1(K2)    ! (4)
    INTEGER, KIND :: K2
    TYPE(DT(K2))  :: Arr(10,10)
  END TYPE

  TYPE(DT1(4)) ::  Arr1(10)
  PARAMETER  (  Arr1=(/(DT1(4)(Arr), I=1, 10)/) )

  TYPE, EXTENDS(DT) :: DT2    ! (4)
  END TYPE

  TYPE(DT2(4)), PARAMETER  :: Arr2(10:10)=DT2(4)(DT=Arr1(10)%Arr(1,1))


  IF (ANY(LBOUND(Arr(1,10)%ID)   .NE. (/10,1/)  ))                                        STOP 11
  IF (ANY(UBOUND(Arr(10,1)%ID)   .NE. (/19,10/) ))                                        STOP 12
  IF (ANY(Arr(10, 10)%ID         .NE. RESHAPE((/((I*J, I=1,10),J=1,10)/), (/10,10/))  ) ) STOP 13

  IF (ANY(LBOUND(Arr1(1)%Arr(10,1)%ID )  .NE. (/10,1/) )   )                              STOP 21
  IF (ANY(UBOUND(Arr1(10)%Arr(1,10)%ID)  .NE. (/19,10/) ) )                               STOP 22
  IF (ANY(Arr1(1)%Arr(10,10)%ID          .NE. RESHAPE((/((I*J, I=1,10),J=1,10)/), (/10,10/))  ) ) STOP 23
  IF (ANY(Arr1(10)%Arr(1,1)%ID           .NE. RESHAPE((/((I*J, I=1,10),J=1,10)/), (/10,10/))  ) ) STOP 24

  IF (ANY(LBOUND(Arr2(10)%ID)  .NE. (/10,1/)) )                                          STOP 31
  IF (ANY(UBOUND(Arr2(10)%ID)  .NE. (/19,10/)) )                                         STOP 32
  IF (ANY(Arr2(10)%ID          .NE. RESHAPE((/((I*J, I=1,10),J=1,10)/), (/10,10/))  ) )  STOP 33
  IF (ANY(Arr2(10)%ID          .NE. RESHAPE((/((I*J, I=1,10),J=1,10)/), (/10,10/))  ) )  STOP 34

  END


