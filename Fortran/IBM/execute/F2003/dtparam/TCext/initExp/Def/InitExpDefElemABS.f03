! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/initExp/Def/InitExpDefElemABS.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

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
!*  a reference to an elemental intrinsic function that is not a hardware-specific,
!*  service/utility, floating-point status/control, or vector intrinsic function, where each
!*  argument is an initialization expression;
!*  -- ABS
!*  (318859)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K1,K2)    ! (4,8)
    INTEGER, KIND :: K1,K2
    INTEGER(K1)   :: I=ABS(-1)+ABS(-2)
    COMPLEX(K2)   :: Cmpx=ABS((-3.0, -4.0))
    PROCEDURE(),NOPASS, POINTER :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE, PASS ::  ModSub
  END TYPE

  TYPE(DT(4,8)), PARAMETER :: Param=DT(4,8)(I=ABS(-4.1))

  CONTAINS

  SUBROUTINE ModSub(Arg)
  CLASS(DT(4,8)) :: Arg
  END SUBROUTINE

  END MODULE


  PROGRAM InitExpDefElemABS
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  TYPE :: DT1(K3,K4)    ! (4,8)
    INTEGER, KIND   :: K3,K4
    TYPE(DT(K3,K4)) :: Comp=DT(K3,K4)(I=ABS(-Param%I))
  END TYPE

  TYPE(DT1(4,8)) ::  T1,Arr1(ABS(-Param%I):INT(ABS(-Param%Cmpx)))
  PARAMETER  (  Arr1=(/(DT1(4,8)(DT(4,8)(I=ABS(-I), Cmpx=(I, -I))),&
                       I=ABS(-Param%I), INT(ABS(-Param%Cmpx)))/)  )

  TYPE, EXTENDS(DT) :: DT2(K5)    ! (4,8,4)
      INTEGER, KIND :: K5
  END TYPE

  TYPE(DT2(4,8,4)) :: Arr2(INT(ABS(-Param%Cmpx)))=DT2(4,8,4)(DT=DT(4,8)(I=ABS(-1), Cmpx=(1_2, -1_2)))


  IF (Param%I    .NE. 4   )                ERROR STOP 11
  IF (Param%Cmpx .NE. 5.0 )                ERROR STOP 12

  IF (T1%Comp%I    .NE. 4   )              ERROR STOP 21
  IF (T1%Comp%Cmpx .NE. 5.0 )              ERROR STOP 22

  IF (ANY(LBOUND(Arr1)   .NE. (/4/)) )     ERROR STOP 31
  IF (ANY(UBOUND(Arr1)   .NE. (/5/)) )     ERROR STOP 32

  IF (ANY(Arr1%Comp%I    .NE. (/(ABS(-I), I=ABS(-Param%I), ABS(-Param%Cmpx))/) ))    ERROR STOP 41
  IF (ANY(Arr1%Comp%Cmpx .NE. (/((I, -I), I=ABS(-Param%I), ABS(-Param%Cmpx))/) ))    ERROR STOP 42

  IF (ANY(LBOUND(Arr2)   .NE. (/1/)  ))                 ERROR STOP 51
  IF (ANY(UBOUND(Arr2)   .NE. (/ABS(-Param%Cmpx)/) ))   ERROR STOP 52

  IF (ANY(Arr2%I    .NE. ABS(-1) ))        ERROR STOP 61
  IF (ANY(Arr2%Cmpx .NE. (1_2, -1_2) ))    ERROR STOP 62


  END

