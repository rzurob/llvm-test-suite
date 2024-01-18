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

  TYPE :: DT
    INTEGER    :: I=ABS(-1)+ABS(-2)
    COMPLEX(8) :: Cmpx=ABS((-3.0, -4.0))
    PROCEDURE(),NOPASS, POINTER :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE, PASS ::  ModSub
  END TYPE

  TYPE(DT), PARAMETER :: Param=DT(I=ABS(-4.1))

  CONTAINS

  SUBROUTINE ModSub(Arg)
  CLASS(DT) :: Arg
  END SUBROUTINE

  END MODULE


  PROGRAM InitExpDefElemABS
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  TYPE :: DT1
    TYPE(DT) :: Comp=DT(I=ABS(-Param%I))
  END TYPE

  TYPE(DT1) ::  T1,Arr1(ABS(-Param%I):INT(ABS(-Param%Cmpx)))
  PARAMETER  (  Arr1=(/(DT1(DT(I=ABS(-I), Cmpx=(I, -I))),&
                       I=ABS(-Param%I), INT(ABS(-Param%Cmpx)))/)  )

  TYPE, EXTENDS(DT) :: DT2
  END TYPE

  TYPE(DT2) :: Arr2(INT(ABS(-Param%Cmpx)))=DT2(DT=DT(I=ABS(-1), Cmpx=(1_2, -1_2)))


  IF (Param%I    .NE. 4   )                STOP 11
  IF (Param%Cmpx .NE. 5.0 )                STOP 12

  IF (T1%Comp%I    .NE. 4   )              STOP 21
  IF (T1%Comp%Cmpx .NE. 5.0 )              STOP 22

  IF (ANY(LBOUND(Arr1)   .NE. (/4/)) )     STOP 31
  IF (ANY(UBOUND(Arr1)   .NE. (/5/)) )     STOP 32

  IF (ANY(Arr1%Comp%I    .NE. (/(ABS(-I), I=ABS(-Param%I), ABS(-Param%Cmpx))/) ))    STOP 41
  IF (ANY(Arr1%Comp%Cmpx .NE. (/((I, -I), I=ABS(-Param%I), ABS(-Param%Cmpx))/) ))    STOP 42

  IF (ANY(LBOUND(Arr2)   .NE. (/1/)  ))                 STOP 51
  IF (ANY(UBOUND(Arr2)   .NE. (/ABS(-Param%Cmpx)/) ))   STOP 52

  IF (ANY(Arr2%I    .NE. ABS(-1) ))        STOP 61
  IF (ANY(Arr2%Cmpx .NE. (1_2, -1_2) ))    STOP 62


  END


