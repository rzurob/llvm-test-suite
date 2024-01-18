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
!*  a reference to an elemental intrinsic function that is not a hardware-specific, service/utility,
!*  floating-point status/control, or vector intrinsic function, where each argument is
!*  an initialization expression;
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M


  INTEGER :: IArr(10)

  TYPE :: DT
    INTEGER  :: I
    PROCEDURE(),NOPASS, POINTER :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE, NOPASS ::  ModSub
  END TYPE

  TYPE(DT), PARAMETER :: Param=DT(10)

  CONTAINS

  SUBROUTINE ModSub()
  END SUBROUTINE

  END MODULE


  PROGRAM InitExpDefElem
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  TYPE :: DT1
    TYPE(DT) :: Arr(1:ABS(-Param%I), 1:SIZE(IArr))=DT(ABS(ABS(-Param%I)))
  END TYPE

  TYPE(DT1) ::  T1,Arr1(ABS(SIZE(IArr)))
  PARAMETER  (  Arr1=(/(DT1(DT(ABS(-1))),&
                       I=KIND(1_1), SIZE(IArr))/)  )

  TYPE, EXTENDS(DT) :: DT2
  END TYPE

  TYPE(DT2) :: Arr2(1+9)=DT2(DT=DT(SIZE(Arr1)))

  IF (ANY(LBOUND(T1%Arr)   .NE. (/1,1 /) ))            ERROR STOP 11
  IF (ANY(UBOUND(T1%Arr)   .NE. (/10,10/) ))           ERROR STOP 12
  IF (ANY(T1%Arr(:,:)%I    .NE. ABS(ABS(-Param%I))))   ERROR STOP 13

  DO I=1, 10
    IF (ANY(LBOUND(Arr1(I)%Arr)   .NE. (/1,1 /) ))     ERROR STOP 21
    IF (ANY(UBOUND(Arr1(I)%Arr)   .NE. (/10,10/) ))    ERROR STOP 22
    IF (ANY(Arr1(I)%Arr(:, :)%I   .NE. 1  ) )          ERROR STOP 23
  END DO

  IF (ANY(Arr2%I .NE. SIZE(Arr1)) )    ERROR STOP 31

  END


