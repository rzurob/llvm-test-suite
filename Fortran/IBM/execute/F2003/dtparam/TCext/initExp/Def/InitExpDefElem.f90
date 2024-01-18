! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/initExp/Def/InitExpDefElem.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElem.f
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

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: I
    PROCEDURE(),NOPASS, POINTER :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE, NOPASS ::  ModSub
  END TYPE

  TYPE(DT(4)), PARAMETER :: Param=DT(4)(10)

  CONTAINS

  SUBROUTINE ModSub()
  END SUBROUTINE

  END MODULE


  PROGRAM InitExpDefElem
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  TYPE :: DT1(K2)    ! (4)
    INTEGER, KIND :: K2
    TYPE(DT(K2))  :: Arr(1:ABS(-Param%I), 1:SIZE(IArr))=DT(K2)(ABS(ABS(-Param%I)))
  END TYPE

  TYPE(DT1(4)) ::  T1,Arr1(ABS(SIZE(IArr)))
  PARAMETER  (  Arr1=(/(DT1(4)(DT(4)(ABS(-1))),&
                       I=KIND(1_1), SIZE(IArr))/)  )

  TYPE, EXTENDS(DT) :: DT2    ! (4)
  END TYPE

  TYPE(DT2(4)) :: Arr2(1+9)=DT2(4)(DT=DT(4)(SIZE(Arr1)))

  IF (ANY(LBOUND(T1%Arr)   .NE. (/1,1 /) ))            STOP 11
  IF (ANY(UBOUND(T1%Arr)   .NE. (/10,10/) ))           STOP 12
  IF (ANY(T1%Arr(:,:)%I    .NE. ABS(ABS(-Param%I))))   STOP 13

  DO I=1, 10
    IF (ANY(LBOUND(Arr1(I)%Arr)   .NE. (/1,1 /) ))     STOP 21
    IF (ANY(UBOUND(Arr1(I)%Arr)   .NE. (/10,10/) ))    STOP 22
    IF (ANY(Arr1(I)%Arr(:, :)%I   .NE. 1  ) )          STOP 23
  END DO

  IF (ANY(Arr2%I .NE. SIZE(Arr1)) )    STOP 31

  END


