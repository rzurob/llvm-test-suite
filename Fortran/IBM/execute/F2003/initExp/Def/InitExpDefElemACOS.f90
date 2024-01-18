!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemACOS.f
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
!*  a reference to an elemental intrinsic
!*
!*  -ACOS
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT
    INTEGER    :: I=3*ACOS(0.54030231_8)
    REAL(4)    :: R= 5_8*ACOS(0.54030231_8)
    PROCEDURE(),NOPASS, POINTER :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE, PASS ::  ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg)
  CLASS(DT) :: Arg
  END SUBROUTINE

  END MODULE


  PROGRAM InitExpDefElemACOS
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  TYPE(DT), PARAMETER :: Param=DT(I=4._4*ACOS(0.54030231_4), R=5_8*ACOS(0.54030231_8))

  TYPE :: DT1
    TYPE(DT) :: Comp=DT(I=ABS(-Param%I))
  END TYPE

  TYPE(DT1) ::  T1,Arr1(NINT(3*ACOS(0.54030231_8)):NINT(Param%R))
  PARAMETER  (  Arr1=(/(DT1(DT(I=NINT(ACOS(I/5.1)), R=ACOS(I/5.001))),&
                       I=3*NINT(ACOS(0.54030231_8)), NINT(Param%R))/)  )

  TYPE, EXTENDS(DT) :: DT2
  END TYPE

  TYPE(DT2) :: Arr2(NINT(ACOS(Param%R)):NINT(3*ACOS(Param%R)))=    &
                        DT2(DT=DT(I=ACOS(0.54030231_8),   R=-ACOS(0.54030231_8)))


  IF (Param%I             .NE. 3   )                      STOP 11
  IF (ABS(Param%R-5_8*ACOS(0.54030231_8)) .GE. 1.e-5  )   STOP 12

  IF (T1%Comp%I          .NE. 3   )                       STOP 21
  IF (ABS(T1%Comp%R-5_8*ACOS(0.54030231_8)) .GE. 1.e-5 )  STOP 22

  IF (ANY(LBOUND(Arr1)   .NE. (/3/)) )     STOP 31
  IF (ANY(UBOUND(Arr1)   .NE. (/5/)) )     STOP 32

  IF (ANY(Arr1%Comp%I    .NE. (/(NINT(ACOS(I/5.0)),    I=3, 5)/) ))         STOP 41

  IF (.NOT. ALL(precision_R4(Arr1%Comp%R, (/(ACOS(I/5.001),    &
          I=NINT(3*ACOS(0.54030231_8)), NINT(5*ACOS(0.54030231_8)))/) )))   STOP 42

  IF (ANY(LBOUND(Arr2)   .NE. (/NINT(ACOS(Param%R))/)  ))             STOP 51
  IF (ANY(UBOUND(Arr2)   .NE. (/NINT(3*ACOS(Param%R))/) ))            STOP 52

  IF (ANY(Arr2%I    .NE. INT(ACOS(0.54030231_8)) ))                   STOP 61
  IF (ANY(ABS(Arr2%R +ACOS(0.54030231_8)).GE. 1.E-6  ))               STOP 62

  CONTAINS

      elemental logical function precision_R4*4(value,exp)
      real*4, intent(in) :: value,exp
      real*4  range,high_a,low_a,temp

      range = .00001

      temp = exp*range
      high_a = temp + exp
      low_a = exp - temp

      if(exp < 0.0E0) then
          precision_R4 = ((value.ge.high_a).and.(value.le.low_a))
      else
          precision_R4 = ((value.le.high_a).and.(value.ge.low_a))
      end if
      return

      end function

  END

