!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 29 2006
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
!*  The save attribute
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpSave

  INTERFACE
    SUBROUTINE ExtSub(Arg1, Arg2)
    INTEGER :: Arg1
    LOGICAL :: Arg2
    END SUBROUTINE
  END INTERFACE

  CALL ExtSub(1, .FALSE.)
  CALL ExtSub(2, .TRUE.)
  CALL ExtSub(3, .FALSE.)

  END


  SUBROUTINE ExtSub(Arg1, Arg2)
  INTEGER :: Arg1
  LOGICAL :: Arg2

  INTEGER(KIND=1)  :: I1(128)=1
  INTEGER(KIND=2)  :: I2(128)=1
  INTEGER(KIND=4)  :: I4(128)=1
  INTEGER(KIND=8)  :: I8(128)=1

  LOGICAL(KIND=1)  :: L1(128)=.FALSE.
  LOGICAL(KIND=2)  :: L2(128)=.FALSE.
  LOGICAL(KIND=4)  :: L4(128)=.FALSE.
  LOGICAL(KIND=8)  :: L8(128)=.FALSE.

  REAL(4)    :: R4(128)=1
  REAL(8)    :: R8(128)=1
  REAL(16)   :: R6(128)=1

  COMPLEX(4) :: Z4(128)=(1.,-1.)
  COMPLEX(8) :: Z8(128)=(1.,-1.)
  COMPLEX(16):: Z6(128)=(1.,-1.)

  CHARACTER(LEN=1, KIND=1) :: C(128)=ACHAR(1)


  IF ( ANY(I1   .NE. Arg1 ) ) ERROR STOP 11
  IF ( ANY(I2   .NE. Arg1 ) ) ERROR STOP 12
  IF ( ANY(I4   .NE. Arg1 ) ) ERROR STOP 13
  IF ( ANY(I8   .NE. Arg1 ) ) ERROR STOP 14

  IF ( ANY(L1   .NEQV. Arg2 ) ) ERROR STOP 21
  IF ( ANY(L2   .NEQV. Arg2 ) ) ERROR STOP 22
  IF ( ANY(L4   .NEQV. Arg2 ) ) ERROR STOP 23
  IF ( ANY(L8   .NEQV. Arg2 ) ) ERROR STOP 24

  IF ( ANY(R4   .NE. Arg1 ) ) ERROR STOP 31
  IF ( ANY(R8   .NE. Arg1 ) ) ERROR STOP 32
  IF ( ANY(R6   .NE. Arg1 ) ) ERROR STOP 33

  IF ( ANY(Z4   .NE. (Arg1, -Arg1) ) ) ERROR STOP 41
  IF ( ANY(Z8   .NE. (Arg1, -Arg1) ) ) ERROR STOP 42
  IF ( ANY(Z6   .NE. (Arg1, -Arg1) ) ) ERROR STOP 43

  IF ( ANY(C   .NE. ACHAR(Arg1) ) ) ERROR STOP 51

  I1 = I1 + 1
  I2 = I2 + 1
  I4 = I4 + 1
  I8 = I8 + 1

  L1 = .NOT. L1
  L2 = .NOT. L2
  L4 = .NOT. L4
  L8 = .NOT. L8

  R4 = R4 + 1
  R8 = R8 + 1
  R6 = R6 + 1

  Z4 = Z4 + (1, -1)
  Z8 = Z8 + (1, -1)
  Z6 = Z6 + (1, -1)

  C = ACHAR(IACHAR(C) + 1)

  END SUBROUTINE



