!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 05, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration and specification
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  -- The common statement
!*
!*  Continuation of common blocks
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT_R(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    REAL(K)       :: R(L)!=K
  END TYPE

  TYPE :: DT_C(K,L)
    INTEGER, KIND :: K!=4
    INTEGER, LEN  :: L!=4
    SEQUENCE
    CHARACTER(L)  :: C(L)!=CHAR(48+K)
  END TYPE

  TYPE :: DT_I(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    INTEGER(K)    :: I(L)!=K
  END TYPE

  TYPE :: DT_L(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    LOGICAL(K)    :: A(L)!=.TRUE.
  END TYPE

  TYPE :: DT_Z(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    COMPLEX(K)    :: Z(L)!=(K,-K)
  END TYPE

  END MODULE

  BLOCK DATA

  INTEGER J
  REAL(16)          :: R1(9)!=16
  CHARACTER(9)      :: C1(9)!=CHAR(48-1)
  INTEGER(2)        :: I1(9)!=-2
  LOGICAL(8)        :: L1(9)!=.FALSE.
  COMPLEX(16)       :: Z1(9)!=-(16,-16)
  COMPLEX(16), PARAMETER :: ZC=-(16,-16)
  CHARACTER,   PARAMETER :: CC=CHAR(48-1)

  DATA R1 /9*16/
  DATA C1 /9*CC/
  DATA I1 /9*-2/
  DATA L1 /9*.FALSE./
  DATA Z1 /9*ZC/

  COMMON /BLK/R1
  COMMON /BLK/C1
  COMMON /BLK/I1
  COMMON /BLK/L1
  COMMON /BLK/Z1

  END BLOCK DATA

  PROGRAM dtpCommon2
  USE M

  CALL ExtSub()
  CALL IntSub()

  CONTAINS

  SUBROUTINE IntSub()

  INTEGER J
  REAL(16)          :: R1(9)
  CHARACTER(9)      :: C1(9)
  INTEGER(2)        :: I1(9)
  LOGICAL(8)        :: L1(9)
  COMPLEX(16)       :: Z1(9)

  COMMON /BLK/R1
  COMMON /BLK/C1
  COMMON /BLK/I1
  COMMON /BLK/L1
  COMMON /BLK/Z1

  DO J=1, 9
    IF ( ANY( R1 .NE.    -16        ) ) ERROR STOP 21
    IF ( ANY( C1 .NE.    CHAR(0)    ) ) ERROR STOP 22
    IF ( ANY( I1 .NE.    2          ) ) ERROR STOP 23
    IF ( ANY( L1 .NEQV. .TRUE.      ) ) ERROR STOP 24
    IF ( ANY( Z1 .NE.    (16,-16)   ) ) ERROR STOP 25
  END DO

  END SUBROUTINE

  END

  SUBROUTINE ExtSub()
  USE M

  TYPE(DT_R(16,9))  :: R(1)
  TYPE(DT_C(-1,9))  :: C(1)
  TYPE(DT_I(2,9))   :: I(1)
  TYPE(DT_L(8,9))   :: L(1)
  TYPE(DT_Z(16,9))  :: Z(1)

  COMMON /BLK/R
  COMMON /BLK/C
  COMMON /BLK/I
  COMMON /BLK/L
  COMMON /BLK/Z

  IF ( ANY( R(1)%R .NE.    16         ) ) ERROR STOP 11
  R(1)%R = -16

  IF ( ANY( C(1)%C .NE.    CHAR(48-1) ) ) ERROR STOP 12
  C(1)%C = CHAR(0)

  IF ( ANY( I(1)%I .NE.    -2         ) ) ERROR STOP 13
  I(1)%I = 2

  IF ( ANY( L(1)%A .NEQV. .FALSE.     ) ) ERROR STOP 14
  L(1)%A = .TRUE.

  IF ( ANY( Z(1)%Z .NE.    -(16,-16)  ) ) ERROR STOP 15
  Z(1)%Z = (16,-16)
  END SUBROUTINE