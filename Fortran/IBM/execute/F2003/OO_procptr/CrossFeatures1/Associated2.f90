! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 2, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  ASSOCIATED(POINTER [, TARGET])
!*  on proc pointer components
!*  (304566)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  CONTAINS

  FUNCTION FInt()
    INTEGER FInt
    FInt = -1
  END FUNCTION

  FUNCTION FReal()
    REAL FREAL
    FReal = -1.0
  END FUNCTION

  FUNCTION FCmp()
    COMPLEX ::  FCmp
    FCmp = (-1.0, 1.0)
  END FUNCTION

  FUNCTION FChar()
    CHARACTER FChar
    FChar = "!"
  END FUNCTION

  FUNCTION FLog()
    LOGICAL FLog
    FLog = .TRUE.
  END FUNCTION

  END MODULE

  PROGRAM Associated2
  USE M
  IMPLICIT NONE

  TYPE :: DT
    PROCEDURE(INTEGER),   POINTER, NOPASS :: PtrInt
    PROCEDURE(REAL),      POINTER, NOPASS :: PtrReal
    PROCEDURE(COMPLEX),   POINTER, NOPASS :: PtrCmp
    PROCEDURE(CHARACTER), POINTER, NOPASS :: PtrChar
    PROCEDURE(LOGICAL),   POINTER, NOPASS :: PtrLog
  END TYPE

  TYPE (DT) :: V(3)=DT(NULL(), NULL(), NULL(), NULL(), NULL())
  INTEGER   :: I

  DO I =1, 3
    IF ( ASSOCIATED( V(I)%PtrInt ))  STOP 11
    IF ( ASSOCIATED( V(I)%PtrReal )) STOP 12
    IF ( ASSOCIATED( V(I)%PtrCmp ))  STOP 13
    IF ( ASSOcIATED( V(I)%PtrChar )) STOP 14
    IF ( ASSOCIATED( V(I)%PtrLog ))  STOP 15
  END DO

  V = DT(FInt, FReal, FCmp, FChar, FLog)

  DO I =1, 3
    iF ( .NOT. ASSOCIATED( V(I)%PtrInt ))  STOP 21
    IF ( .NOT. ASSOCIATED( V(I)%PtrReal )) STOP 22
    IF ( .NOT. ASSOCIATED( V(I)%PtrCmp ))  STOP 23
    IF ( .NOT. ASSOCIATED( V(I)%PtrChar )) STOP 24
    IF ( .NOT. ASSOCIATED( V(I)%PtrLog ))  STOP 25
  END DO

  END

