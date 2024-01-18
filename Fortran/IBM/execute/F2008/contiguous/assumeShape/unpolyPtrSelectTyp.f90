! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
MODULE Mod
      IMPLICIT NONE
      INTEGER, TARGET  :: tgt(100)

      TYPE :: Base
        CLASS(*), POINTER, CONTIGUOUS ::  cp(:)
      END TYPE

      CONTAINS

      SUBROUTINE Sub(arg)
        INTEGER :: I
        CLASS(*), POINTER, CONTIGUOUS :: arg(:)

        tgt = [(I, I=1,100)]

        IF ( .NOT. IS_CONTIGUOUS(arg) ) STOP 31
        IF ( .NOT. IS_CONTIGUOUS(tgt) ) STOP 32

        arg => tgt
        IF ( .NOT. IS_CONTIGUOUS(arg) ) STOP 33

      END SUBROUTINE Sub
END MODULE Mod
PROGRAM unpolyPtrSelectTyp
      USE Mod
      IMPLICIT NONE

      INTEGER :: I, J
      TYPE(Base), TARGET :: T0(2)
      TYPE(Base), ALLOCATABLE, TARGET :: T1(:)
      TYPE(Base), POINTER, CONTIGUOUS :: P0(:)

      P0 => T0
      IF ( .NOT. ASSOCIATED(P0)    ) STOP 14
      IF ( .NOT. IS_CONTIGUOUS(P0) ) STOP 15
      IF ( .NOT. IS_CONTIGUOUS(T0) ) STOP 16

      DO I = 1, SIZE(P0)
          CALL Sub(P0(I)%cp)
          SELECT TYPE (s => P0(I)%cp)
              TYPEIS (INTEGER)
                IF (ANY(s .NE. [(J, J=1,100)])) STOP 17
              CLASS DEFAULT
                ERROR STOP 18
          END SELECT
      END DO

      T1 = T0
      IF ( .NOT. ALLOCATED(T1)     ) STOP 19
      IF ( .NOT. IS_CONTIGUOUS(T1) ) STOP 20

      P0 => T1

      IF ( .NOT. ASSOCIATED(P0)    ) STOP 21
      IF ( .NOT. IS_CONTIGUOUS(P0) ) STOP 22
      IF ( .NOT. IS_CONTIGUOUS(T0) ) STOP 23

      DO I = 1, SIZE(P0)
          CALL Sub(P0(I)%cp)
          SELECT TYPE (s => P0(I)%cp)
              TYPEIS (INTEGER)
                IF (ANY(s .NE. [(J, J=1,100)])) STOP 24
              CLASS DEFAULT
                ERROR STOP 25
          END SELECT
      END DO
END PROGRAM unpolyPtrSelectTyp
