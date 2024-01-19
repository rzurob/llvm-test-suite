! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
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

      TYPE :: Base(K0, L0)
        INTEGER, KIND :: K0=4
        INTEGER, LEN  :: L0=10

        INTEGER(K0), POINTER, CONTIGUOUS ::  cp(:)
      END TYPE
END MODULE Mod
PROGRAM dtCompTar
      USE Mod
      IMPLICIT NONE

      INTEGER :: I, J, Iarr(10)
      INTEGER, TARGET :: tgt(10)
      TYPE(Base), TARGET :: T0(2)
      TYPE(Base), ALLOCATABLE, TARGET :: T1(:)
      TYPE(Base), POINTER, CONTIGUOUS :: P0(:)

      Iarr = [(I, I=1,10)]
      tgt = 2* Iarr
      IF ( .NOT. IS_CONTIGUOUS(Iarr) )    ERROR STOP 10
      IF ( .NOT. IS_CONTIGUOUS(tgt)  )    ERROR STOP 11
      IF (ANY(Iarr .NE.   [(I, I=1,10)])) ERROR STOP 12
      IF (ANY(tgt  .NE. [(2*I, I=1,10)])) ERROR STOP 13

      P0 => T0
      IF ( .NOT. ASSOCIATED(P0)    ) ERROR STOP 14
      IF ( .NOT. IS_CONTIGUOUS(P0) ) ERROR STOP 15
      IF ( .NOT. IS_CONTIGUOUS(T0) ) ERROR STOP 16
      IF ( P0%K0 .NE. 4  ) ERROR STOP 32
      IF ( P0%L0 .NE. 10 ) ERROR STOP 33

      DO I = 1, SIZE(P0)
          P0(I)%cp => tgt(:I)
          IF ( .NOT. IS_CONTIGUOUS(P0(I)%cp) ) ERROR STOP 17
      END DO

      DO I = 1, SIZE(P0)
          IF (ANY(P0(I)%cp .NE. [(2*J, J=1,I)])) ERROR STOP 18
      END DO

      T1 = T0
      IF ( .NOT. ALLOCATED(T1)     ) ERROR STOP 19
      IF ( .NOT. IS_CONTIGUOUS(T1) ) ERROR STOP 20

      P0 => T1

      IF ( .NOT. ASSOCIATED(P0)    ) ERROR STOP 21
      IF ( .NOT. IS_CONTIGUOUS(P0) ) ERROR STOP 22
      IF ( .NOT. IS_CONTIGUOUS(T0) ) ERROR STOP 23
      IF ( P0%K0 .NE. 4  ) ERROR STOP 24
      IF ( P0%L0 .NE. 10 ) ERROR STOP 25

      DO I = 1, SIZE(P0)
          P0(I)%cp => tgt(:I)
          IF ( .NOT. IS_CONTIGUOUS(P0(I)%cp) ) ERROR STOP 26
      END DO

      DO I = 1, SIZE(P0)
          IF (ANY(P0(I)%cp .NE. [(2*J, J=1,I)])) ERROR STOP 27
      END DO
END PROGRAM dtCompTar
