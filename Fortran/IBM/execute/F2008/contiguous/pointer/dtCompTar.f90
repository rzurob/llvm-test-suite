! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : dtCompTar.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
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
      IF ( .NOT. IS_CONTIGUOUS(Iarr) )    STOP 10
      IF ( .NOT. IS_CONTIGUOUS(tgt)  )    STOP 11
      IF (ANY(Iarr .NE.   [(I, I=1,10)])) STOP 12    
      IF (ANY(tgt  .NE. [(2*I, I=1,10)])) STOP 13   

      P0 => T0 
      IF ( .NOT. ASSOCIATED(P0)    ) STOP 14
      IF ( .NOT. IS_CONTIGUOUS(P0) ) STOP 15
      IF ( .NOT. IS_CONTIGUOUS(T0) ) STOP 16
      IF ( P0%K0 .NE. 4  ) STOP 32
      IF ( P0%L0 .NE. 10 ) STOP 33

      DO I = 1, SIZE(P0)
          P0(I)%cp => tgt(:I)
          IF ( .NOT. IS_CONTIGUOUS(P0(I)%cp) ) STOP 17
      END DO

      DO I = 1, SIZE(P0)
          IF (ANY(P0(I)%cp .NE. [(2*J, J=1,I)])) STOP 18    
      END DO

      T1 = T0
      IF ( .NOT. ALLOCATED(T1)     ) STOP 19
      IF ( .NOT. IS_CONTIGUOUS(T1) ) STOP 20
      
      P0 => T1 

      IF ( .NOT. ASSOCIATED(P0)    ) STOP 21
      IF ( .NOT. IS_CONTIGUOUS(P0) ) STOP 22
      IF ( .NOT. IS_CONTIGUOUS(T0) ) STOP 23
      IF ( P0%K0 .NE. 4  ) STOP 24
      IF ( P0%L0 .NE. 10 ) STOP 25

      DO I = 1, SIZE(P0)
          P0(I)%cp => tgt(:I)
          IF ( .NOT. IS_CONTIGUOUS(P0(I)%cp) ) STOP 26
      END DO

      DO I = 1, SIZE(P0)
          IF (ANY(P0(I)%cp .NE. [(2*J, J=1,I)])) STOP 27
      END DO
END PROGRAM dtCompTar
