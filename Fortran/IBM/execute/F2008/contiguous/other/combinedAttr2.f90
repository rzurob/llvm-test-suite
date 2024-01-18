! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : combinedAttr2.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2010-10-13
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS attribute
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : - Combination of attributes CONTIGUOUS,
!*                                   and TARGET
!*                               - Array is of type INTEGER and of 
!*                                   derived type with type parameters
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

      TYPE :: DT0(K0, L0)
        INTEGER, KIND :: K0=0
        INTEGER, LEN  :: L0=0
      END TYPE

      INTEGER, PARAMETER :: M=512, K=5, P=2, NULL=0

      CONTAINS

      SUBROUTINE Sub3(tgt)
        CLASS(DT0(K,:)), POINTER :: ptr(:)
        TYPE(DT0(K,*)), TARGET, CONTIGUOUS :: tgt(:)

        IF (      ASSOCIATED(ptr)     ) STOP 40
        IF ( .NOT. IS_CONTIGUOUS(tgt) ) STOP 41

        ptr=>tgt
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) STOP 42

        SELECT TYPE( s=>ptr )
          TYPE IS (DT0(K,*))  
               IF ( s%K0 .NE. K ) STOP 43
               IF ( s%L0 .NE. M ) STOP 44

          CLASS DEFAULT
               STOP 45
        END SELECT
      END SUBROUTINE Sub3

      FUNCTION Fun1(tgt)
        CLASS(DT0(1,1)), TARGET, CONTIGUOUS, INTENT(IN)  :: tgt(:)
        CLASS(DT0(1,1)), POINTER :: Fun1

        IF ( SIZE(tgt) .LT. 1 ) STOP 50
        Fun1 => tgt(1)
      END FUNCTION
END MODULE
PROGRAM combinedAttr2
      USE Mod
      IMPLICIT NONE

      INTEGER :: I, I0(M)
      TYPE(DT0), ALLOCATABLE :: T0(:)
      CLASS(DT0(K,:)), ALLOCATABLE :: T1(:)
      CLASS(DT0(1,1)), POINTER, CONTIGUOUS :: P0(:)
      CLASS(DT0(1,1)), POINTER :: P1

      I0 = [(I, I=1,M)]
      IF ( .NOT. IS_CONTIGUOUS(I0) )     STOP 10
      CALL Sub1(I0)
      IF (ANY(I0 .NE. [(I, I=P,P*M,P)])) STOP 11

      ALLOCATE( T0(2), SOURCE = DT0() )
      IF ( .NOT. IS_CONTIGUOUS(T0) )     STOP 12
      CALL Sub2(T0)

      ALLOCATE( DT0(K,M) :: T1(M) )
      IF ( .NOT. IS_CONTIGUOUS(T1) )     STOP 13
      CALL Sub3(T1)

      ALLOCATE( P0(1) )
      IF ( .NOT. IS_CONTIGUOUS(P0) )     STOP 14
      IF ( .NOT.    ASSOCIATED(P0) )     STOP 15
      P1 => Fun1(P0) 
      IF (  .NOT.   ASSOCIATED(P1) )     STOP 16

      CONTAINS

      SUBROUTINE Sub1(tgt)
        INTEGER :: J
        INTEGER, POINTER :: ptr(:) 
        INTEGER, TARGET, CONTIGUOUS :: tgt(:)

        IF (      ASSOCIATED(ptr)     ) STOP 20
        IF ( .NOT. IS_CONTIGUOUS(tgt) ) STOP 21

        ptr=>tgt

        IF ( .NOT. IS_CONTIGUOUS(ptr) ) STOP 22

        DO J = 1, SIZE(tgt)
            tgt(J) = P*tgt(J) 
        END DO
      END SUBROUTINE Sub1

      SUBROUTINE Sub2(tgt)
        TYPE(DT0), POINTER :: ptr(:)
        TYPE(DT0), TARGET, CONTIGUOUS :: tgt(:)

        IF (      ASSOCIATED(ptr)     ) STOP 30
        IF ( .NOT. IS_CONTIGUOUS(tgt) ) STOP 31
        IF ( tgt%K0 .NE. NULL) STOP 32
        IF ( tgt%L0 .NE. NULL) STOP 33

        ptr=>tgt
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) STOP 34
        IF ( ptr%K0 .NE. NULL) STOP 35
        IF ( ptr%L0 .NE. NULL) STOP 36
      END SUBROUTINE Sub2
END PROGRAM combinedAttr2
