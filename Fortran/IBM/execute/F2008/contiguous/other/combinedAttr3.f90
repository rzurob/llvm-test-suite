! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-15
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS attribute
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - Combination of attributes CONTIGUOUS,
!*                                   INTENT and OPTIONAL
!*                               - Array is of type derived type with
!*                                   type parameters
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
        INTEGER, KIND :: K0=4
        INTEGER, LEN  :: L0=10

        INTEGER       :: I0(K0) = -99
      END TYPE

      TYPE, EXTENDS(DT0)  :: DT1(K1, L1)
        INTEGER(K0), KIND    :: K1=K0
        INTEGER(K0), LEN     :: L1=K0

        CHARACTER(L1+3) :: C0 = "XLFtest"
      END TYPE

      CONTAINS

      SUBROUTINE Sub(Arg0, Arg1, Arg2)
        INTEGER :: I
        TYPE(DT0), OPTIONAL, CONTIGUOUS :: Arg0(:)
        CLASS(DT0(4,*)), OPTIONAL, CONTIGUOUS, INTENT(IN) :: Arg1(:)
        CLASS(DT0(5,*)), OPTIONAL, CONTIGUOUS, INTENT(INOUT) :: Arg2(:)

        IF ( PRESENT(Arg0) ) THEN
           IF ( .NOT. IS_CONTIGUOUS(Arg0) ) ERROR STOP 10
           IF ( Arg0%K0        .NE.     4 ) ERROR STOP 11
           IF ( Arg0%L0        .NE.    10 ) ERROR STOP 12
           IF ( SIZE(Arg0)     .NE.     1 ) ERROR STOP 13
           DO I = 1, SIZE(Arg0)
               IF ( ANY(Arg0(I)%I0 .NE. -99) ) ERROR STOP 14
           END DO
        END IF

        IF ( PRESENT(Arg1) ) THEN
           IF ( .NOT. IS_CONTIGUOUS(Arg1) ) ERROR STOP 20
           IF ( Arg1%K0        .NE.     4 ) ERROR STOP 21
           IF ( Arg1%L0        .NE.    10 ) ERROR STOP 22
           IF ( SIZE(Arg1)     .NE.     1 ) ERROR STOP 23
           DO I = 1, SIZE(Arg1)
               IF ( ANY(Arg1(I)%I0 .NE. -99) ) ERROR STOP 24
           END DO
        END IF

        IF ( PRESENT(Arg2) ) THEN
           IF ( .NOT. IS_CONTIGUOUS(Arg2) ) ERROR STOP 30
           IF ( Arg2%K0        .NE.     5 ) ERROR STOP 31
           IF ( Arg2%L0        .NE.    12 ) ERROR STOP 32
           IF ( SIZE(Arg2)     .NE.    12 ) ERROR STOP 33

           DO I = 1, SIZE(Arg2)
               IF ( ANY(Arg2(I)%I0 .NE. -99) ) ERROR STOP 34
               Arg2(I)%I0 = I
           END DO
        END IF
      END SUBROUTINE Sub
END MODULE
PROGRAM combinedAttr3
      USE Mod
      IMPLICIT NONE

      INTEGER :: I
      TYPE(DT0) :: T0(1)
      CLASS(DT0), ALLOCATABLE :: T1(:)
      CLASS(DT0(5,:)), ALLOCATABLE :: T2(:)

      IF ( .NOT. IS_CONTIGUOUS(T0) )     ERROR STOP 101

      ALLOCATE( T1(1), SOURCE = DT1() )
      IF ( .NOT. IS_CONTIGUOUS(T1) )     ERROR STOP 102

      ALLOCATE( DT0(5,12) :: T2(12) )
      IF ( .NOT. IS_CONTIGUOUS(T2) )     ERROR STOP 103

      CALL Sub( Arg0=T0 )
      CALL Sub( Arg1=T1 )
      CALL Sub( Arg2=T2 )

      DO I = 1, SIZE(T2)
         IF (ANY(T2(I)%I0 .NE. I)) ERROR STOP 104
         T2(I)%I0 = -99
      END DO

      CALL Sub( Arg0=T0, Arg1=T1, Arg2=T2)

      DO I = 1, SIZE(T2)
         IF (ANY(T2(I)%I0 .NE. I)) ERROR STOP 105
         T2(I)%I0 = -99
      END DO

      CALL Sub( T0, T1, T2)

      DO I = 1, SIZE(T2)
         IF (ANY(T2(I)%I0 .NE. I)) ERROR STOP 106
         T2(I)%I0 = -99
      END DO
END PROGRAM combinedAttr3
