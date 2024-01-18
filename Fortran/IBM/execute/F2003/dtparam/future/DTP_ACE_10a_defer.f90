!*  ===================================================================
!*
!*  DATE                       : April 24, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Array constructor with Type Specification
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!* Defect : 365272
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN  :: l1 = 10

        INTEGER(k1) :: A01(l1) = -1, A02(l1+1) = -2
      END TYPE

      TYPE :: DT1 (k2,l2)
        INTEGER, KIND :: k2 = 4
        INTEGER, LEN  :: l2 = 5

        INTEGER(k2) :: A1(l2-1) = -3
        TYPE(Base(k2,l2)) :: cmp
      END TYPE

      TYPE :: DT2 (k3,l3)
        INTEGER, KIND :: k3 = 4
        INTEGER, LEN  :: l3 = 2

        INTEGER(k3) :: A21(l3) = -4, A22(l3+2) = -5
        TYPE(DT1(k3,l3)) :: cmp(l3)
      END TYPE

      CONTAINS

      SUBROUTINE CreateNewBase(Arg)
      TYPE(DT2(4,*)) :: Arg(:)
      INTEGER :: I, J

      Arg = [DT2(4,Arg(size(Arg))%l3) :: DT2(4,Arg(size(Arg))%l3)( &
                      11, 22, DT1(4,Arg(size(Arg))%l3)([1, 2], Base(4,Arg(size(Arg))%l3)()) )]
      IF ( SIZE(Arg) .NE. 1 ) STOP 10
      DO I = 1, SIZE(Arg)
           IF ( Arg(I)%k3 .NE. 4 ) STOP 11
           IF ( Arg(I)%l3 .NE. 3 ) STOP 12
           IF ( SIZE(Arg(I)%A21)     .NE.   3 ) STOP 13
           IF ( SIZE(Arg(I)%A22)     .NE.   5 ) STOP 14
           IF ( LBOUND(Arg(I)%A21,1) .NE.   1 ) STOP 15
           IF ( LBOUND(Arg(I)%A22,1) .NE.   1 ) STOP 16
           IF ( UBOUND(Arg(I)%A21,1) .NE.   3 ) STOP 17
           IF ( UBOUND(Arg(I)%A22,1) .NE.   5 ) STOP 18
           IF ( ANY(Arg(I)%A21 .NE. 11) ) STOP 19
           IF ( ANY(Arg(I)%A22 .NE. 22) ) STOP 20

           IF ( SIZE(Arg(I)%cmp) .NE. 3 ) STOP 21
           DO J = 1, SIZE(Arg(I)%cmp)
                IF ( Arg(I)%cmp(J)%k2 .NE. Arg(I)%k3 ) STOP 22
                IF ( Arg(I)%cmp(J)%l2 .NE. Arg(I)%l3 ) STOP 23
                IF ( SIZE(Arg(I)%cmp(J)%A1)     .NE.   2 ) STOP 24
                IF ( LBOUND(Arg(I)%cmp(J)%A1,1) .NE.   1 ) STOP 25
                IF ( UBOUND(Arg(I)%cmp(J)%A1,1) .NE.   2 ) STOP 26
                IF ( Arg(I)%cmp(J)%cmp%k1  .NE. Arg(I)%k3 ) STOP 27
                IF ( Arg(I)%cmp(J)%cmp%l1  .NE. Arg(I)%l3 ) STOP 28
                IF ( SIZE(Arg(I)%cmp(J)%cmp%A01)     .NE.   3 ) STOP 29
                IF ( SIZE(Arg(I)%cmp(J)%cmp%A02)     .NE.   4 ) STOP 30
                IF ( LBOUND(Arg(I)%cmp(J)%cmp%A01,1) .NE.   1 ) STOP 31
                IF ( LBOUND(Arg(I)%cmp(J)%cmp%A02,1) .NE.   1 ) STOP 32
                IF ( UBOUND(Arg(I)%cmp(J)%cmp%A01,1) .NE.   3 ) STOP 33
                IF ( UBOUND(Arg(I)%cmp(J)%cmp%A02,1) .NE.   4 ) STOP 34
                IF ( ANY(Arg(I)%cmp(J)%A1      .NE. [1, 2]) ) STOP 35
                IF ( ANY(Arg(I)%cmp(J)%cmp%A01 .NE.     -1) ) STOP 36
                IF ( ANY(Arg(I)%cmp(J)%cmp%A02 .NE.     -2) ) STOP 37
           END DO
      END DO

      END SUBROUTINE

END MODULE
PROGRAM DTP_ACE_10
      USE Mod
      IMPLICIT NONE

      INTEGER, PARAMETER :: K = 4, M = 3
      INTEGER :: I, J, P
      TYPE(DT2(K,M)) :: n1(1)
      CLASS(DT2(K,:)), ALLOCATABLE :: n2(:)

      CALL CreateNewBase(n1)

      IF ( SIZE(n1) .NE. 1 ) STOP 40
      DO I = 1, SIZE(n1)
           IF ( n1(I)%k3 .NE. K ) STOP 41
           IF ( n1(I)%l3 .NE. M ) STOP 42
           IF ( SIZE(n1(I)%A21)     .NE.   M ) STOP 43
           IF ( SIZE(n1(I)%A22)     .NE. M+2 ) STOP 44
           IF ( LBOUND(n1(I)%A21,1) .NE.   1 ) STOP 45
           IF ( LBOUND(n1(I)%A22,1) .NE.   1 ) STOP 46
           IF ( UBOUND(n1(I)%A21,1) .NE.   M ) STOP 47
           IF ( UBOUND(n1(I)%A22,1) .NE. M+2 ) STOP 48
           IF ( ANY(n1(I)%A21 .NE. 11) ) STOP 49
           IF ( ANY(n1(I)%A22 .NE. 22) ) STOP 50

           IF ( SIZE(n1(I)%cmp) .NE. M ) STOP 51
           DO J = 1, SIZE(n1(I)%cmp)
                IF ( n1(I)%cmp(J)%k2 .NE. n1(I)%k3 ) STOP 52
                IF ( n1(I)%cmp(J)%l2 .NE. n1(I)%l3 ) STOP 53
                IF ( SIZE(n1(I)%cmp(J)%A1)     .NE. M-1 ) STOP 54
                IF ( LBOUND(n1(I)%cmp(J)%A1,1) .NE.   1 ) STOP 55
                IF ( UBOUND(n1(I)%cmp(J)%A1,1) .NE. M-1 ) STOP 56
                IF ( n1(I)%cmp(J)%cmp%k1  .NE. n1(I)%k3 ) STOP 57
                IF ( n1(I)%cmp(J)%cmp%l1  .NE. n1(I)%l3 ) STOP 58
                IF ( SIZE(n1(I)%cmp(J)%cmp%A01)     .NE.   M ) STOP 59
                IF ( SIZE(n1(I)%cmp(J)%cmp%A02)     .NE. M+1 ) STOP 60
                IF ( LBOUND(n1(I)%cmp(J)%cmp%A01,1) .NE.   1 ) STOP 61
                IF ( LBOUND(n1(I)%cmp(J)%cmp%A02,1) .NE.   1 ) STOP 62
                IF ( UBOUND(n1(I)%cmp(J)%cmp%A01,1) .NE.   M ) STOP 63
                IF ( UBOUND(n1(I)%cmp(J)%cmp%A02,1) .NE. M+1 ) STOP 64
                IF ( ANY(n1(I)%cmp(J)%A1      .NE. [1, 2]) ) STOP 65
                IF ( ANY(n1(I)%cmp(J)%cmp%A01 .NE.     -1) ) STOP 66
                IF ( ANY(n1(I)%cmp(J)%cmp%A02 .NE.     -2) ) STOP 67
           END DO
      END DO

      ALLOCATE( DT2(K,M) :: n2(1) )
      CALL CreateNewBase(n2)

      IF ( SIZE(n2) .NE. 1 ) STOP 70
      DO I = 1, SIZE(n2)
           IF ( n2(I)%k3 .NE. K ) STOP 71
           IF ( n2(I)%l3 .NE. M ) STOP 72
           IF ( SIZE(n2(I)%A21)     .NE.   M ) STOP 73
           IF ( SIZE(n2(I)%A22)     .NE. M+2 ) STOP 74
           IF ( LBOUND(n2(I)%A21,1) .NE.   1 ) STOP 75
           IF ( LBOUND(n2(I)%A22,1) .NE.   1 ) STOP 76
           IF ( UBOUND(n2(I)%A21,1) .NE.   M ) STOP 77
           IF ( UBOUND(n2(I)%A22,1) .NE. M+2 ) STOP 78
           IF ( ANY(n2(I)%A21 .NE. 11) ) STOP 79
           IF ( ANY(n2(I)%A22 .NE. 22) ) STOP 80

           IF ( SIZE(n2(I)%cmp) .NE. M ) STOP 81
           DO J = 1, SIZE(n2(I)%cmp)
                IF ( n2(I)%cmp(J)%k2 .NE. n2(I)%k3 ) STOP 82
                IF ( n2(I)%cmp(J)%l2 .NE. n2(I)%l3 ) STOP 83
                IF ( SIZE(n2(I)%cmp(J)%A1)     .NE. M-1 ) STOP 84
                IF ( LBOUND(n2(I)%cmp(J)%A1,1) .NE.   1 ) STOP 85
                IF ( UBOUND(n2(I)%cmp(J)%A1,1) .NE. M-1 ) STOP 86
                IF ( n2(I)%cmp(J)%cmp%k1  .NE. n2(I)%k3 ) STOP 87
                IF ( n2(I)%cmp(J)%cmp%l1  .NE. n2(I)%l3 ) STOP 88
                IF ( SIZE(n2(I)%cmp(J)%cmp%A01)     .NE.   M ) STOP 89
                IF ( SIZE(n2(I)%cmp(J)%cmp%A02)     .NE. M+1 ) STOP 90
                IF ( LBOUND(n2(I)%cmp(J)%cmp%A01,1) .NE.   1 ) STOP 91
                IF ( LBOUND(n2(I)%cmp(J)%cmp%A02,1) .NE.   1 ) STOP 92
                IF ( UBOUND(n2(I)%cmp(J)%cmp%A01,1) .NE.   M ) STOP 93
                IF ( UBOUND(n2(I)%cmp(J)%cmp%A02,1) .NE. M+1 ) STOP 94
                IF ( ANY(n2(I)%cmp(J)%A1      .NE. [1, 2]) ) STOP 95
                IF ( ANY(n2(I)%cmp(J)%cmp%A01 .NE.     -1) ) STOP 96
                IF ( ANY(n2(I)%cmp(J)%cmp%A02 .NE.     -2) ) STOP 97
           END DO
      END DO

END PROGRAM DTP_ACE_10
