!*  ===================================================================
!*
!*  DATE                       : April 24, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Array constructor with Type Specification
!*  SECONDARY FUNCTIONS TESTED : ACE unfolding at compile time
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!* Defect : 364760
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
        TYPE(Base(k2,l2)) :: bcmp(l2)
      END TYPE

      TYPE :: DT2 (k3,l3)
        INTEGER, KIND :: k3 = 4
        INTEGER, LEN  :: l3 = 2

        INTEGER(k3) :: A21(l3) = -4, A22(l3+2) = -5
        TYPE(DT1(k3,l3)) :: dcmp(l3)
      END TYPE

END MODULE
PROGRAM DTP_ACE_09
      USE Mod
      IMPLICIT NONE

      INTEGER, PARAMETER :: K = 4, M = 3
      INTEGER :: I, J, P
      TYPE(DT2(K,M)), PARAMETER :: n1(1) = [DT2(K,M) :: DT2(K,M) (11, 22, [DT1(K,M) :: DT1(K,M)([ &
        (I, I = 1, M-1)], Base(K,M)()), DT1(K,M)(3, Base(K,M)(1, 2)), DT1(K,M)(bcmp=Base(K,M)() )] )]
      TYPE(DT2(K,:)), ALLOCATABLE :: n2(:)

      IF ( SIZE(n1) .NE. 1 ) STOP 10
      DO I = 1, SIZE(n1)
           IF ( n1(I)%k3 .NE. K ) STOP 11
           IF ( n1(I)%l3 .NE. M ) STOP 12
           IF ( SIZE(n1(I)%A21)     .NE.   M ) STOP 13
           IF ( SIZE(n1(I)%A22)     .NE. M+2 ) STOP 14
           IF ( LBOUND(n1(I)%A21,1) .NE.   1 ) STOP 15
           IF ( LBOUND(n1(I)%A22,1) .NE.   1 ) STOP 16
           IF ( UBOUND(n1(I)%A21,1) .NE.   M ) STOP 17
           IF ( UBOUND(n1(I)%A22,1) .NE. M+2 ) STOP 18
           IF ( ANY(n1(I)%A21 .NE. 11) ) STOP 19
           IF ( ANY(n1(I)%A22 .NE. 22) ) STOP 20
           IF ( ANY(n1(I)%dcmp(1)%A1 .NE. [(I, I = 1, M-1)]) ) STOP 21
           IF ( ANY(n1(I)%dcmp(2)%A1 .NE.                 3) ) STOP 22
           IF ( ANY(n1(I)%dcmp(3)%A1 .NE.                -3) ) STOP 23

           IF ( SIZE(n1(I)%dcmp) .NE. M ) STOP 24
           DO J = 1, SIZE(n1(I)%dcmp)
                IF ( n1(I)%dcmp(J)%k2 .NE. n1(I)%k3 ) STOP 30
                IF ( n1(I)%dcmp(J)%l2 .NE. n1(I)%l3 ) STOP 31
                IF ( SIZE(n1(I)%dcmp(J)%A1)     .NE. M-1 ) STOP 32
                IF ( LBOUND(n1(I)%dcmp(J)%A1,1) .NE.   1 ) STOP 33
                IF ( UBOUND(n1(I)%dcmp(J)%A1,1) .NE. M-1 ) STOP 34
                IF ( SIZE(n1(I)%dcmp(J)%bcmp)   .NE.   M ) STOP 35
                DO P = 1, SIZE(n1(I)%dcmp(J)%bcmp)
                    IF ( n1(I)%dcmp(J)%bcmp(P)%k1  .NE. n1(I)%k3 ) STOP 36
                    IF ( n1(I)%dcmp(J)%bcmp(P)%l1  .NE. n1(I)%l3 ) STOP 37
                    IF ( SIZE(n1(I)%dcmp(J)%bcmp(P)%A01)     .NE.   M ) STOP 38
                    IF ( SIZE(n1(I)%dcmp(J)%bcmp(P)%A02)     .NE. M+1 ) STOP 39
                    IF ( LBOUND(n1(I)%dcmp(J)%bcmp(P)%A01,1) .NE.   1 ) STOP 40
                    IF ( LBOUND(n1(I)%dcmp(J)%bcmp(P)%A02,1) .NE.   1 ) STOP 41
                    IF ( UBOUND(n1(I)%dcmp(J)%bcmp(P)%A01,1) .NE.   M ) STOP 42
                    IF ( UBOUND(n1(I)%dcmp(J)%bcmp(P)%A02,1) .NE. M+1 ) STOP 43
                    IF ( ANY(n1(I)%dcmp(1)%bcmp(P)%A01       .NE. -1) ) STOP 44
                    IF ( ANY(n1(I)%dcmp(1)%bcmp(P)%A02       .NE. -2) ) STOP 45
                    IF ( ANY(n1(I)%dcmp(2)%bcmp(P)%A01       .NE.  1) ) STOP 46
                    IF ( ANY(n1(I)%dcmp(2)%bcmp(P)%A02       .NE.  2) ) STOP 47
                    IF ( ANY(n1(I)%dcmp(3)%bcmp(P)%A01       .NE. -1) ) STOP 48
                    IF ( ANY(n1(I)%dcmp(3)%bcmp(P)%A02       .NE. -2) ) STOP 49
                END DO
           END DO
      END DO

      ALLOCATE( n2(2), SOURCE = [n1, n1] )

      IF ( SIZE(n2) .NE. 2 ) STOP 50
      DO I = 1, SIZE(n2)
           IF ( n2(I)%k3 .NE. K ) STOP 51
           IF ( n2(I)%l3 .NE. M ) STOP 52
           IF ( SIZE(n2(I)%A21)     .NE.   M ) STOP 53
           IF ( SIZE(n2(I)%A22)     .NE. M+2 ) STOP 54
           IF ( LBOUND(n2(I)%A21,1) .NE.   1 ) STOP 55
           IF ( LBOUND(n2(I)%A22,1) .NE.   1 ) STOP 56
           IF ( UBOUND(n2(I)%A21,1) .NE.   M ) STOP 57
           IF ( UBOUND(n2(I)%A22,1) .NE. M+2 ) STOP 58
           IF ( ANY(n2(I)%A21 .NE. 11) ) STOP 59
           IF ( ANY(n2(I)%A22 .NE. 22) ) STOP 60
           IF ( ANY(n2(I)%dcmp(1)%A1 .NE. [(I, I = 1, M-1)]) ) STOP 61
           IF ( ANY(n2(I)%dcmp(2)%A1 .NE.                 3) ) STOP 62
           IF ( ANY(n2(I)%dcmp(3)%A1 .NE.                -3) ) STOP 63

           IF ( SIZE(n2(I)%dcmp) .NE. M ) STOP 64
           DO J = 1, SIZE(n2(I)%dcmp)
                IF ( n2(I)%dcmp(J)%k2 .NE. n2(I)%k3 ) STOP 70
                IF ( n2(I)%dcmp(J)%l2 .NE. n2(I)%l3 ) STOP 71
                IF ( SIZE(n2(I)%dcmp(J)%A1)     .NE. M-1 ) STOP 72
                IF ( LBOUND(n2(I)%dcmp(J)%A1,1) .NE.   1 ) STOP 73
                IF ( UBOUND(n2(I)%dcmp(J)%A1,1) .NE. M-1 ) STOP 74
                IF ( SIZE(n2(I)%dcmp(J)%bcmp)   .NE.   M ) STOP 75
                DO P = 1, SIZE(n2(I)%dcmp(J)%bcmp)
                    IF ( n2(I)%dcmp(J)%bcmp(P)%k1  .NE. n2(I)%k3 ) STOP 76
                    IF ( n2(I)%dcmp(J)%bcmp(P)%l1  .NE. n2(I)%l3 ) STOP 77
                    IF ( SIZE(n2(I)%dcmp(J)%bcmp(P)%A01)     .NE.   M ) STOP 78
                    IF ( SIZE(n2(I)%dcmp(J)%bcmp(P)%A02)     .NE. M+1 ) STOP 79
                    IF ( LBOUND(n2(I)%dcmp(J)%bcmp(P)%A01,1) .NE.   1 ) STOP 80
                    IF ( LBOUND(n2(I)%dcmp(J)%bcmp(P)%A02,1) .NE.   1 ) STOP 81
                    IF ( UBOUND(n2(I)%dcmp(J)%bcmp(P)%A01,1) .NE.   M ) STOP 82
                    IF ( UBOUND(n2(I)%dcmp(J)%bcmp(P)%A02,1) .NE. M+1 ) STOP 83
                    IF ( ANY(n2(I)%dcmp(1)%bcmp(P)%A01       .NE. -1) ) STOP 84
                    IF ( ANY(n2(I)%dcmp(1)%bcmp(P)%A02       .NE. -2) ) STOP 85
                    IF ( ANY(n2(I)%dcmp(2)%bcmp(P)%A01       .NE.  1) ) STOP 86
                    IF ( ANY(n2(I)%dcmp(2)%bcmp(P)%A02       .NE.  2) ) STOP 87
                    IF ( ANY(n2(I)%dcmp(3)%bcmp(P)%A01       .NE. -1) ) STOP 88
                    IF ( ANY(n2(I)%dcmp(3)%bcmp(P)%A02       .NE. -2) ) STOP 89
                END DO
           END DO
      END DO

END PROGRAM DTP_ACE_09
