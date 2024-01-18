!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : DTP_ACE_06.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : April 24, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array constructor with Type Specification 
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DRIVER STANZA              : xlf2003
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
        TYPE(Base(k2,l2)) :: cmp
      END TYPE

      TYPE :: DT2 (k3,l3)
        INTEGER, KIND :: k3 = 4
        INTEGER, LEN  :: l3 = 2

        INTEGER(k3) :: A21(l3) = -4, A22(l3+2) = -5 
        TYPE(DT1(k3,l3)) :: cmp(l3)
      END TYPE

END MODULE
PROGRAM DTP_ACE_06
      USE Mod
      IMPLICIT NONE

      INTEGER, PARAMETER :: K =4 , N = 10, M = 3
      INTEGER :: I, J
      TYPE(DT1(K,M)) :: d1 = DT1(K,M)( [(I, I = 1, M-1)], Base(K,M)() ),   &
                        d2 = DT1(K,M)( 3*N, Base(K,M)([(1, I = 1, M)],     &
                                       [(2, I = 1, M+1)]) )
      TYPE(DT2(K,M)) :: n1(2)
      TYPE(DT2(K,m)), ALLOCATABLE :: n2(:)

      n1 = [DT2(K,M) :: DT2(K,M) (11, 22, d1), DT2(K,M) (33, 44, d2)]
      IF ( SIZE(n1) .NE. 2 ) STOP 10
      DO I = 1, SIZE(n1)
           IF ( n1(I)%k3 .NE. K ) STOP 11
           IF ( n1(I)%l3 .NE. M ) STOP 12
           IF ( SIZE(n1(I)%A21)     .NE.   M ) STOP 13
           IF ( SIZE(n1(I)%A22)     .NE. M+2 ) STOP 14
           IF ( LBOUND(n1(I)%A21,1) .NE.   1 ) STOP 15
           IF ( LBOUND(n1(I)%A22,1) .NE.   1 ) STOP 16
           IF ( UBOUND(n1(I)%A21,1) .NE.   M ) STOP 17
           IF ( UBOUND(n1(I)%A22,1) .NE. M+2 ) STOP 18

           IF ( SIZE(n1(I)%cmp) .NE. M ) STOP 19
           DO J = 1, SIZE(n1(I)%cmp)
                IF ( n1(I)%cmp(J)%k2 .NE. n1(I)%k3 ) STOP 20
                IF ( n1(I)%cmp(J)%l2 .NE. n1(I)%l3 ) STOP 21
                IF ( SIZE(n1(I)%cmp(J)%A1)     .NE. M-1 ) STOP 22
                IF ( LBOUND(n1(I)%cmp(J)%A1,1) .NE.   1 ) STOP 23
                IF ( UBOUND(n1(I)%cmp(J)%A1,1) .NE. M-1 ) STOP 24
                IF ( n1(I)%cmp(J)%cmp%k1  .NE. n1(I)%k3 ) STOP 25
                IF ( n1(I)%cmp(J)%cmp%l1  .NE. n1(I)%l3 ) STOP 26
                IF ( SIZE(n1(I)%cmp(J)%cmp%A01)     .NE.   M ) STOP 27
                IF ( SIZE(n1(I)%cmp(J)%cmp%A02)     .NE. M+1 ) STOP 28
                IF ( LBOUND(n1(I)%cmp(J)%cmp%A01,1) .NE.   1 ) STOP 29
                IF ( LBOUND(n1(I)%cmp(J)%cmp%A02,1) .NE.   1 ) STOP 30
                IF ( UBOUND(n1(I)%cmp(J)%cmp%A01,1) .NE.   M ) STOP 31
                IF ( UBOUND(n1(I)%cmp(J)%cmp%A02,1) .NE. M+1 ) STOP 32
           END DO
      END DO
      IF ( ANY(n1(1)%A21 .NE. 11) ) STOP 33
      IF ( ANY(n1(1)%A22 .NE. 22) ) STOP 34
      IF ( ANY(n1(2)%A21 .NE. 33) ) STOP 35
      IF ( ANY(n1(2)%A22 .NE. 44) ) STOP 36
      IF ( ANY(n1(1)%cmp(1)%A1      .NE. [(I, I = 1, M-1)]) ) STOP 37
      IF ( ANY(n1(1)%cmp(1)%cmp%A01 .NE.                -1) ) STOP 38
      IF ( ANY(n1(1)%cmp(1)%cmp%A02 .NE.                -2) ) STOP 39
      IF ( ANY(n1(1)%cmp(2)%A1      .NE. [(I, I = 1, M-1)]) ) STOP 40
      IF ( ANY(n1(1)%cmp(2)%cmp%A01 .NE.                -1) ) STOP 41
      IF ( ANY(n1(1)%cmp(2)%cmp%A02 .NE.                -2) ) STOP 42
      IF ( ANY(n1(1)%cmp(3)%A1      .NE. [(I, I = 1, M-1)]) ) STOP 43
      IF ( ANY(n1(1)%cmp(3)%cmp%A01 .NE.                -1) ) STOP 44
      IF ( ANY(n1(1)%cmp(3)%cmp%A02 .NE.                -2) ) STOP 45
      IF ( ANY(n1(2)%cmp(1)%A1      .NE.               3*N) ) STOP 46
      IF ( ANY(n1(2)%cmp(1)%cmp%A01 .NE.                 1) ) STOP 47
      IF ( ANY(n1(2)%cmp(1)%cmp%A02 .NE.                 2) ) STOP 48
      IF ( ANY(n1(2)%cmp(2)%A1      .NE.               3*N) ) STOP 49
      IF ( ANY(n1(2)%cmp(2)%cmp%A01 .NE.                 1) ) STOP 50
      IF ( ANY(n1(2)%cmp(2)%cmp%A02 .NE.                 2) ) STOP 51
      IF ( ANY(n1(2)%cmp(3)%A1      .NE.               3*N) ) STOP 52
      IF ( ANY(n1(2)%cmp(3)%cmp%A01 .NE.                 1) ) STOP 53
      IF ( ANY(n1(2)%cmp(3)%cmp%A02 .NE.                 2) ) STOP 54

      ALLOCATE( n2(2), SOURCE = n1 )
      IF ( SIZE(n2) .NE. 2 ) STOP 60
      DO I = 1, SIZE(n2)
           IF ( n2(I)%k3 .NE. K ) STOP 61
           IF ( n2(I)%l3 .NE. M ) STOP 62
           IF ( SIZE(n2(I)%A21)     .NE.   M ) STOP 63
           IF ( SIZE(n2(I)%A22)     .NE. M+2 ) STOP 64
           IF ( LBOUND(n2(I)%A21,1) .NE.   1 ) STOP 65
           IF ( LBOUND(n2(I)%A22,1) .NE.   1 ) STOP 66
           IF ( UBOUND(n2(I)%A21,1) .NE.   M ) STOP 67
           IF ( UBOUND(n2(I)%A22,1) .NE. M+2 ) STOP 68

           IF ( SIZE(n2(I)%cmp) .NE. M ) STOP 69
           DO J = 1, SIZE(n2(I)%cmp)
                IF ( n2(I)%cmp(J)%k2 .NE. n2(I)%k3 ) STOP 70
                IF ( n2(I)%cmp(J)%l2 .NE. n2(I)%l3 ) STOP 71
                IF ( SIZE(n2(I)%cmp(J)%A1)     .NE. M-1 ) STOP 72
                IF ( LBOUND(n2(I)%cmp(J)%A1,1) .NE.   1 ) STOP 73
                IF ( UBOUND(n2(I)%cmp(J)%A1,1) .NE. M-1 ) STOP 74
                IF ( n2(I)%cmp(J)%cmp%k1  .NE. n2(I)%k3 ) STOP 75
                IF ( n2(I)%cmp(J)%cmp%l1  .NE. n2(I)%l3 ) STOP 76
                IF ( SIZE(n2(I)%cmp(J)%cmp%A01)     .NE.   M ) STOP 77
                IF ( SIZE(n2(I)%cmp(J)%cmp%A02)     .NE. M+1 ) STOP 78
                IF ( LBOUND(n2(I)%cmp(J)%cmp%A01,1) .NE.   1 ) STOP 79
                IF ( LBOUND(n2(I)%cmp(J)%cmp%A02,1) .NE.   1 ) STOP 80
                IF ( UBOUND(n2(I)%cmp(J)%cmp%A01,1) .NE.   M ) STOP 81
                IF ( UBOUND(n2(I)%cmp(J)%cmp%A02,1) .NE. M+1 ) STOP 82
           END DO
      END DO
      IF ( ANY(n2(1)%A21 .NE. 11) ) STOP 83
      IF ( ANY(n2(1)%A22 .NE. 22) ) STOP 84
      IF ( ANY(n2(2)%A21 .NE. 33) ) STOP 85
      IF ( ANY(n2(2)%A22 .NE. 44) ) STOP 86
      IF ( ANY(n2(1)%cmp(1)%A1      .NE. [(I, I = 1, M-1)]) ) STOP 87
      IF ( ANY(n2(1)%cmp(1)%cmp%A01 .NE.                -1) ) STOP 88
      IF ( ANY(n2(1)%cmp(1)%cmp%A02 .NE.                -2) ) STOP 89
      IF ( ANY(n2(1)%cmp(2)%A1      .NE. [(I, I = 1, M-1)]) ) STOP 90
      IF ( ANY(n2(1)%cmp(2)%cmp%A01 .NE.                -1) ) STOP 91
      IF ( ANY(n2(1)%cmp(2)%cmp%A02 .NE.                -2) ) STOP 92
      IF ( ANY(n2(1)%cmp(3)%A1      .NE. [(I, I = 1, M-1)]) ) STOP 93
      IF ( ANY(n2(1)%cmp(3)%cmp%A01 .NE.                -1) ) STOP 94
      IF ( ANY(n2(1)%cmp(3)%cmp%A02 .NE.                -2) ) STOP 95
      IF ( ANY(n2(2)%cmp(1)%A1      .NE.               3*N) ) STOP 96
      IF ( ANY(n2(2)%cmp(1)%cmp%A01 .NE.                 1) ) STOP 97
      IF ( ANY(n2(2)%cmp(1)%cmp%A02 .NE.                 2) ) STOP 98
      IF ( ANY(n2(2)%cmp(2)%A1      .NE.               3*N) ) STOP 99
      IF ( ANY(n2(2)%cmp(2)%cmp%A01 .NE.                 1) ) STOP 100
      IF ( ANY(n2(2)%cmp(2)%cmp%A02 .NE.                 2) ) STOP 101
      IF ( ANY(n2(2)%cmp(3)%A1      .NE.               3*N) ) STOP 102
      IF ( ANY(n2(2)%cmp(3)%cmp%A01 .NE.                 1) ) STOP 103
      IF ( ANY(n2(2)%cmp(3)%cmp%A02 .NE.                 2) ) STOP 104

END PROGRAM DTP_ACE_06
