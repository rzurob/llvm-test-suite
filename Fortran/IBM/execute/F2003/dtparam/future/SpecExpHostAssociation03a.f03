!*  ===================================================================
!*
!*  DATE                       : June 14, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Speification expression - Host Association
!*  SECONDARY FUNCTIONS TESTED : Default Initialization
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  An objet designator with a base object that is made accessible by host association
!*
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM SpeExpHostAssociation03a
      IMPLICIT NONE

      TYPE Base (k1,l1)
         INTEGER, KIND :: k1 = 2
         INTEGER, LEN  :: l1 = 2

         INTEGER(k1) :: I1 = k1, A1(l1) = 2*k1
         CHARACTER(l1) :: Carr = 'B'
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
         INTEGER, KIND :: k2 = 4
         INTEGER, LEN  :: l2 = 4

         TYPE(Base(k2,k2)) :: b1 = Base(k2,k2) ( )
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen (k3,l3)
         INTEGER, KIND :: k3 = 8
         INTEGER, LEN  :: l3 = 8

         TYPE(Child(k3,k2,k3,k1)) :: c1 = Child(k3,k2,k3,k1) ( )
      END TYPE

      INTEGER I
      TYPE(NextGen) :: n1
      TYPE(NextGen(4,3,4,5,4,7)) :: n2

      CALL Sub11(8)
      CALL Sub12(8)
      CALL Sub13(8,8)
      CALL Sub14(8,16,16)
      CALL Sub15(8,8,8)

      n1 = NextGen ( b1 = Base(4,4)(I1 = 10, A1 = 20),  &
                        c1 = Child(8,4,8,2)( b1 = Base(8,8)(I1 = 11, A1 = [(2*I, I =1,n1%k3)]) ) )
      CALL Sub11(8)
      CALL Sub12(8)
      CALL Sub13(8,11)
      CALL Sub14(8,2,4)
      CALL Sub15(8,8,11)

      CALL Sub21(4)
      CALL Sub22(4)
      CALL Sub23(4,4)
      CALL Sub24(4,8,8)
      CALL Sub25(4,4,4)

      n2 = NextGen(4,3,4,5,4,7) ( b1 = Base(4,4)(I1 = 10, A1 = 20),  &
                        c1 = Child(4,4,4,4)(b1 = Base(4,4)(I1 = 22, A1 = [(4*I, I =1,n1%k3)]))  )
      CALL Sub21(4)
      CALL Sub22(4)
      CALL Sub23(4,22)
      CALL Sub24(4,4,12)
      CALL Sub25(4,4,22)

      CONTAINS

      SUBROUTINE Sub11(N)
        INTEGER :: N
        TYPE(Base(n1%c1%b1%k1,n1%c1%b1%l1)) :: Obj

         IF ( Obj%k1 .NE. N ) ERROR STOP 10
         IF ( Obj%l1 .NE. N ) ERROR STOP 11
         IF ( Obj%I1 .NE. N ) ERROR STOP 12
         IF ( SIZE(Obj%A1)   .NE.    N ) ERROR STOP 13
         IF ( ANY(Obj%A1     .NE. 2*N) ) ERROR STOP 14
         IF ( LEN(Obj%Carr)  .NE.    N ) ERROR STOP 15
         IF ( TRIM(Obj%Carr) .NE.  'B' ) ERROR STOP 16
      END SUBROUTINE Sub11

      SUBROUTINE Sub12(N)
        INTEGER :: N
        TYPE(Base(n1%c1%b1%l1,2*n1%c1%b1%k1)) :: Obj

         IF ( Obj%k1 .NE.   N ) ERROR STOP 17
         IF ( Obj%l1 .NE. 2*N ) ERROR STOP 18
         IF ( Obj%I1 .NE.   N ) ERROR STOP 19
         IF ( SIZE(Obj%A1)   .NE.  2*N ) ERROR STOP 20
         IF ( ANY(Obj%A1     .NE. 2*N) ) ERROR STOP 21
         IF ( LEN(Obj%Carr)  .NE.  2*N ) ERROR STOP 22
         IF ( TRIM(Obj%Carr) .NE.  'B' ) ERROR STOP 23
      END SUBROUTINE Sub12

      SUBROUTINE Sub13(N, M)
        INTEGER :: N, M
        TYPE(Child(KIND(n1%c1%b1%I1),n1%c1%b1%I1,KIND(n1%c1%b1%I1),2*n1%c1%b1%I1)) :: Obj

         IF ( Obj%k1 .NE.   N ) ERROR STOP 24
         IF ( Obj%l1 .NE.   M ) ERROR STOP 25
         IF ( Obj%k2 .NE.   N ) ERROR STOP 26
         IF ( Obj%l2 .NE. 2*M ) ERROR STOP 27
         IF ( Obj%I1 .NE.   N ) ERROR STOP 28
         IF ( SIZE(Obj%A1)   .NE.    M ) ERROR STOP 29
         IF ( ANY(Obj%A1     .NE. 2*N) ) ERROR STOP 30
         IF ( LEN(Obj%Carr)  .NE.    M ) ERROR STOP 31
         IF ( TRIM(Obj%Carr) .NE.  'B' ) ERROR STOP 32

         IF ( Obj%b1%k1 .NE.   N ) ERROR STOP 33
         IF ( Obj%b1%l1 .NE.   N ) ERROR STOP 34
         IF ( Obj%b1%I1 .NE.   N ) ERROR STOP 35
         IF ( SIZE(Obj%b1%A1)   .NE.    N ) ERROR STOP 36
         IF ( ANY(Obj%b1%A1     .NE. 2*N) ) ERROR STOP 37
         IF ( LEN(Obj%b1%Carr)  .NE.    N ) ERROR STOP 38
         IF ( TRIM(Obj%b1%Carr) .NE.  'B' ) ERROR STOP 39
      END SUBROUTINE Sub13

      SUBROUTINE Sub14(N, M, P)
        INTEGER :: N, M, P
        TYPE(Child(KIND(n1%c1%b1%A1(1)),n1%c1%b1%A1(1),KIND(n1%c1%b1%A1(n1%l1)),n1%c1%b1%A1(n1%l1))) :: Obj

         IF ( Obj%k1 .NE. N ) ERROR STOP 40
         IF ( Obj%l1 .NE. M ) ERROR STOP 41
         IF ( Obj%k2 .NE. N ) ERROR STOP 42
         IF ( Obj%l2 .NE. P ) ERROR STOP 43
         IF ( Obj%I1 .NE. N ) ERROR STOP 44
         IF ( SIZE(Obj%A1)   .NE.    M ) ERROR STOP 45
         IF ( ANY(Obj%A1     .NE. 2*N) ) ERROR STOP 46
         IF ( LEN(Obj%Carr)  .NE.    M ) ERROR STOP 47
         IF ( TRIM(Obj%Carr) .NE.  'B' ) ERROR STOP 48

         IF ( Obj%b1%k1 .NE.   N ) ERROR STOP 49
         IF ( Obj%b1%l1 .NE.   N ) ERROR STOP 50
         IF ( Obj%b1%I1 .NE.   N ) ERROR STOP 51
         IF ( SIZE(Obj%b1%A1)   .NE.    N ) ERROR STOP 52
         IF ( ANY(Obj%b1%A1     .NE. 2*N) ) ERROR STOP 53
         IF ( LEN(Obj%b1%Carr)  .NE.    N ) ERROR STOP 54
         IF ( TRIM(Obj%b1%Carr) .NE.  'B' ) ERROR STOP 55
      END SUBROUTINE Sub14

      SUBROUTINE Sub15(N, M, P)
        INTEGER :: N, M, P, I
        TYPE(Base(KIND(n1%c1%b1%I1),LEN(n1%c1%b1%Carr))) :: Obj(n1%c1%b1%I1)

         IF ( SIZE(Obj) .NE. P ) ERROR STOP 56
         DO I = 1, n1%c1%b1%I1
            IF ( Obj(I)%k1 .NE. N ) ERROR STOP 57
            IF ( Obj(I)%l1 .NE. M ) ERROR STOP 58
            IF ( Obj(I)%I1 .NE. N ) ERROR STOP 59
            IF ( SIZE(Obj(I)%A1)   .NE.    M ) ERROR STOP 60
            IF ( ANY(Obj(I)%A1     .NE. 2*N) ) ERROR STOP 61
            IF ( LEN(Obj(I)%Carr)  .NE.    N ) ERROR STOP 62
            IF ( TRIM(Obj(I)%Carr) .NE.  'B' ) ERROR STOP 63
         END DO
      END SUBROUTINE Sub15

      SUBROUTINE Sub21(N)
        INTEGER :: N
        TYPE(Base(n2%c1%b1%k1,n2%c1%b1%l1)) :: Obj

         IF ( Obj%k1 .NE. N ) ERROR STOP 70
         IF ( Obj%l1 .NE. N ) ERROR STOP 71
         IF ( Obj%I1 .NE. N ) ERROR STOP 72
         IF ( SIZE(Obj%A1)   .NE.    N ) ERROR STOP 73
         IF ( ANY(Obj%A1     .NE. 2*N) ) ERROR STOP 74
         IF ( LEN(Obj%Carr)  .NE.    N ) ERROR STOP 75
         IF ( TRIM(Obj%Carr) .NE.  'B' ) ERROR STOP 76
      END SUBROUTINE Sub21

      SUBROUTINE Sub22(N)
        INTEGER :: N
        TYPE(Base(n2%c1%b1%l1,2*n2%c1%b1%k1)) :: Obj

         IF ( Obj%k1 .NE.   N ) ERROR STOP 77
         IF ( Obj%l1 .NE. 2*N ) ERROR STOP 78
         IF ( Obj%I1 .NE.   N ) ERROR STOP 79
         IF ( SIZE(Obj%A1)   .NE.  2*N ) ERROR STOP 80
         IF ( ANY(Obj%A1     .NE. 2*N) ) ERROR STOP 81
         IF ( LEN(Obj%Carr)  .NE.  2*N ) ERROR STOP 82
         IF ( TRIM(Obj%Carr) .NE.  'B' ) ERROR STOP 83
      END SUBROUTINE Sub22

      SUBROUTINE Sub23(N, M)
        INTEGER :: N, M
        TYPE(Child(KIND(n2%c1%b1%I1),n2%c1%b1%I1,KIND(n2%c1%b1%I1),2*n2%c1%b1%I1)) :: Obj

         IF ( Obj%k1 .NE.   N ) ERROR STOP 84
         IF ( Obj%l1 .NE.   M ) ERROR STOP 85
         IF ( Obj%k2 .NE.   N ) ERROR STOP 86
         IF ( Obj%l2 .NE. 2*M ) ERROR STOP 87
         IF ( Obj%I1 .NE.   N ) ERROR STOP 88
         IF ( SIZE(Obj%A1)   .NE.    M ) ERROR STOP 89
         IF ( ANY(Obj%A1     .NE. 2*N) ) ERROR STOP 90
         IF ( LEN(Obj%Carr)  .NE.    M ) ERROR STOP 91
         IF ( TRIM(Obj%Carr) .NE.  'B' ) ERROR STOP 92

         IF ( Obj%b1%k1 .NE.   N ) ERROR STOP 93
         IF ( Obj%b1%l1 .NE.   N ) ERROR STOP 94
         IF ( Obj%b1%I1 .NE.   N ) ERROR STOP 95
         IF ( SIZE(Obj%b1%A1)   .NE.    N ) ERROR STOP 96
         IF ( ANY(Obj%b1%A1     .NE. 2*N) ) ERROR STOP 97
         IF ( LEN(Obj%b1%Carr)  .NE.    N ) ERROR STOP 98
         IF ( TRIM(Obj%b1%Carr) .NE.  'B' ) ERROR STOP 99
      END SUBROUTINE Sub23

      SUBROUTINE Sub24(N, M, P)
        INTEGER :: N, M, P
        TYPE(Child(KIND(n2%c1%b1%A1(1)),n2%c1%b1%A1(1),KIND(n2%c1%b1%A1(n2%l1)),n2%c1%b1%A1(n2%l1))) :: Obj

         IF ( Obj%k1 .NE. N ) ERROR STOP 100
         IF ( Obj%l1 .NE. M ) ERROR STOP 101
         IF ( Obj%k2 .NE. N ) ERROR STOP 102
         IF ( Obj%l2 .NE. P ) ERROR STOP 103
         IF ( Obj%I1 .NE. N ) ERROR STOP 104
         IF ( SIZE(Obj%A1)   .NE.    M ) ERROR STOP 105
         IF ( ANY(Obj%A1     .NE. 2*N) ) ERROR STOP 106
         IF ( LEN(Obj%Carr)  .NE.    M ) ERROR STOP 107
         IF ( TRIM(Obj%Carr) .NE.  'B' ) ERROR STOP 108

         IF ( Obj%b1%k1 .NE.   N ) ERROR STOP 109
         IF ( Obj%b1%l1 .NE.   N ) ERROR STOP 110
         IF ( Obj%b1%I1 .NE.   N ) ERROR STOP 111
         IF ( SIZE(Obj%b1%A1)   .NE.    N ) ERROR STOP 112
         IF ( ANY(Obj%b1%A1     .NE. 2*N) ) ERROR STOP 113
         IF ( LEN(Obj%b1%Carr)  .NE.    N ) ERROR STOP 114
         IF ( TRIM(Obj%b1%Carr) .NE.  'B' ) ERROR STOP 115
      END SUBROUTINE Sub24

      SUBROUTINE Sub25(N, M, P)
        INTEGER :: N, M, P, I
        TYPE(Base(KIND(n2%c1%b1%I1),LEN(n2%c1%b1%Carr))) :: Obj(n2%c1%b1%I1)

         IF ( SIZE(Obj) .NE. P ) ERROR STOP 116
         DO I = 1, n2%c1%b1%I1
            IF ( Obj(I)%k1 .NE. N ) ERROR STOP 117
            IF ( Obj(I)%l1 .NE. M ) ERROR STOP 118
            IF ( Obj(I)%I1 .NE. N ) ERROR STOP 119
            IF ( SIZE(Obj(I)%A1)   .NE.    M ) ERROR STOP 120
            IF ( ANY(Obj(I)%A1     .NE. 2*N) ) ERROR STOP 121
            IF ( LEN(Obj(I)%Carr)  .NE.    N ) ERROR STOP 122
            IF ( TRIM(Obj(I)%Carr) .NE.  'B' ) ERROR STOP 123
         END DO
      END SUBROUTINE Sub25

END PROGRAM SpeExpHostAssociation03a