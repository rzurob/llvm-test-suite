!*  ===================================================================
!*
!*  DATE                       : June 14, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Speification expression - Host Association
!*  SECONDARY FUNCTIONS TESTED : Named Constant
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
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
         INTEGER, KIND :: k1 = 2
         INTEGER, LEN  :: l1 = 2

         INTEGER(k1) :: I1, A1(l1)
         CHARACTER(l1) :: name
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
         INTEGER, KIND :: k2 = 4
         INTEGER, LEN  :: l2 = 4

         INTEGER(k2) :: I2, A2(l2)
         TYPE(Base(k2,l2)) :: b1
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen (k3,l3)
         INTEGER, KIND :: k3 = 8
         INTEGER, LEN  :: l3 = 8

         INTEGER(k3) :: I3, A3(l3)
         TYPE(Child(k2,l2,k3,l3)) :: c1
      END TYPE

END MODULE
PROGRAM SpeExpHostAssociation03b
      USE Mod
      IMPLICIT TYPE (NextGen) (n)

      PARAMETER (n1 = NextGen ( I1 = 1, I2 = 2, I3 = 3, A1 = 11 , A2 = 22, A3 = 33, name = 'A',                     &
        c1 = Child(4,4,8,8)(1, [(I, I=1,4)], 'C', 2, [(2*I, I=1,8)], b1=Base(8,8)(3, [(3*I, I =1,8)], 'D')),  &
        b1 = Base(4,4)(10, 20, 'B') ) )

      TYPE(NextGen(4,3,4,5,4,7)), PARAMETER :: m1 =  NextGen(4,3,4,5,4,7) (                                      &
        b1 = Base(4,5)( I1 = 10, A1 = [(4*I, I =1,5)], name = 'BBBBB' ),                                         &
        c1 = Child(4,5,4,7)( I1 = 10, A1 = [(10*I, I =1,5)], name = 'CCCCC', I2 = 20, A2 = [(20*I, I =1,7)],     &
                             b1 = Base(4,7)( I1 = 30, A1 = [(30*I, I =1,7)], name = 'DDDDDDD' ) ),               &
        I1 = 1, I2 = 2, I3 = 3, A1 = [(I, I=1,3)] , A2 = [(2*I, I=1,5)], A3 = [(3*I, I=1,7)], name = 'AAA' )

      CALL Sub11(8)
      CALL Sub12(8)
      CALL Sub13(8,3)
      CALL Sub14(8,3,24)
      CALL Sub15(8,8,3)

      CALL Sub21(4,7)
      CALL Sub22(8,14)
      CALL Sub23(4,30)
      CALL Sub24(4,30,210)
      CALL Sub25(4,7,30)

      CONTAINS

      SUBROUTINE Sub11(N)
        INTEGER :: N
        TYPE(Base(n1%c1%b1%k1,n1%c1%b1%l1)) :: Obj

        IF ( Obj%k1 .NE. N ) STOP 10
        IF ( Obj%l1 .NE. N ) STOP 11
        IF ( SIZE(Obj%A1)  .NE. N ) STOP 12
        IF ( LEN(Obj%name) .NE. N ) STOP 13
      END SUBROUTINE Sub11

      SUBROUTINE Sub12(N)
        INTEGER :: N
        TYPE(Base(n1%c1%b1%l1,2*n1%c1%b1%k1)) :: Obj

        IF ( Obj%k1 .NE.   N ) STOP 14
        IF ( Obj%l1 .NE. 2*N ) STOP 15
        IF ( SIZE(Obj%A1)  .NE. 2*N ) STOP 16
        IF ( LEN(Obj%name) .NE. 2*N ) STOP 17
      END SUBROUTINE Sub12

      SUBROUTINE Sub13(N, M)
        INTEGER :: N, M
        TYPE(Child(KIND(n1%c1%b1%I1),n1%c1%b1%I1,KIND(n1%c1%b1%I1),2*n1%c1%b1%I1)) :: Obj

        IF ( Obj%k1 .NE.   N ) STOP 18
        IF ( Obj%l1 .NE.   M ) STOP 19
        IF ( Obj%k2 .NE.   N ) STOP 20
        IF ( Obj%l2 .NE. 2*M ) STOP 21
        IF ( SIZE(Obj%A1)  .NE. M ) STOP 22
        IF ( LEN(Obj%name) .NE. M ) STOP 23

        IF ( Obj%b1%k1 .NE.   N ) STOP 24
        IF ( Obj%b1%l1 .NE. 2*M ) STOP 25
        IF ( SIZE(Obj%b1%A1)  .NE. 2*M ) STOP 26
        IF ( LEN(Obj%b1%name) .NE. 2*M ) STOP 27
      END SUBROUTINE Sub13

      SUBROUTINE Sub14(N, M, P)
        INTEGER :: N, M, P
        TYPE(Child(KIND(n1%c1%b1%A1(1)),n1%c1%b1%A1(1),KIND(n1%c1%b1%A1(n1%l3)),n1%c1%b1%A1(n1%l3))) :: Obj

        IF ( Obj%k1 .NE. N ) STOP 28
        IF ( Obj%l1 .NE. M ) STOP 29
        IF ( Obj%k2 .NE. N ) STOP 30
        IF ( Obj%l2 .NE. P ) STOP 31
        IF ( SIZE(Obj%A1)  .NE. M ) STOP 32
        IF ( LEN(Obj%name) .NE. M ) STOP 33

        IF ( Obj%b1%k1 .NE. N ) STOP 34
        IF ( Obj%b1%l1 .NE. P ) STOP 35
        IF ( SIZE(Obj%b1%A1)  .NE. P ) STOP 36
        IF ( LEN(Obj%b1%name) .NE. P ) STOP 37
      END SUBROUTINE Sub14

      SUBROUTINE Sub15(N, M, P)
        INTEGER :: N, M, P, I
        TYPE(Base(KIND(n1%c1%b1%I1),LEN(n1%c1%b1%name))) :: Obj(n1%c1%b1%I1)

         IF ( SIZE(Obj) .NE. P ) STOP 38
         DO I = 1, n1%c1%b1%I1
            IF ( Obj(I)%k1 .NE. N ) STOP 39
            IF ( Obj(I)%l1 .NE. M ) STOP 40
            IF ( SIZE(Obj(I)%A1)  .NE. M ) STOP 41
            IF ( LEN(Obj(I)%name) .NE. N ) STOP 42
         END DO
      END SUBROUTINE Sub15

      SUBROUTINE Sub21(N, M)
        INTEGER :: N, M
        TYPE(Base(m1%c1%b1%k1,m1%c1%b1%l1)) :: Obj

        IF ( Obj%k1 .NE. N ) STOP 50
        IF ( Obj%l1 .NE. M ) STOP 51
        IF ( SIZE(Obj%A1)  .NE. M ) STOP 52
        IF ( LEN(Obj%name) .NE. M ) STOP 53
      END SUBROUTINE Sub21

      SUBROUTINE Sub22(N, M)
        INTEGER :: N, M
        TYPE(Base(2*m1%c1%b1%k1,2*SIZE(m1%c1%b1%A1))) :: Obj

        IF ( Obj%k1 .NE. N ) STOP 54
        IF ( Obj%l1 .NE. M ) STOP 55
        IF ( SIZE(Obj%A1)  .NE. M ) STOP 56
        IF ( LEN(Obj%name) .NE. M ) STOP 57
      END SUBROUTINE Sub22

      SUBROUTINE Sub23(N, M)
        INTEGER :: N, M
        TYPE(Child(KIND(m1%c1%b1%I1),m1%c1%b1%I1,KIND(m1%c1%b1%I1),2*m1%c1%b1%I1)) :: Obj

        IF ( Obj%k1 .NE.   N ) STOP 58
        IF ( Obj%l1 .NE.   M ) STOP 59
        IF ( Obj%k2 .NE.   N ) STOP 60
        IF ( Obj%l2 .NE. 2*M ) STOP 61
        IF ( SIZE(Obj%A1)  .NE. M ) STOP 62
        IF ( LEN(Obj%name) .NE. M ) STOP 63

        IF ( Obj%b1%k1 .NE.   N ) STOP 64
        IF ( Obj%b1%l1 .NE. 2*M ) STOP 65
        IF ( SIZE(Obj%b1%A1)  .NE. 2*M ) STOP 66
        IF ( LEN(Obj%b1%name) .NE. 2*M ) STOP 67
      END SUBROUTINE Sub23

      SUBROUTINE Sub24(N, M, P)
        INTEGER :: N, M, P
        TYPE(Child(KIND(m1%c1%b1%A1(1)),m1%c1%b1%A1(1),KIND(m1%c1%b1%A1(m1%l3)),m1%c1%b1%A1(m1%l3))) :: Obj

        IF ( Obj%k1 .NE. N ) STOP 68
        IF ( Obj%l1 .NE. M ) STOP 69
        IF ( Obj%k2 .NE. N ) STOP 70
        IF ( Obj%l2 .NE. P ) STOP 71
        IF ( SIZE(Obj%A1)  .NE. M ) STOP 72
        IF ( LEN(Obj%name) .NE. M ) STOP 73

        IF ( Obj%b1%k1 .NE. N ) STOP 74
        IF ( Obj%b1%l1 .NE. P ) STOP 75
        IF ( SIZE(Obj%b1%A1)  .NE. P ) STOP 76
        IF ( LEN(Obj%b1%name) .NE. P ) STOP 77
      END SUBROUTINE Sub24

      SUBROUTINE Sub25(N, M, P)
        INTEGER :: N, M, P, I
        TYPE(Base(KIND(m1%c1%b1%I1),LEN(m1%c1%b1%name))) :: Obj(m1%c1%b1%I1)

        IF ( SIZE(Obj) .NE. P ) STOP 78
        DO I = 1, m1%c1%b1%I1
           IF ( Obj(I)%k1 .NE. N ) STOP 79
           IF ( Obj(I)%l1 .NE. M ) STOP 80
           IF ( SIZE(Obj(I)%A1)  .NE. M ) STOP 81
           IF ( LEN(Obj(I)%name) .NE. M ) STOP 82
         END DO
      END SUBROUTINE Sub25

END PROGRAM SpeExpHostAssociation03b
