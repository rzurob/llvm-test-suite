!*  ===================================================================
!*
!*  DATE                       : June 14, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Speification expression - Host Association
!*  SECONDARY FUNCTIONS TESTED : Explicit Initialization
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
         CLASS(Base(k2,l2)), POINTER :: b1
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen (k3,l3)
         INTEGER, KIND :: k3 = 8
         INTEGER, LEN  :: l3 = 8

         INTEGER(k3) :: I3, A3(l3)
         CLASS(Base(k3,l3)), POINTER :: c1
      END TYPE

END MODULE
PROGRAM SpeExpHostAssociation03b
      USE Mod
      IMPLICIT NONE

      INTEGER I
      TYPE(NextGen(4,3,4,5,4,7)) :: n1 =  NextGen(4,3,4,5,4,7) (  I1 = 1, I2 = 2, I3 = 3,         &
         A1 = [(I, I=1,3)] , A2 = [(2*I, I=1,5)], A3 = [(3*I, I=1,7)], name = 'AAA', b1 = NULL(), c1 = NULL() )

      ALLOCATE( n1%b1, SOURCE =  Base(4,5)( I1 = 4, A1 = [(4*I, I = 1,5)], name = 'BBBBB' ) )
      ALLOCATE( n1%c1, SOURCE =  Base(4,7)( I1 = 10, A1 = [(10*I, I = 1,7)], name = 'CCCCCCC') )

      CALL Sub11(4,5)
      CALL Sub12(4,4,10)
      CALL Sub13(4,4,20)
      CALL Sub14(4,5,4)

      CALL Sub21(4,7)
      CALL Sub22(4,10,14)
      CALL Sub23(4,10,70)
      CALL Sub24(4,7,10)

      ALLOCATE( n1%b1, SOURCE =  Child(4,5,4,9)( I1 = 5, A1 = [(5*I, I =1,5)], name = 'DDDDD', I2 = 6, A2 = [(6*I, I =1,9)], &
                b1 = NULL() ) )

      SELECT TYPE ( s => n1%b1 )
         CLASS IS (Child(4,*,4,*))
           ALLOCATE( s%b1, SOURCE =  Base(4,9)( I1 = 20, A1 = [(20*I, I =1,9)], name = 'EEEEEEEEE' ) )
           CALL Sub11(4,5)
           CALL Sub12(4,5,10)
           CALL Sub13(4,5,25)
           CALL Sub14(4,5,5)

           CALL Sub21(4,7)
           CALL Sub22(4,10,14)
           CALL Sub23(4,10,70)
           CALL Sub24(4,7,10)

         CLASS DEFAULT
           STOP 100
      END SELECT

      CONTAINS

      SUBROUTINE Sub11(N, M)
        INTEGER :: N, M
        TYPE(Base(n1%b1%k1,n1%b1%l1)) :: Obj

        IF ( Obj%k1 .NE. N ) ERROR STOP 10
        IF ( Obj%l1 .NE. M ) ERROR STOP 11
        IF ( SIZE(Obj%A1)  .NE. M ) ERROR STOP 12
        IF ( LEN(Obj%name) .NE. M ) ERROR STOP 13
      END SUBROUTINE Sub11

      SUBROUTINE Sub21(N, M)
        INTEGER :: N, M
        TYPE(Base(n1%c1%k1,n1%c1%l1)) :: Obj

        IF ( Obj%k1 .NE. N ) ERROR STOP 14
        IF ( Obj%l1 .NE. M ) ERROR STOP 15
        IF ( SIZE(Obj%A1)  .NE. M ) ERROR STOP 16
        IF ( LEN(Obj%name) .NE. M ) ERROR STOP 17
      END SUBROUTINE Sub21

      SUBROUTINE Sub12(N, M, P)
        INTEGER :: N, M, P
        TYPE(Child(KIND(n1%b1%I1),n1%b1%I1,2*KIND(n1%b1%I1),2*SIZE(n1%b1%A1))) :: Obj

        IF ( Obj%k1 .NE.   N ) ERROR STOP 18
        IF ( Obj%l1 .NE.   M ) ERROR STOP 19
        IF ( Obj%k2 .NE. 2*N ) ERROR STOP 20
        IF ( Obj%l2 .NE.   P ) ERROR STOP 21
        IF ( SIZE(Obj%A1)  .NE. M ) ERROR STOP 22
        IF ( LEN(Obj%name) .NE. M ) ERROR STOP 23

        ALLOCATE( Base(Obj%k2,Obj%l2) :: Obj%b1 )
        IF ( Obj%b1%k1 .NE. 2*N ) ERROR STOP 24
        IF ( Obj%b1%l1 .NE.   P ) ERROR STOP 25
        IF ( SIZE(Obj%b1%A1)  .NE. P ) ERROR STOP 26
        IF ( LEN(Obj%b1%name) .NE. P ) ERROR STOP 27
      END SUBROUTINE Sub12

      SUBROUTINE Sub22(N, M, P)
        INTEGER :: N, M, P
        TYPE(Child(KIND(n1%c1%I1),n1%c1%I1,KIND(n1%c1%I1),2*LEN(n1%c1%name))) :: Obj

        IF ( Obj%k1 .NE. N ) ERROR STOP 28
        IF ( Obj%l1 .NE. M ) ERROR STOP 29
        IF ( Obj%k2 .NE. N ) ERROR STOP 30
        IF ( Obj%l2 .NE. P ) ERROR STOP 31
        IF ( SIZE(Obj%A1)  .NE. M ) ERROR STOP 32
        IF ( LEN(Obj%name) .NE. M ) ERROR STOP 33

        ALLOCATE( Base(Obj%k2,Obj%l2) :: Obj%b1 )
        IF ( Obj%b1%k1 .NE. N ) ERROR STOP 34
        IF ( Obj%b1%l1 .NE. P ) ERROR STOP 35
        IF ( SIZE(Obj%b1%A1)  .NE. P ) ERROR STOP 36
        IF ( LEN(Obj%b1%name) .NE. P ) ERROR STOP 37
      END SUBROUTINE Sub22

      SUBROUTINE Sub13(N, M, P)
        INTEGER :: N, M, P
        TYPE(Child(KIND(n1%b1%A1(1)),n1%b1%A1(1),KIND(n1%b1%A1(n1%b1%l1)),n1%b1%A1(n1%b1%l1))) :: Obj

        IF ( Obj%k1 .NE. N ) ERROR STOP 38
        IF ( Obj%l1 .NE. M ) ERROR STOP 39
        IF ( Obj%k2 .NE. N ) ERROR STOP 40
        IF ( Obj%l2 .NE. P ) ERROR STOP 41
        IF ( SIZE(Obj%A1)  .NE. M ) ERROR STOP 42
        IF ( LEN(Obj%name) .NE. M ) ERROR STOP 43

        ALLOCATE( Base(Obj%k2,Obj%l2) :: Obj%b1 )
        IF ( Obj%b1%k1 .NE. N ) ERROR STOP 44
        IF ( Obj%b1%l1 .NE. P ) ERROR STOP 45
        IF ( SIZE(Obj%b1%A1)  .NE. P ) ERROR STOP 46
        IF ( LEN(Obj%b1%name) .NE. P ) ERROR STOP 47
      END SUBROUTINE Sub13

      SUBROUTINE Sub23(N, M, P)
        INTEGER :: N, M, P
        TYPE(Child(KIND(n1%c1%A1(1)),n1%c1%A1(1),KIND(n1%c1%A1(n1%c1%l1)),n1%c1%A1(n1%c1%l1))) :: Obj

        IF ( Obj%k1 .NE. N ) ERROR STOP 48
        IF ( Obj%l1 .NE. M ) ERROR STOP 49
        IF ( Obj%k2 .NE. N ) ERROR STOP 50
        IF ( Obj%l2 .NE. P ) ERROR STOP 51
        IF ( SIZE(Obj%A1)  .NE. M ) ERROR STOP 52
        IF ( LEN(Obj%name) .NE. M ) ERROR STOP 53

        ALLOCATE( Base(Obj%k2,Obj%l2) :: Obj%b1 )
        IF ( Obj%b1%k1 .NE. N ) ERROR STOP 54
        IF ( Obj%b1%l1 .NE. P ) ERROR STOP 55
        IF ( SIZE(Obj%b1%A1)  .NE. P ) ERROR STOP 56
        IF ( LEN(Obj%b1%name) .NE. P ) ERROR STOP 57
      END SUBROUTINE Sub23

      SUBROUTINE Sub14(N, M, P)
        INTEGER :: N, M, P, I
        TYPE(Base(KIND(n1%b1%I1),LEN(n1%b1%name))) :: Obj(n1%b1%I1)

        IF ( SIZE(Obj) .NE. P ) ERROR STOP 58
        DO I = 1, n1%b1%I1
           IF ( Obj(I)%k1 .NE. N ) ERROR STOP 59
           IF ( Obj(I)%l1 .NE. M ) ERROR STOP 60
           IF ( SIZE(Obj(I)%A1)  .NE. M ) ERROR STOP 61
           IF ( LEN(Obj(I)%name) .NE. M ) ERROR STOP 62
        END DO
      END SUBROUTINE Sub14

      SUBROUTINE Sub24(N, M, P)
        INTEGER :: N, M, P, I
        TYPE(Base(KIND(n1%c1%I1),LEN(n1%c1%name))) :: Obj(n1%c1%I1)

        IF ( SIZE(Obj) .NE. P ) ERROR STOP 63
        DO I = 1, n1%c1%I1
           IF ( Obj(I)%k1 .NE. N ) ERROR STOP 64
           IF ( Obj(I)%l1 .NE. M ) ERROR STOP 65
           IF ( SIZE(Obj(I)%A1)  .NE. M ) ERROR STOP 66
           IF ( LEN(Obj(I)%name) .NE. M ) ERROR STOP 67
        END DO
      END SUBROUTINE Sub24

END PROGRAM SpeExpHostAssociation03b
