!*  ===================================================================
!*
!*  DATE                       : June 14, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Specification expression - Host Association
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
!*  An objet designator with a base object that is made accessible by host association
!*  Polymorphic base object
!*
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM SpeExpHostAssociation04a
      IMPLICIT NONE

      TYPE Base (k1,l1)
         INTEGER, KIND :: k1 = 2
         INTEGER, LEN  :: l1 = 2

         INTEGER(k1) :: I1, A1(l1)
         CHARACTER(l1) :: C1
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
         INTEGER, KIND :: k2 = 4
         INTEGER, LEN  :: l2 = 4

         INTEGER(k2) :: A2(l2)
         CHARACTER(l2) :: C2
         TYPE(Base(k2,l2)) :: cmp1
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen (k3,l3)
         INTEGER, KIND :: k3 = 8
         INTEGER, LEN  :: l3 = 8

         INTEGER(k3) :: A3(l3)
         CHARACTER(l3) :: C3
         TYPE(Base(k3,l3)) :: cmp2
      END TYPE

      INTEGER I
      TYPE(Base(4,:)), ALLOCATABLE :: poly

      ALLOCATE( poly, SOURCE = Base(4,5)( I1 = 4, A1 = [(4*I, I = 1,5)], C1 = 'BBBBB' ) )

      CALL Sub11(4,5)
      CALL Sub12(4,4,10)
      CALL Sub13(4,4,8,12)
      CALL Sub14(4,5,4)

      poly = Base(4,7)( I1 = 10, A1 = [(10*I, I = 1,7)], C1 = 'CCCCCCC')

      CALL Sub11(4,7)
      CALL Sub12(4,10,14)
      CALL Sub13(4,10,20,30)
      CALL Sub14(4,7,10)

      CONTAINS

      SUBROUTINE Sub11(N, M)
        INTEGER :: N, M
        TYPE(Base(poly%k1,poly%l1)) :: Obj

        IF ( Obj%k1 .NE. N ) STOP 10
        IF ( Obj%l1 .NE. M ) STOP 11
        IF ( SIZE(Obj%A1)  .NE. M ) STOP 12
        IF ( LEN(Obj%C1) .NE. M ) STOP 13
      END SUBROUTINE Sub11

      SUBROUTINE Sub12(N, M, P)
        INTEGER :: N, M, P
        TYPE(Child(KIND(poly%I1),poly%I1,2*KIND(poly%I1),2*SIZE(poly%A1))) :: Obj

        IF ( Obj%k1 .NE.   N ) STOP 14
        IF ( Obj%l1 .NE.   M ) STOP 15
        IF ( Obj%k2 .NE. 2*N ) STOP 16
        IF ( Obj%l2 .NE.   P ) STOP 17
        IF ( SIZE(Obj%A1) .NE. M ) STOP 18
        IF ( LEN(Obj%C1)  .NE. M ) STOP 19
        IF ( SIZE(Obj%A2) .NE. P ) STOP 20
        IF ( LEN(Obj%C2)  .NE. P ) STOP 21

        IF ( Obj%cmp1%k1 .NE. 2*N ) STOP 22
        IF ( Obj%cmp1%l1 .NE.   P ) STOP 23
        IF ( SIZE(Obj%cmp1%A1) .NE. P ) STOP 24
        IF ( LEN(Obj%cmp1%C1)  .NE. P ) STOP 25
      END SUBROUTINE Sub12

      SUBROUTINE Sub13(N, M, P, Q)
        INTEGER :: N, M, P, Q
        TYPE(NextGen(KIND(poly%A1(1)),poly%A1(1),KIND(poly%A1(2)),poly%A1(2),KIND(poly%A1(3)),poly%A1(3))) :: Obj

        IF ( Obj%k1 .NE. N ) STOP 26
        IF ( Obj%l1 .NE. M ) STOP 27
        IF ( Obj%k2 .NE. N ) STOP 28
        IF ( Obj%l2 .NE. P ) STOP 29
        IF ( Obj%k3 .NE. N ) STOP 30
        IF ( Obj%l3 .NE. Q ) STOP 31
        IF ( SIZE(Obj%A1) .NE. M ) STOP 32
        IF ( LEN(Obj%C1)  .NE. M ) STOP 33
        IF ( SIZE(Obj%A2) .NE. P ) STOP 34
        IF ( LEN(Obj%C2)  .NE. P ) STOP 35
        IF ( SIZE(Obj%A3) .NE. Q ) STOP 36
        IF ( LEN(Obj%C3)  .NE. Q ) STOP 37

        IF ( Obj%cmp1%k1 .NE. N ) STOP 38
        IF ( Obj%cmp1%l1 .NE. P ) STOP 39
        IF ( SIZE(Obj%cmp1%A1) .NE. P ) STOP 40
        IF ( LEN(Obj%cmp1%C1)  .NE. P ) STOP 41

        IF ( Obj%cmp2%k1 .NE. N ) STOP 42
        IF ( Obj%cmp2%l1 .NE. Q ) STOP 43
        IF ( SIZE(Obj%cmp2%A1) .NE. Q ) STOP 44
        IF ( LEN(Obj%cmp2%C1)  .NE. Q ) STOP 45
      END SUBROUTINE Sub13

      SUBROUTINE Sub14(N, M, P)
        INTEGER :: N, M, P, I
        TYPE(Base(KIND(poly%I1),LEN(poly%C1))) :: Obj(poly%I1)

        IF ( SIZE(Obj) .NE. P ) STOP 46
        DO I = 1, poly%I1
           IF ( Obj(I)%k1 .NE. N ) STOP 47
           IF ( Obj(I)%l1 .NE. M ) STOP 48
           IF ( SIZE(Obj(I)%A1) .NE. M ) STOP 49
           IF ( LEN(Obj(I)%C1)  .NE. M ) STOP 50
        END DO
      END SUBROUTINE Sub14
END PROGRAM SpeExpHostAssociation04a
