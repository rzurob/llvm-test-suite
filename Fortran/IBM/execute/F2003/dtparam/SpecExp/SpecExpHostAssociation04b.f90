!*  ===================================================================
!*
!*  DATE                       : June 14, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Specification expression - Host Association
!*  SECONDARY FUNCTIONS TESTED : Defined assignment
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
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
         INTEGER, KIND :: k1 = 2
         INTEGER, LEN  :: l1 = 2

         INTEGER(k1) :: I1, A1(l1)
         CHARACTER(l1) :: C1

         CONTAINS
         PROCEDURE, PASS :: assgnBase
         GENERIC :: assignment(=) => assgnBase
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
         INTEGER, KIND :: k2 = 4
         INTEGER, LEN  :: l2 = 4

         INTEGER(k2) :: A2(l2)
         CHARACTER(l2) :: C2
         TYPE(Base(k2,l2)) :: cmp1

         CONTAINS
         PROCEDURE, PASS :: assgnChild
         GENERIC :: assignment(=) => assgnChild
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen (k3,l3)
         INTEGER, KIND :: k3 = 8
         INTEGER, LEN  :: l3 = 8

         INTEGER(k3) :: A3(l3)
         CHARACTER(l3) :: C3
         TYPE(Base(k3,l3)) :: cmp2
      END TYPE

      CONTAINS

      SUBROUTINE assgnBase(this, arg)
        CLASS(Base(4,*)), INTENT(OUT) :: this
        TYPE(Base(4,*)), INTENT(IN) :: arg

        this%I1 = arg%I1
        this%A1 = arg%A1
        this%C1 = arg%C1
      END SUBROUTINE

      SUBROUTINE assgnChild(this, arg)
        CLASS(Child(4,*,4,*)), INTENT(OUT) :: this
        TYPE(Child(4,*,4,*)), INTENT(IN) :: arg

        this%I1 = arg%I1
        this%A1 = arg%A1
        this%C1 = arg%C1
        this%A2 = arg%A2
        this%C2 = arg%C2
        this%cmp1 = arg%cmp1
      END SUBROUTINE
END MODULE
PROGRAM SpeExpHostAssociation04b
      USE Mod
      IMPLICIT NONE

      INTEGER I
      CLASS(Child(4,:,4,:)), ALLOCATABLE :: poly

      ALLOCATE( poly, SOURCE = Child(4,3,4,6)( 1, [(I, I = 1,3)], 'AAA', [(2*I, I = 1,6)], 'BBBBBB',   &
                               Base(4,6)( 3, [(3*I, I = 1,6)], 'CCCCCC') ) )

      CALL Sub11(4,6)
      CALL Sub12(4,3,12)
      CALL Sub13(4,3,6,9)
      CALL Sub14(4,6,3)

      poly%cmp1 = Base(4,6)( I1 = 4, A1 = [(4*I, I = 1,6)], C1 = 'DDDDDD')

      CALL Sub11(4,6)
      CALL Sub12(4,4,12)
      CALL Sub13(4,4,8,12)
      CALL Sub14(4,6,4)

      DEALLOCATE(poly)
      ALLOCATE( Child(4,5,4,10) :: poly )
      poly = Child(4,5,4,10)( 5, [(5*I, I = 1,5)], 'XLF', [(10*I, I = 1,10)], 'IBM', Base(4,10)( 11, [(11*I, I = 1,10)], 'test') )


      CALL Sub11(4,10)
      CALL Sub12(4,11,20)
      CALL Sub13(4,11,22,33)
      CALL Sub14(4,10,11)

      CONTAINS

      SUBROUTINE Sub11(N, M)
        INTEGER :: N, M
        TYPE(Base(poly%cmp1%k1,poly%cmp1%l1)) :: Obj

        IF ( Obj%k1 .NE. N ) STOP 10
        IF ( Obj%l1 .NE. M ) STOP 11
        IF ( SIZE(Obj%A1)  .NE. M ) STOP 12
        IF ( LEN(Obj%C1) .NE. M ) STOP 13
      END SUBROUTINE Sub11

      SUBROUTINE Sub12(N, M, P)
        INTEGER :: N, M, P
        TYPE(Child(KIND(poly%cmp1%I1),poly%cmp1%I1,2*KIND(poly%cmp1%I1),2*SIZE(poly%cmp1%A1))) :: Obj

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
        TYPE(NextGen(KIND(poly%cmp1%A1(1)),poly%cmp1%A1(1),                                   &
             KIND(poly%cmp1%A1(2)),poly%cmp1%A1(2),KIND(poly%cmp1%A1(3)),poly%cmp1%A1(3))) :: Obj

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
        TYPE(Base(KIND(poly%cmp1%I1),LEN(poly%cmp1%C1))) :: Obj(poly%cmp1%I1)

        IF ( SIZE(Obj) .NE. P ) STOP 58
        DO I = 1, poly%I1
           IF ( Obj(I)%k1 .NE. N ) STOP 59
           IF ( Obj(I)%l1 .NE. M ) STOP 60
           IF ( SIZE(Obj(I)%A1) .NE. M ) STOP 61
           IF ( LEN(Obj(I)%C1)  .NE. M ) STOP 62
        END DO
      END SUBROUTINE Sub14
END PROGRAM SpeExpHostAssociation04b
