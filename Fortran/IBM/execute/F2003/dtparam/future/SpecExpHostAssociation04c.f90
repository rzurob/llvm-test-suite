!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : SpeExpHostAssociation04c.f
!*
!*  PROGRAMMER                 : Dorra Bouhiha
!*  DATE                       : June 14, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Speification expression - Host Association
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
!*  An objet designator with a base object that is made accessible by host association
!*  Polymorphic base object
!*
!* Defect: 354497
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
         INTEGER, KIND :: k1
         INTEGER, LEN  :: l1

         INTEGER(k1) :: I1, A1(l1)
         CHARACTER(l1) :: C1
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
         INTEGER, KIND :: k2
         INTEGER, LEN  :: l2

         INTEGER(k2) :: A2(l2)
         CHARACTER(l2) :: C2
         TYPE(Base(k2,l2)), ALLOCATABLE :: poly
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen (k3,l3)
         INTEGER, KIND :: k3
         INTEGER, LEN  :: l3

         INTEGER(k3) :: A3(l3)
         CHARACTER(l3) :: C3
      END TYPE

      CONTAINS

      FUNCTION CreateNew(arg) Result(Res)
        TYPE(Base(4,*)), INTENT(IN) :: arg
        TYPE(Base(4,:)), ALLOCATABLE :: Res

        ALLOCATE( Res, SOURCE = arg )

      END FUNCTION
END MODULE
PROGRAM SpeExpHostAssociation04c
      USE Mod
      IMPLICIT NONE

      INTEGER :: I, J
      TYPE(Base(4,:)), ALLOCATABLE :: b1
      TYPE(Child(4,:,4,:)), ALLOCATABLE :: c1

      b1 = CreateNew( Base(4,5)(5, [1,2,3,4,5], 'AAAAA') )
      IF ( .NOT. ALLOCATED(b1) ) STOP 100
      CALL Sub11(4,5)
      CALL Sub12(4,5,10)
      CALL Sub13(4,1,2,3)
      CALL Sub14(4,5,5)

      b1 = CreateNew( Base(4,7)(I1 = 10, A1 = [(10*I, I = 1,7)], C1 = 'BBBBBBB') )
      IF ( .NOT. ALLOCATED(b1) ) STOP 101
      CALL Sub11(4,7)
      CALL Sub12(4,10,14)
      CALL Sub13(4,10,20,30)
      CALL Sub14(4,7,10)

      c1 = Child(4,1,4,10) ( 8, [88], 'X', [(99*I, I = 1,10)], 'XLFtest', Base(4,10)(3, [(7*I, I = 1,10)], 'IBM') )
      IF ( .NOT. ALLOCATED(c1) ) STOP 102
      IF ( .NOT. ALLOCATED(c1%poly) ) STOP 103
      CALL Sub21(4,10)
      CALL Sub22(4,3,20)
      CALL Sub23(4,7,14,21)
      CALL Sub24(4,10,3)

      CONTAINS
 
      SUBROUTINE Sub11(N, M)
        INTEGER :: N, M
        TYPE(Base(b1%k1,b1%l1)) :: Obj

        IF ( Obj%k1 .NE. N ) STOP 10
        IF ( Obj%l1 .NE. M ) STOP 11
        IF ( SIZE(Obj%A1) .NE. M ) STOP 12
        IF ( LEN(Obj%C1)  .NE. M ) STOP 13
      END SUBROUTINE Sub11
 
      SUBROUTINE Sub21(N, M)
        INTEGER :: N, M
        TYPE(Base(c1%poly%k1,c1%poly%l1)) :: Obj

        IF ( Obj%k1 .NE. N ) STOP 14
        IF ( Obj%l1 .NE. M ) STOP 15
        IF ( SIZE(Obj%A1) .NE. M ) STOP 16
        IF ( LEN(Obj%C1)  .NE. M ) STOP 17
      END SUBROUTINE Sub21

      SUBROUTINE Sub12(N, M, P)
        INTEGER :: N, M, P
        TYPE(Child(KIND(b1%I1),b1%I1,2*KIND(b1%I1),2*SIZE(b1%A1))) :: Obj

        IF ( Obj%k1 .NE.   N ) STOP 18
        IF ( Obj%l1 .NE.   M ) STOP 19
        IF ( Obj%k2 .NE. 2*N ) STOP 20
        IF ( Obj%l2 .NE.   P ) STOP 21
        IF ( SIZE(Obj%A1) .NE. M ) STOP 22
        IF ( LEN(Obj%C1)  .NE. M ) STOP 23
        IF ( SIZE(Obj%A2) .NE. P ) STOP 24
        IF ( LEN(Obj%C2)  .NE. P ) STOP 25

        ALLOCATE( Base(Obj%k2,Obj%l2) :: Obj%poly )
        IF ( Obj%poly%k1 .NE. 2*N ) STOP 26
        IF ( Obj%poly%l1 .NE.   P ) STOP 27
        IF ( SIZE(Obj%poly%A1) .NE. P ) STOP 28
        IF ( LEN(Obj%poly%C1)  .NE. P ) STOP 29
      END SUBROUTINE Sub12

      SUBROUTINE Sub22(N, M, P)
        INTEGER :: N, M, P
        TYPE(Child(KIND(c1%poly%I1),c1%poly%I1,2*KIND(c1%poly%I1),2*SIZE(c1%poly%A1))) :: Obj

        IF ( Obj%k1 .NE.   N ) STOP 30
        IF ( Obj%l1 .NE.   M ) STOP 31
        IF ( Obj%k2 .NE. 2*N ) STOP 32
        IF ( Obj%l2 .NE.   P ) STOP 33
        IF ( SIZE(Obj%A1) .NE. M ) STOP 34
        IF ( LEN(Obj%C1)  .NE. M ) STOP 35
        IF ( SIZE(Obj%A2) .NE. P ) STOP 36
        IF ( LEN(Obj%C2)  .NE. P ) STOP 37

        ALLOCATE( Base(Obj%k2,Obj%l2) :: Obj%poly )
        IF ( Obj%poly%k1 .NE. 2*N ) STOP 38
        IF ( Obj%poly%l1 .NE.   P ) STOP 39
        IF ( SIZE(Obj%poly%A1) .NE. P ) STOP 40
        IF ( LEN(Obj%poly%C1)  .NE. P ) STOP 41
      END SUBROUTINE Sub22

      SUBROUTINE Sub13(N, M, P, Q)
        INTEGER :: N, M, P, Q
        TYPE(NextGen(KIND(b1%A1(1)),b1%A1(1),KIND(b1%A1(2)),b1%A1(2),KIND(b1%A1(3)),b1%A1(3))) :: Obj

        IF ( Obj%k1 .NE. N ) STOP 42
        IF ( Obj%l1 .NE. M ) STOP 43
        IF ( Obj%k2 .NE. N ) STOP 44
        IF ( Obj%l2 .NE. P ) STOP 45
        IF ( Obj%k3 .NE. N ) STOP 46
        IF ( Obj%l3 .NE. Q ) STOP 47
        IF ( SIZE(Obj%A1) .NE. M ) STOP 48
        IF ( LEN(Obj%C1)  .NE. M ) STOP 49
        IF ( SIZE(Obj%A2) .NE. P ) STOP 50
        IF ( LEN(Obj%C2)  .NE. P ) STOP 51
        IF ( SIZE(Obj%A3) .NE. Q ) STOP 52
        IF ( LEN(Obj%C3)  .NE. Q ) STOP 53

        ALLOCATE( Base(Obj%k2,Obj%l2) :: Obj%poly )
        IF ( Obj%poly%k1 .NE. N ) STOP 54
        IF ( Obj%poly%l1 .NE. P ) STOP 55
        IF ( SIZE(Obj%poly%A1) .NE. P ) STOP 56
        IF ( LEN(Obj%poly%C1)  .NE. P ) STOP 57
      END SUBROUTINE Sub13

      SUBROUTINE Sub23(N, M, P, Q)
        INTEGER :: N, M, P, Q
        TYPE(NextGen(KIND(c1%poly%A1(1)),c1%poly%A1(1),                               &
             KIND(c1%poly%A1(2)),c1%poly%A1(2),KIND(c1%poly%A1(3)),c1%poly%A1(3))) :: Obj

        IF ( Obj%k1 .NE. N ) STOP 58
        IF ( Obj%l1 .NE. M ) STOP 59
        IF ( Obj%k2 .NE. N ) STOP 60
        IF ( Obj%l2 .NE. P ) STOP 61
        IF ( Obj%k3 .NE. N ) STOP 62
        IF ( Obj%l3 .NE. Q ) STOP 63
        IF ( SIZE(Obj%A1) .NE. M ) STOP 64
        IF ( LEN(Obj%C1)  .NE. M ) STOP 65
        IF ( SIZE(Obj%A2) .NE. P ) STOP 66
        IF ( LEN(Obj%C2)  .NE. P ) STOP 67
        IF ( SIZE(Obj%A3) .NE. Q ) STOP 68
        IF ( LEN(Obj%C3)  .NE. Q ) STOP 69

        ALLOCATE( Base(Obj%k2,Obj%l2) :: Obj%poly )
        IF ( Obj%poly%k1 .NE. N ) STOP 70
        IF ( Obj%poly%l1 .NE. P ) STOP 71
        IF ( SIZE(Obj%poly%A1) .NE. P ) STOP 72
        IF ( LEN(Obj%poly%C1)  .NE. P ) STOP 73
      END SUBROUTINE Sub23

      SUBROUTINE Sub14(N, M, P)
        INTEGER :: N, M, P, I
        TYPE(Base(KIND(b1%I1),LEN(b1%C1))) :: Obj(b1%I1)

        IF ( SIZE(Obj) .NE. P ) STOP 74
        DO I = 1, b1%I1
           IF ( Obj(I)%k1 .NE. N ) STOP 75
           IF ( Obj(I)%l1 .NE. M ) STOP 76
           IF ( SIZE(Obj(I)%A1) .NE. M ) STOP 77
           IF ( LEN(Obj(I)%C1)  .NE. M ) STOP 78
        END DO
      END SUBROUTINE Sub14

      SUBROUTINE Sub24(N, M, P)
        INTEGER :: N, M, P, I
        TYPE(Base(KIND(c1%poly%I1),LEN(c1%poly%C1))) :: Obj(c1%poly%I1)

        IF ( SIZE(Obj) .NE. P ) STOP 79
        DO I = 1, b1%I1
           IF ( Obj(I)%k1 .NE. N ) STOP 80
           IF ( Obj(I)%l1 .NE. M ) STOP 81
           IF ( SIZE(Obj(I)%A1) .NE. M ) STOP 82
           IF ( LEN(Obj(I)%C1)  .NE. M ) STOP 83
        END DO
      END SUBROUTINE Sub24
END PROGRAM SpeExpHostAssociation04c
