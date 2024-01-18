!*  ===================================================================
!*
!*  DATE                       : June 14, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Specification expression
!*  SECONDARY FUNCTIONS TESTED : Explicit Initialization - Host Association
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  An object designator with a base object that is made accessible by host association
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

         CLASS(Base(k2,l2)), ALLOCATABLE :: ptr
      END TYPE

END MODULE
PROGRAM SpecExpHostAssociation02c
      USE Mod
      IMPLICIT NONE

      TYPE(Child(4,10,4,5)) :: c1 = Child(4,10,4,5) ( 10, 20 , 'Heisenberg', NULL() )
      INTEGER I

      ALLOCATE( c1%ptr, SOURCE = Base(4,5) ( 30, [(I, I =1,c1%l1)], 'ABCDE' ) )
      CALL Sub11(4,5)
      CALL Sub12(4,5)
      CALL Sub13(4,30)
      CALL Sub14(4,1,5)
      CALL Sub15(4,5,30)

      c1 = Child(4,10,4,5) ( 99, [(9*I, I =1,c1%l1)], 'Sommerfeld', Base(4,5) ( 11, 22, 'FGHIJ') )
      CALL Sub11(4,5)
      CALL Sub12(4,5)
      CALL Sub13(4,11)
      CALL Sub14(4,22,22)
      CALL Sub15(4,5,11)

      c1%ptr%A1 = [(2*I, I =1,c1%l1)]
      CALL Sub11(4,5)
      CALL Sub12(4,5)
      CALL Sub13(4,11)
      CALL Sub14(4,2,10)
      CALL Sub15(4,5,11)

      CONTAINS

      SUBROUTINE Sub11(N, M)
        INTEGER :: N, M
        TYPE(Base(c1%ptr%k1,c1%ptr%l1)) :: Obj

         IF ( Obj%k1 .NE. N ) STOP 10
         IF ( Obj%l1 .NE. M ) STOP 11
         IF ( SIZE(Obj%A1) .NE. M ) STOP 12
         IF ( LEN(Obj%C1)  .NE. M ) STOP 13
      END SUBROUTINE Sub11

      SUBROUTINE Sub12(N, M)
        INTEGER :: N, M
        TYPE(Child(c1%ptr%k1,c1%ptr%l1,c1%ptr%k1,c1%ptr%l1)) :: Obj

         IF ( Obj%k1 .NE. N ) STOP 14
         IF ( Obj%l1 .NE. M ) STOP 15
         IF ( Obj%k2 .NE. N ) STOP 16
         IF ( Obj%l2 .NE. M ) STOP 17
         IF ( SIZE(Obj%A1) .NE. M ) STOP 18
         IF ( LEN(Obj%C1)  .NE. M ) STOP 19

         ALLOCATE( Base(Obj%k2,Obj%l2) :: Obj%ptr )
         IF ( Obj%ptr%k1 .NE. N ) STOP 20
         IF ( Obj%ptr%l1 .NE. M ) STOP 21
         IF ( SIZE(Obj%ptr%A1) .NE. M ) STOP 22
         IF ( LEN(Obj%ptr%C1)  .NE. M ) STOP 23
      END SUBROUTINE Sub12

      SUBROUTINE Sub13(N, M)
        INTEGER :: N, M
        TYPE(Child(KIND(c1%ptr%I1),c1%ptr%I1,KIND(c1%ptr%I1),2*c1%ptr%I1)) :: Obj

         IF ( Obj%k1 .NE. N ) STOP 24
         IF ( Obj%l1 .NE. M ) STOP 25
         IF ( Obj%k2 .NE. N ) STOP 26
         IF ( Obj%l2 .NE. 2*M ) STOP 27
         IF ( SIZE(Obj%A1) .NE. M ) STOP 28
         IF ( LEN(Obj%C1)  .NE. M ) STOP 29

         ALLOCATE( Base(Obj%k2,Obj%l2) :: Obj%ptr )
         IF ( Obj%ptr%k1 .NE.   N ) STOP 30
         IF ( Obj%ptr%l1 .NE. 2*M ) STOP 31
         IF ( SIZE(Obj%ptr%A1) .NE. 2*M ) STOP 32
         IF ( LEN(Obj%ptr%C1)  .NE. 2*M ) STOP 33
      END SUBROUTINE Sub13

      SUBROUTINE Sub14(N, M, P)
        INTEGER :: N, M, P
        TYPE(Child(KIND(c1%ptr%A1(1)),c1%ptr%A1(1),KIND(c1%ptr%A1(c1%l2)),c1%ptr%A1(c1%l2))) :: Obj

         IF ( Obj%k1 .NE. N ) STOP 34
         IF ( Obj%l1 .NE. M ) STOP 35
         IF ( Obj%k2 .NE. N ) STOP 36
         IF ( Obj%l2 .NE. P ) STOP 37
         IF ( SIZE(Obj%A1) .NE. M ) STOP 38
         IF ( LEN(Obj%C1)  .NE. M ) STOP 39

         ALLOCATE( Base(Obj%k2,Obj%l2) :: Obj%ptr )
         IF ( Obj%ptr%k1 .NE.   N ) STOP 40
         IF ( Obj%ptr%l1 .NE.   P ) STOP 41
         IF ( SIZE(Obj%ptr%A1) .NE. P ) STOP 42
         IF ( LEN(Obj%ptr%C1)  .NE. P ) STOP 43
      END SUBROUTINE Sub14

      SUBROUTINE Sub15(N, M, P)
        INTEGER :: N, M, P, I
        TYPE(Base(KIND(c1%ptr%I1),LEN(c1%ptr%C1))) :: Obj(c1%ptr%I1)

         IF ( SIZE(Obj) .NE. P ) STOP 44
         DO I = 1, c1%ptr%I1
            IF ( Obj(I)%k1 .NE. N ) STOP 45
            IF ( Obj(I)%l1 .NE. M ) STOP 46
            IF ( SIZE(Obj(I)%A1) .NE. M ) STOP 47
            IF ( LEN(Obj(I)%C1)  .NE. M ) STOP 48
         END DO
      END SUBROUTINE Sub15

END PROGRAM SpecExpHostAssociation02c
