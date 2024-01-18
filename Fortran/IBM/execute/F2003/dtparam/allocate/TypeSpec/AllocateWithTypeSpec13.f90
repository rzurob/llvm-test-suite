!*  ===================================================================
!*
!*  DATE                       : January 20, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with type-spec
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : Deferred len type parameter
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!* allocate-stmt is
!*   ALLOCATE ( [ type-spec :: ] allocation-list [, alloc-opt-list ] )
!*
!* Defect 361702
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        REAL(k1)      :: data(l1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        REAL(k2)      :: mat(l2,l1)
        CLASS(Base(k2,l2-l1)), ALLOCATABLE :: b_cmp
      END TYPE Child

      TYPE, EXTENDS(Base) :: Branch  (k3,l3,l4,l5)
        INTEGER, KIND :: k3
        INTEGER, LEN  :: l3, l4, l5

        CLASS(Base(k3,l4)), ALLOCATABLE :: left
        CLASS(Base(k3,l5)), ALLOCATABLE :: right
      END TYPE Branch
END MODULE Mod
PROGRAM AllocateWithTypeSpec13
      USE Mod
      IMPLICIT NONE

      CLASS(*), ALLOCATABLE :: upoly
      CHARACTER(20) :: tag

      IF ( ALLOCATED(upoly)) STOP 10

!*   Base

      ALLOCATE(Base(4,8) :: upoly)
      CALL verify_type(upoly)
      IF (tag .NE. 'Base') STOP 11

      CALL alloc_comp(upoly)

      DEALLOCATE(upoly)

!*   Child

      ALLOCATE(Child(4,8,4,16) :: upoly)
      CALL verify_type(upoly)
      IF (tag .NE. 'Child') STOP 12

      CALL alloc_comp(upoly)

      DEALLOCATE(upoly)

!*   Branch

      ALLOCATE(Branch(4,10,4,5,8,8) :: upoly)
      CALL verify_type(upoly)
      IF (tag .NE. 'Branch') STOP 13

      CALL alloc_comp(upoly)

      DEALLOCATE(upoly)

      CONTAINS

      SUBROUTINE verify_type(Arg)
         CLASS(*) :: Arg

         SELECT TYPE ( Arg )
              CLASS IS (Base(4,*))
                 tag = 'Base'
                 IF (Arg%l1 .NE. 8) STOP 20
                 IF (Size(Arg%data) .NE. Arg%l1) STOP 21

              CLASS IS (Child(4,*,4,*))
                 tag = 'Child'
                 IF (Arg%l1 .NE. 8) STOP 22
                 IF (Arg%l2 .NE. 16) STOP 23
                 IF (Size(Arg%data) .NE. Arg%l1) STOP 24
                 IF (Size(Arg%mat)  .NE. Arg%l1*Arg%l2) STOP 25

              CLASS IS (Branch(4,*,4,*,*,*))
                 tag = 'Branch'
                 IF (Arg%l3 .NE. 5) STOP 26
                 IF (Arg%l4 .NE. 8) STOP 27
                 IF (Arg%l5 .NE. 8) STOP 28
                 IF (Size(Arg%data) .NE. Arg%l1) STOP 29

              CLASS DEFAULT
                 STOP 30
         END SELECT

      END SUBROUTINE verify_type

      SUBROUTINE alloc_comp(Arg)
         CLASS(*), ALLOCATABLE :: Arg

         SELECT TYPE ( Arg )
              CLASS IS (Base(4,*))

              CLASS IS (Child(4,*,4,*))
                 IF ( ALLOCATED(Arg%b_cmp)) STOP 40
                 ALLOCATE(Child(4,Arg%l2-Arg%l1,4,Arg%l2) :: Arg%b_cmp)

                 CALL verify_type(Arg%b_cmp)
                 IF (tag .NE. 'Child') STOP 41

              CLASS IS (Branch(4,*,4,*,*,*))
                 IF ( ALLOCATED(Arg%left)) STOP 42
                 IF ( ALLOCATED(Arg%right)) STOP 43
                 ALLOCATE(Branch(4,Arg%l4,4,Arg%l1-Arg%l3,Arg%l4,Arg%l4) :: Arg%left)
                 ALLOCATE(Branch(4,Arg%l5,4,Arg%l1-Arg%l3,Arg%l5,Arg%l5) :: Arg%right)

                 CALL verify_type(Arg%left)
                 IF (tag .NE. 'Branch') STOP 44

                 CALL verify_type(Arg%right)
                 IF (tag .NE. 'Branch') STOP 45

              CLASS DEFAULT
                 STOP 46
         END SELECT

      END SUBROUTINE alloc_comp

END PROGRAM AllocateWithTypeSpec13
