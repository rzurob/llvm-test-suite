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
!* Defect 354497
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        CHARACTER(2*l1+k1) :: status
        CHARACTER(l1) :: tag
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        TYPE(Base(k2,l2-l1)), ALLOCATABLE :: b_cmp
      END TYPE Child

      TYPE, EXTENDS(Base) :: Branch  (k3,l3,l4,l5)
        INTEGER, KIND :: k3
        INTEGER, LEN  :: l3, l4, l5

        TYPE(Base(k3,l4)), ALLOCATABLE :: left
        TYPE(Base(k3,l5)), ALLOCATABLE :: right
      END TYPE Branch
END MODULE Mod
PROGRAM AllocateWithTypeSpec05
      USE Mod
      IMPLICIT NONE

      CLASS(Base(4,:)), ALLOCATABLE :: b1

      IF ( ALLOCATED(b1)) ERROR STOP 10

!*   Base

      ALLOCATE(Base(4,8) :: b1)
      CALL verify_type_param(b1)
      IF (b1%tag .NE. 'Base') ERROR STOP 11

      CALL alloc_comp(b1)
      IF (b1%status .NE. '0 allocation done') ERROR STOP 12

      DEALLOCATE(b1)

!*   Child

      ALLOCATE(Child(4,10,4,18) :: b1)
      CALL verify_type_param(b1)
      IF (b1%tag .NE. 'Child') ERROR STOP 13

      CALL alloc_comp(b1)
      IF (b1%status .NE. '1 allocation done') ERROR STOP 14

      DEALLOCATE(b1)

!*   Branch

      ALLOCATE(Branch(4,10,4,5,8,8) :: b1)
      CALL verify_type_param(b1)
      IF (b1%tag .NE. 'Branch') ERROR STOP 15

      CALL alloc_comp(b1)
      IF (b1%status .NE. '2 allocations done') ERROR STOP 16

      DEALLOCATE(b1)


      CONTAINS
!*
      SUBROUTINE verify_type_param(Arg)
         CLASS(Base(4,*)) :: Arg

         SELECT TYPE ( Arg )
              CLASS IS (Base(4,*))
                 Arg%tag = 'Base'
                 IF (Arg%l1 .NE. 8) ERROR STOP 20

              CLASS IS (Child(4,*,4,*))
                 Arg%tag = 'Child'
                 IF (Arg%l1 .NE. 10) ERROR STOP 21
                 IF (Arg%l2 .NE. 18) ERROR STOP 22

              CLASS IS (Branch(4,*,4,*,*,*))
                 Arg%tag = 'Branch'
                 IF (Arg%l3 .NE. 5) ERROR STOP 23
                 IF (Arg%l4 .NE. 8) ERROR STOP 24
                 IF (Arg%l5 .NE. 8) ERROR STOP 25

              CLASS DEFAULT
                 STOP 26
         END SELECT

      END SUBROUTINE verify_type_param

      SUBROUTINE alloc_comp(Arg)
         CLASS(Base(4,:)), ALLOCATABLE :: Arg

         SELECT TYPE ( Arg )
              CLASS IS (Base(4,*))
                 Arg%status = '0 allocation done'

              CLASS IS (Child(4,*,4,*))
                 IF ( ALLOCATED(Arg%b_cmp)) ERROR STOP 30
                 ALLOCATE(Arg%b_cmp)
                 Arg%status = '1 allocation done'

                 CALL verify_type_param(Arg%b_cmp)
                 IF (Arg%b_cmp%tag .NE. 'Base') ERROR STOP 31

              CLASS IS (Branch(4,*,4,*,*,*))
                 IF ( ALLOCATED(Arg%left)) ERROR STOP 32
                 IF ( ALLOCATED(Arg%right)) ERROR STOP 33
                 ALLOCATE(Base(4,Arg%l4) :: Arg%left)
                 ALLOCATE(Base(4,Arg%l5) :: Arg%right)
                 Arg%status = '2 allocations done'

                 CALL verify_type_param(Arg%left)
                 IF (Arg%left%tag .NE. 'Base') ERROR STOP 34

                 CALL verify_type_param(Arg%right)
                 IF (Arg%right%tag .NE. 'Base') ERROR STOP 35

              CLASS DEFAULT
                 STOP 36
         END SELECT


      END SUBROUTINE alloc_comp

END PROGRAM AllocateWithTypeSpec05