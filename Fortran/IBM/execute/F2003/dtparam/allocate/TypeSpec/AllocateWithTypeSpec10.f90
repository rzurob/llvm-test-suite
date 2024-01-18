!*  ===================================================================
!*
!*  DATE                       : January 20, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with type-spec
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : Component with deferred len type parameter
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!* allocate-stmt is
!*   ALLOCATE ( [ type-spec :: ] allocation-list [, alloc-opt-list ] )
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        CHARACTER(3*l1) :: status
        CHARACTER(l1) :: tag
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        TYPE(Base(k2,:)), POINTER :: b_cmp
      END TYPE Child

      TYPE, EXTENDS(Child) :: Branch  (k3,l3,l4,l5)
        INTEGER, KIND :: k3
        INTEGER, LEN  :: l3, l4, l5

        TYPE(Base(k3,:)), POINTER :: left
        TYPE(Base(k3,:)), POINTER :: right
      END TYPE Branch
END MODULE Mod
PROGRAM AllocateWithTypeSpec10
      USE Mod
      IMPLICIT NONE

      CLASS(Base(4,:)), POINTER :: b1

      IF ( ASSOCIATED(b1)) ERROR STOP 10

!*   Base

      ALLOCATE(Base(4,8) :: b1)
      CALL verify_type_param(b1)
      IF (b1%tag .NE. 'Base') ERROR STOP 11

      CALL alloc_comp(b1)
      IF (b1%status .NE. '0 allocation done') ERROR STOP 12

!*   Child

      ALLOCATE(Child(4,10,4,8) :: b1)
      CALL verify_type_param(b1)
      IF (b1%tag .NE. 'Child') ERROR STOP 13

      CALL alloc_comp(b1)
      IF (b1%status .NE. '1 allocation done') ERROR STOP 14

!*   Branch

      ALLOCATE(Branch(4,10,4,8,4,5,8,8) :: b1)
      CALL verify_type_param(b1)
      IF (b1%tag .NE. 'Branch') ERROR STOP 15

      CALL alloc_comp(b1)
      IF (b1%status .NE. '3 allocations done') ERROR STOP 16

      DEALLOCATE(b1)

      CONTAINS
!*
      SUBROUTINE verify_type_param(Arg)
         CLASS(*) :: Arg

         SELECT TYPE ( Arg )
              CLASS IS (Base(4,*))
                 Arg%tag = 'Base'
                 IF (Arg%l1 .NE. 8) ERROR STOP 20

              CLASS IS (Child(4,*,4,*))
                 Arg%tag = 'Child'
                 IF (Arg%l1 .NE. 10) ERROR STOP 21
                 IF (Arg%l2 .NE. 8) ERROR STOP 22

              CLASS IS (Branch(4,*,4,*,4,*,*,*))
                 Arg%tag = 'Branch'
                 IF (Arg%l1 .NE. 10) ERROR STOP 23
                 IF (Arg%l2 .NE. 8) ERROR STOP 24
                 IF (Arg%l3 .NE. 5) ERROR STOP 25
                 IF (Arg%l4 .NE. 8) ERROR STOP 26
                 IF (Arg%l5 .NE. 8) ERROR STOP 27

              CLASS DEFAULT
                 STOP 28
         END SELECT

      END SUBROUTINE verify_type_param

      SUBROUTINE alloc_comp(Arg)
         CLASS(Base(4,*)) :: Arg

         SELECT TYPE ( Arg )
              CLASS IS (Base(4,*))
                 Arg%status = '0 allocation done'

              CLASS IS (Child(4,*,4,*))
                 IF ( ASSOCIATED(Arg%b_cmp)) ERROR STOP 30
                 ALLOCATE(Base(4,Arg%l2) :: Arg%b_cmp)

                 Arg%status = '1 allocation done'

                 CALL verify_type_param(Arg%b_cmp)
                 IF (Arg%b_cmp%tag .NE. 'Base') ERROR STOP 31

              CLASS IS (Branch(4,*,4,*,4,*,*,*))
                 IF ( ASSOCIATED(Arg%b_cmp)) ERROR STOP 32
                 IF ( ASSOCIATED(Arg%left)) ERROR STOP 33
                 IF ( ASSOCIATED(Arg%right)) ERROR STOP 34

                 ALLOCATE(Base(4,Arg%l2) :: Arg%b_cmp)
                 ALLOCATE(Base(4,Arg%l4) :: Arg%left)
                 ALLOCATE(Base(4,Arg%l5) :: Arg%right)

                 Arg%status = '3 allocations done'

                 CALL verify_type_param(Arg%b_cmp)
                 IF (Arg%b_cmp%tag .NE. 'Base') ERROR STOP 35

                 CALL verify_type_param(Arg%left)
                 IF (Arg%left%tag .NE. 'Base') ERROR STOP 36

                 CALL verify_type_param(Arg%right)
                 IF (Arg%right%tag .NE. 'Base') ERROR STOP 37

              CLASS DEFAULT
                 STOP 38
         END SELECT

      END SUBROUTINE alloc_comp

END PROGRAM AllocateWithTypeSpec10
