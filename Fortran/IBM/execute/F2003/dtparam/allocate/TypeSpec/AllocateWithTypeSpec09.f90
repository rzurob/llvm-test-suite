!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AllocateWithTypeSpec09 
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : January 20, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with type-spec
!*  SECONDARY FUNCTIONS TESTED :
!*                               
!*
!*  DRIVER STANZA              : xlf2003
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
!* Defect 361707 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE Mod
      IMPLICIT NONE 

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 
        INTEGER, LEN  :: l1 

        CHARACTER(2*l1+k1) :: status
        CHARACTER(l1) :: tag
        INTEGER(k1), ALLOCATABLE :: my_arr(:)  
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        TYPE(Base(k2,l2)), ALLOCATABLE :: b_cmp   
      END TYPE Child

      TYPE, EXTENDS(Child) :: Branch  (k3,l3,l4,l5)
        INTEGER, KIND :: k3 
        INTEGER, LEN  :: l3, l4, l5

        TYPE(Base(k3,l4)), ALLOCATABLE :: left   
        TYPE(Base(k3,l5)), ALLOCATABLE :: right 
      END TYPE Branch
END MODULE Mod
PROGRAM AllocateWithTypeSpec09
      USE Mod
      IMPLICIT NONE

      INTEGER :: i 
      CLASS(Base(4,:)), POINTER :: b1

      IF ( ASSOCIATED(b1)) STOP 10

!*   Base

      ALLOCATE(Base(4,8) :: b1)
      CALL verify_type_param(b1)
      IF (b1%tag .NE. 'Base') STOP 11

      CALL alloc_comp(b1)
      IF (b1%status .NE. '1 allocation done') STOP 12

!*   Child 

      ALLOCATE(Child(4,10,4,8) :: b1)
      CALL verify_type_param(b1)
      IF (b1%tag .NE. 'Child') STOP 13

      CALL alloc_comp(b1)
      IF (b1%status .NE. '3 allocations done') STOP 14

!*   Branch 

      ALLOCATE(Branch(4,10,4,8,4,5,8,8) :: b1)
      CALL verify_type_param(b1)
      IF (b1%tag .NE. 'Branch') STOP 15

      CALL alloc_comp(b1)
      IF (b1%status .NE. '7 allocations done') STOP 16

      DEALLOCATE(b1)

      CONTAINS
!*
      SUBROUTINE verify_type_param(Arg)
         CLASS(Base(4,*)) :: Arg    

         SELECT TYPE ( Arg )  
              CLASS IS (Base(4,*))
                 Arg%tag = 'Base'
                 IF (Arg%l1 .NE. 8) STOP 20

              CLASS IS (Child(4,*,4,*))
                 Arg%tag = 'Child'
                 IF (Arg%l1 .NE. 10) STOP 21
                 IF (Arg%l2 .NE. 8) STOP 22

              CLASS IS (Branch(4,*,4,*,4,*,*,*))
                 Arg%tag = 'Branch'
                 IF (Arg%l1 .NE. 10) STOP 23
                 IF (Arg%l2 .NE. 8) STOP 24
                 IF (Arg%l3 .NE. 5) STOP 25
                 IF (Arg%l4 .NE. 8) STOP 26
                 IF (Arg%l5 .NE. 8) STOP 27

              CLASS DEFAULT
                 STOP 28
         END SELECT

      END SUBROUTINE verify_type_param

      SUBROUTINE verify_my_arr(Arr, k) 
         INTEGER :: k, Arr(:)  
 
         IF ( ANY(Arr .NE. k))  STOP 100 

      END SUBROUTINE verify_my_arr    

      SUBROUTINE alloc_comp(Arg)
         CLASS(Base(4,*)) :: Arg    

         SELECT TYPE ( Arg )  
              CLASS IS (Base(4,*))
                 ALLOCATE(Arg%my_arr(Arg%l1), SOURCE=(/(1, i = 1, Arg%l1)/))
                 IF (SIZE(Arg%my_arr) .NE. 8) STOP 30 
                 CALL verify_my_arr(Arg%my_arr, 1) 

                 Arg%status = '1 allocation done'

              CLASS IS (Child(4,*,4,*))
                 ALLOCATE(Arg%my_arr(Arg%l2), SOURCE=(/(2, i = 1, Arg%l2)/))
                 IF (SIZE(Arg%my_arr) .NE. 8) STOP 31 
                 CALL verify_my_arr(Arg%my_arr, 2) 

                 IF ( ALLOCATED(Arg%b_cmp)) STOP 32
                 ALLOCATE(Base(4,Arg%l2) :: Arg%b_cmp) 
                 ALLOCATE(Arg%b_cmp%my_arr(8), SOURCE=3)
                 IF (SIZE(Arg%b_cmp%my_arr) .NE. 8) STOP 33 
                 CALL verify_my_arr(Arg%b_cmp%my_arr, 3) 

                 Arg%status = '3 allocations done'

                 CALL verify_type_param(Arg%b_cmp)
                 IF (Arg%b_cmp%tag .NE. 'Base') STOP 34

              CLASS IS (Branch(4,*,4,*,4,*,*,*))
                 ALLOCATE(Arg%my_arr(Arg%l3), SOURCE=(/(4, i = 1, Arg%l3)/))
                 IF (SIZE(Arg%my_arr) .NE. 5) STOP 35
                 CALL verify_my_arr(Arg%my_arr, 4) 

                 IF ( ALLOCATED(Arg%b_cmp)) STOP 36
                 IF ( ALLOCATED(Arg%left)) STOP 37
                 IF ( ALLOCATED(Arg%right)) STOP 38

                 ALLOCATE(Base(4,Arg%l2) :: Arg%b_cmp) 
                 ALLOCATE(Arg%b_cmp%my_arr(8), SOURCE=5)
                 IF (SIZE(Arg%b_cmp%my_arr) .NE. 8) STOP 39 
                 CALL verify_my_arr(Arg%b_cmp%my_arr, 5) 

                 ALLOCATE(Arg%left) 
                 ALLOCATE(Arg%left%my_arr(8), SOURCE=6)
                 IF (SIZE(Arg%left%my_arr) .NE. 8) STOP 40 
                 CALL verify_my_arr(Arg%left%my_arr, 6) 

                 ALLOCATE(Arg%right) 
                 ALLOCATE(Arg%right%my_arr(8), SOURCE=7)
                 IF (SIZE(Arg%right%my_arr) .NE. 8) STOP 41 
                 CALL verify_my_arr(Arg%right%my_arr, 7) 

                 Arg%status = '7 allocations done'

                 CALL verify_type_param(Arg%b_cmp)
                 IF (Arg%b_cmp%tag .NE. 'Base') STOP 42

                 CALL verify_type_param(Arg%left)
                 IF (Arg%left%tag .NE. 'Base') STOP 43

                 CALL verify_type_param(Arg%right)
                 IF (Arg%right%tag .NE. 'Base') STOP 44

              CLASS DEFAULT
                 STOP 45
         END SELECT

      END SUBROUTINE alloc_comp

END PROGRAM AllocateWithTypeSpec09
