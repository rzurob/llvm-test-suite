!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AllocateWithTypeSpec15
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
!*  KEYWORD(S)                 : Deferred LEN parameter
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!* allocate-stmt is
!*   ALLOCATE ( [ type-spec :: ] allocation-list [, alloc-opt-list ] )
!*
!*  Defect 361310                
!* 
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM AllocateWithTypeSpec15
      IMPLICIT NONE 

      TYPE Base  (k1,l1,l2)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 1
        INTEGER, LEN  :: l2 = 2

        CHARACTER(l1)  :: name  
        INTEGER(k1) :: my_arr(l1,l2)
      END TYPE Base

      TYPE Child (k2,l1,l2)
        INTEGER, KIND :: k2 = KIND(0)
        INTEGER, LEN  :: l1 = 10
        INTEGER, LEN  :: l2 = 5

        TYPE(Base(k2,l1,l1-l2)), POINTER :: b_cmp => NULL()
        TYPE(Base(k2,l2,l1+l2)), POINTER :: c_cmp => NULL()
      END TYPE Child

      CALL Alloc_auto_child21(2,1)

      CALL Alloc_auto_child73(7,3) 

      CALL Alloc_auto_child105(10,5)

      CONTAINS

      SUBROUTINE Alloc_auto_child21(n, m)
      INTEGER :: n
      INTEGER :: m

      CLASS(Child(4,2,1)), ALLOCATABLE :: c1
      CLASS(Child(4,n,m)), ALLOCATABLE :: c2
      CLASS(Child(4,:,:)), ALLOCATABLE :: c3

      ALLOCATE(Child(4,2,1) :: c1 )
      ALLOCATE(c1%b_cmp, c1%c_cmp)

      ALLOCATE(Child(4,n,m) :: c2 )
      ALLOCATE(c2%b_cmp, c2%c_cmp)

      ALLOCATE(Child(4,n,m) :: c3 )
      ALLOCATE(c3%b_cmp, c3%c_cmp)

      IF (c2%l1 .NE. c1%l1) STOP 10
      IF (c2%l2 .NE. c1%l2) STOP 11
      IF (c2%b_cmp%l1 .NE. c1%b_cmp%l1 ) STOP 12
      IF (c2%b_cmp%l2 .NE. c1%b_cmp%l2 ) STOP 13
      IF (SIZE(c2%b_cmp%my_arr) .NE. SIZE(c1%b_cmp%my_arr)) STOP 14
      IF (SIZE(c2%c_cmp%my_arr) .NE. SIZE(c1%c_cmp%my_arr)) STOP 15

      IF (c3%l1 .NE. c1%l1) STOP 16
      IF (c3%l2 .NE. c1%l2) STOP 17
      IF (c3%b_cmp%l1 .NE. c1%b_cmp%l1 ) STOP 18
      IF (c3%b_cmp%l2 .NE. c1%b_cmp%l2 ) STOP 19
      IF (SIZE(c3%b_cmp%my_arr) .NE. SIZE(c1%b_cmp%my_arr)) STOP 20
      IF (SIZE(c3%c_cmp%my_arr) .NE. SIZE(c1%c_cmp%my_arr)) STOP 21

      DEALLOCATE(c1,c2,c3)

      END SUBROUTINE Alloc_auto_child21

      SUBROUTINE Alloc_auto_child73(n, m)
      INTEGER :: n
      INTEGER :: m

      CLASS(Child(4,7,3)), ALLOCATABLE :: c1
      CLASS(Child(4,n,m)), ALLOCATABLE :: c2
      CLASS(Child(4,:,:)), ALLOCATABLE :: c3

      ALLOCATE(Child(4,7,3) :: c1 )
      ALLOCATE(c1%b_cmp, c1%c_cmp)

      ALLOCATE(Child(4,n,m) :: c2 )
      ALLOCATE(c2%b_cmp, c2%c_cmp)

      ALLOCATE(Child(4,n,m) :: c3 )
      ALLOCATE(c3%b_cmp, c3%c_cmp)

      IF (c2%l1 .NE. c1%l1) STOP 30
      IF (c2%l2 .NE. c1%l2) STOP 31
      IF (c2%b_cmp%l1 .NE. c1%b_cmp%l1 ) STOP 32
      IF (c2%b_cmp%l2 .NE. c1%b_cmp%l2 ) STOP 33
      IF (SIZE(c2%b_cmp%my_arr) .NE. SIZE(c1%b_cmp%my_arr)) STOP 34
      IF (SIZE(c2%c_cmp%my_arr) .NE. SIZE(c1%c_cmp%my_arr)) STOP 35

      IF (c3%l1 .NE. c1%l1) STOP 36
      IF (c3%l2 .NE. c1%l2) STOP 37
      IF (c3%b_cmp%l1 .NE. c1%b_cmp%l1 ) STOP 38
      IF (c3%b_cmp%l2 .NE. c1%b_cmp%l2 ) STOP 39
      IF (SIZE(c3%b_cmp%my_arr) .NE. SIZE(c1%b_cmp%my_arr)) STOP 40
      IF (SIZE(c3%c_cmp%my_arr) .NE. SIZE(c1%c_cmp%my_arr)) STOP 41

      DEALLOCATE(c1,c2,c3)

      END SUBROUTINE Alloc_auto_child73 

      SUBROUTINE Alloc_auto_child105(n, m)
      INTEGER :: n
      INTEGER :: m

      CLASS(Child(4,10,5)), ALLOCATABLE :: c1
      CLASS(Child(4,n,m)), ALLOCATABLE :: c2
      CLASS(Child(4,:,:)), ALLOCATABLE :: c3

      ALLOCATE(Child(4,10,5) :: c1 )
      ALLOCATE(c1%b_cmp, c1%c_cmp)

      ALLOCATE(Child(4,n,m) :: c2 )
      ALLOCATE(c2%b_cmp, c2%c_cmp)

      ALLOCATE(Child(4,n,m) :: c3 )
      ALLOCATE(c3%b_cmp, c3%c_cmp)

      IF (c2%l1 .NE. c1%l1) STOP 50
      IF (c2%l2 .NE. c1%l2) STOP 51
      IF (c2%b_cmp%l1 .NE. c1%b_cmp%l1 ) STOP 52
      IF (c2%b_cmp%l2 .NE. c1%b_cmp%l2 ) STOP 53
      IF (SIZE(c2%b_cmp%my_arr) .NE. SIZE(c1%b_cmp%my_arr)) STOP 54
      IF (SIZE(c2%c_cmp%my_arr) .NE. SIZE(c1%c_cmp%my_arr)) STOP 55

      IF (c3%l1 .NE. c1%l1) STOP 56
      IF (c3%l2 .NE. c1%l2) STOP 57
      IF (c3%b_cmp%l1 .NE. c1%b_cmp%l1 ) STOP 58
      IF (c3%b_cmp%l2 .NE. c1%b_cmp%l2 ) STOP 59
      IF (SIZE(c3%b_cmp%my_arr) .NE. SIZE(c1%b_cmp%my_arr)) STOP 60
      IF (SIZE(c3%c_cmp%my_arr) .NE. SIZE(c1%c_cmp%my_arr)) STOP 61

      DEALLOCATE(c1,c2,c3)

      END SUBROUTINE Alloc_auto_child105

END PROGRAM AllocateWithTypeSpec15
