!*  ===================================================================
!*
!*  DATE                       : January 20, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with type-spec
!*  SECONDARY FUNCTIONS TESTED :
!*
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
!*  Defect 359976
!*
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM AllocateWithTypeSpec14
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 1

        CHARACTER(l1)  :: name
        INTEGER(k1) :: my_arr(l1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = KIND(0)
        INTEGER, LEN  :: l2 = 2

        CLASS(Base(k2,l2)), POINTER :: b_cmp => NULL()
        CLASS(Base(k2,l2)), POINTER :: c_cmp => NULL()
      END TYPE Child

!* test Base

      CALL Alloc_auto2base(1)

      CALL Alloc_auto2base(5)

      CALL Alloc_auto2base(10)

      CALL Alloc_auto2base(20)

!* test Child

      CALL Alloc_auto2child(1)

      CALL Alloc_auto2child(5)

      CALL Alloc_auto2child(10)

      CALL Alloc_auto2child(20)

      CONTAINS

      SUBROUTINE Alloc_auto2base(n)
      INTEGER :: i, n
      TYPE(Base(4,n)), POINTER :: Obj

      Allocate(Base(4,n) :: Obj)
      Obj = Base(4,n)('XLFtest',(/(i, i = 1, n)/))

      print*, Obj%name
      print*, Obj%my_arr

      END SUBROUTINE Alloc_auto2base

      SUBROUTINE Alloc_auto2child(n)
      INTEGER :: i, n
      TYPE(Child(4,n,4,2*n)), POINTER :: Obj
      TYPE(Base(4,Obj%l2)), TARGET :: tgt1, tgt2

      Allocate(Child(4,n,4,2*n) :: Obj)
      Obj = Child(4,n,4,2*n)('XLFtest', (/(i, i = 1, n)/), tgt1, tgt2)

      print*, Obj%name
      print*, Obj%my_arr
      print*,  ASSOCIATED(Obj%b_cmp, tgt1)
      print*,  ASSOCIATED(Obj%c_cmp, tgt2)
      IF ( Obj%b_cmp%l1 .NE. 2*n ) ERROR STOP 20
      IF ( Obj%c_cmp%l1 .NE. 2*n ) ERROR STOP 21

      END SUBROUTINE Alloc_auto2child

END PROGRAM AllocateWithTypeSpec14