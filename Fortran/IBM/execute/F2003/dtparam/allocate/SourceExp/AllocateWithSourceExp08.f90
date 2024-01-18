!*  ===================================================================
!*
!*  DATE                       : January 20, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with Source Expression
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
!* allocate-stmt is
!*   ALLOCATE ( [ TYPE-spec :: ] allocation-list [, alloc-opt-list ] )
!*
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM AllocateWithSourceExp08
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 10

        INTEGER(k1) :: my_arr(l1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = KIND(0)
        INTEGER, LEN  :: l2 = 10
      END TYPE Child

      TYPE(Base(4,10)) :: b1
      TYPE(Child(4,10,4,5)) :: c(2)
      CLASS(*), POINTER :: u_poly1, u_poly2(:)

      ALLOCATE(u_poly1, SOURCE = foo(b1) )

      SELECT TYPE ( u_poly1 )
        CLASS IS (Base(4,*))
           IF (u_poly1%l1 .NE. 10) STOP 10
           IF (SIZE(u_poly1%my_arr) .NE. 10) STOP 11

        CLASS DEFAULT
           STOP 12
      END SELECT


      ALLOCATE(u_poly2(SIZE(foo_array(c))), SOURCE = foo_array(c) )
      IF(SIZE(u_poly2) .NE. 2) STOP 13

      SELECT TYPE ( u_poly2 )
        CLASS IS (Child(4,*,4,*))
           IF (u_poly2%l1 .NE. 10) STOP 14
           IF (u_poly2%l2 .NE. 5) STOP 15
           IF (SIZE(u_poly2(1)%my_arr) .NE. 10) STOP 16

        CLASS DEFAULT
           STOP 17
      END SELECT


      CONTAINS

      FUNCTION foo(Arg)
      TYPE(Base(4,*)) :: Arg
      TYPE(Base(4,:)), ALLOCATABLE :: foo

          ALLOCATE(foo, SOURCE = Arg)

      END FUNCTION foo

      FUNCTION foo_array(Arg)
      CLASS(Base(4,*)) :: Arg(:)
      CLASS(Base(4,:)), ALLOCATABLE :: foo_array(:)

          ALLOCATE(foo_array(SIZE(Arg)), SOURCE = Arg)

      END FUNCTION foo_array

END PROGRAM AllocateWithSourceExp08
