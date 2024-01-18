!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Select_Type_Basic04 - SELECT TYPE 
!*                               DTP-SELECT TYPE Construct
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : July  21, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : ASSOCIATE Construct inside a SELECT TYPE Construct
!*                               
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : SELECT TYPE Construct
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION                :
!*
!*  8.1.5.1 Form of the SELECT TYPE construct
!*
!*  R821 select-type-construct  is  select-type-stmt
!*                                      [ type-guard-stmt
!*                                        block ] ...
!*                                      end-select-type-stmt
!*  R822 select-type-stmt       is  [ select-construct-name : ] SELECT TYPE&
!*                                      &( [ associate-name => ] selector )
!*
!*  R823 type-guard-stmt is TYPE IS ( type-spec ) [ select-construct-name ]
!*                       or CLASS IS ( type-spec ) [ select-construct-name ]
!*                       or CLASS DEFAULT [ select-construct-name ]
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      PROGRAM Select_Type_Basic04
      IMPLICIT NONE 
!*
! DERIVED TYPE declarations
!*
      TYPE Shape (k1,name_len)
        INTEGER, KIND :: k1 = KIND(0.0)  !k1=4
        INTEGER, LEN :: name_len = 20

        REAL(KIND=k1) :: area
        CHARACTER(LEN=name_len) :: name = ''
      END TYPE Shape

      TYPE, EXTENDS(Shape) :: Square
        REAL(KIND=k1) :: width
      END TYPE Square   

      TYPE, EXTENDS(Square) :: Rectangle
        REAL(KIND=k1) :: height 
      END TYPE Rectangle

      TYPE, EXTENDS(Shape) :: Triangle
        REAL(KIND=k1) :: a  !first side of the triangle
        REAL(KIND=k1) :: b  !second side of the triangle
        REAL(KIND=k1) :: c  !third side of the triangle
        REAL(KIND=k1) :: perimeter ! perimeter of the triangle
      END TYPE Triangle

      TYPE, EXTENDS(Triangle) :: ExtTriangle1
      END TYPE ExtTriangle1

      TYPE, EXTENDS(Triangle) :: ExtTriangle2
      END TYPE ExtTriangle2
!*
! Object declarations
!*
      CLASS(*), ALLOCATABLE :: My_shape1
      CLASS(*), POINTER :: My_shape2
!*
      CALL compute_shape1(My_shape1)
      CALL compute_shape2(My_shape2)

      CONTAINS
!*
      SUBROUTINE compute_shape2(My_shape)

      INTEGER(KIND=1), PARAMETER :: k1=KIND(0.0), name_len = 20 !k1=4
      CLASS(*), POINTER :: My_shape , A
      TYPE(Triangle(k1,name_len)), TARGET :: A_triangle

      A_triangle%a = 20.0
      A_triangle%b = 10.0
      A_triangle%c = 22.36
!*
      My_shape => A_triangle
      IF ( .NOT. ASSOCIATED(My_shape)) STOP 20

      SELECT TYPE (A => My_shape)
        CLASS IS (ExtTriangle1(k1,*))
           print *, 'You should not pick this'
           STOP 21

        CLASS IS (ExtTriangle2(k1,*))
           print *, 'You should not pick this'
           STOP 22

        CLASS IS (Triangle(k1,*))
                A%perimeter = (A%a + A%b + A%c)/2.0
           ASSOCIATE ( p => A%perimeter )
                A%area = SQRT( p*(p-A%a)*(p-A%b)*(p-A%c) )
           END ASSOCIATE

           A%name = 'a triangle'

           print *, 'My shape is ', TRIM(A%name), ' and the area is', INT(A%area)

        CLASSDEFAULT
           print *, 'Area cannot be computed: undefined Shape'
           STOP 23
      END SELECT
!*
      END SUBROUTINE compute_shape2

      SUBROUTINE compute_shape1(My_shape)

      INTEGER(KIND=1), PARAMETER :: k1=KIND(0.0D0), name_len = 40 !k1=8
      CLASS(*), ALLOCATABLE :: My_shape
      TYPE(Square(k1,name_len)), ALLOCATABLE :: A_square
!*
      ALLOCATE(A_square)
      
      A_square%width = 10.0D0
 
      ALLOCATE(My_shape, source = A_square)
      IF ( .NOT. ALLOCATED(My_shape)) STOP 10

      SELECT TYPE (A => My_shape)
        TYPE IS (Square(k1,*))
          A%area = A%width**2
          A%name = 'a square'

          print *, 'My shape is ', TRIM(A%name), ' and the area is', INT(A%area)

        TYPE IS (Rectangle(k1,*))
          print *, 'You should not pick this'
          STOP 11

        CLASSDEFAULT
          print *, 'area cannot be computed: Undefined Shape' 
          STOP 12
      END SELECT
!*
      END SUBROUTINE compute_shape1
!*
      END PROGRAM Select_Type_Basic04
