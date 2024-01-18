!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : July  17, 20008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : ASSOCIATE Construct inside a SELECT TYPE Construct
!*                               Unlimited polymorphic
!*                               Host association
!*
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
      PROGRAM Select_Type_Basic02
      IMPLICIT NONE
!*
! DERIVED TYPE declarations
!*
      TYPE Shape (k1,name_len)
        INTEGER, KIND :: k1
        INTEGER, LEN :: name_len

        REAL(k1) :: area
        CHARACTER(LEN=name_len) :: name = ''
      END TYPE Shape

      TYPE, EXTENDS(Shape) :: Square(k2)
        INTEGER, KIND :: k2

        REAL(k2) :: width
      END TYPE Square

      TYPE, EXTENDS(Square) :: Rectangle
        REAL(k2) :: height
      END TYPE Rectangle

      TYPE, EXTENDS(Shape) :: Triangle
        REAL(k1) :: a
        REAL(k1) :: b
        REAL(k1) :: c
        REAL(k1) :: perimeter
      END TYPE Triangle
!*
! Object declarations
!*
      INTEGER, PARAMETER :: k1 = KIND(0.0), k2 = KIND(0.0), name_len = 20
      CLASS(*), POINTER :: My_shape

      CALL generate_shape

      CONTAINS
!*
! area computation depending on the shape
!*
      SUBROUTINE compute_area1

      CLASS(*), ALLOCATABLE :: My_shape  ! different from My_shape in the main program
      TYPE(Square(k1,name_len,k2)), ALLOCATABLE :: A_square

      ALLOCATE(A_square)

      ! Initialization
      A_square%width = 10.0
      A_square%area = -0.0

      ALLOCATE(My_shape, source=A_square)
      IF ( .NOT. ALLOCATED(My_shape)) STOP 10

      SELECT TYPE (My_shape)
         TYPE  IS (Square(8,*,8))
          print *, 'parameters do not match the declared TYPE'
          STOP 11

        TYPE IS (Square(k1,*,k2))
          My_shape%area = My_shape%width**2
          My_shape%name = 'a square'

          print *, 'My shape is ', TRIM(My_shape%name), ' and the area is', FLOOR(My_shape%area)

        TYPE IS (Rectangle(k1,*,k2))
          print *, 'My shape is not a rectangle'
          STOP 12

        CLASSDEFAULT
           print *, 'Area cannot be computed: undefined Shape'
           STOP 13
      END SELECT
!*
      END SUBROUTINE compute_area1

      SUBROUTINE compute_area2

      TYPE(Triangle(k1,name_len)), TARGET :: A_triangle

      A_triangle%a = 20.0
      A_triangle%b = 10.0
      A_triangle%c = 22.36
!*
      A_triangle%area = -0.0

      My_shape => A_triangle        !  My_shape same than the one in the main program
      IF ( .NOT. ASSOCIATED(My_shape)) STOP 20

      SELECT TYPE (My_shape)

        TYPE IS (Triangle(k1,*))
           My_shape%perimeter = (My_shape%a+My_shape%b+My_shape%c)/2.0

           ASSOCIATE ( p => My_shape%perimeter )
                My_shape%area = SQRT( p*(p-My_shape%a)*(p-My_shape%b)*(p-My_shape%c) )
           END ASSOCIATE

           My_shape%name = 'a triangle'

           print *, 'My shape is ', TRIM(My_shape%name), ' and the area is', FLOOR(My_shape%area)

        CLASS IS (Triangle(k1,*))
           print *, 'You should not pick this'
           STOP 21

        CLASS DEFAULT
           print *, 'Area cannot be computed: undefined Shape'
           STOP 22
      END SELECT
!*
      END SUBROUTINE compute_area2

      SUBROUTINE compute_area3

      !  My_shape same than the one in the main program
      ALLOCATE (My_shape , SOURCE = Rectangle(8,50,8)(area=0.0,name='name',width=12.5,height=8.0))
      IF ( .NOT. ASSOCIATED(My_shape)) STOP 30

      SELECT TYPE (A => My_shape)
        TYPE  IS (Square(8,*,8))
          print *, 'My shape is not a square'
          STOP 31

        TYPE IS (Rectangle(k1,*,k2))
          print *, 'parameters do not match the declared TYPE'
          STOP 32

        CLASS IS (Rectangle(k1,*,k2))
          print *, 'parameters do not match the declared TYPE'
          STOP 33

        CLASS IS (Square(8,*,8))
          print *, 'My shape could be a square or a rectangle'

        CLASS IS (Shape(8,*))
          print *, 'It is not the most particuliar CLASS'
          STOP 34

        CLASS DEFAULT
           print *, 'Area cannot be computed: undefined Shape'
           STOP 35
      END SELECT
!*
      END SUBROUTINE compute_area3
!*
! generate pick a shape
!*
      SUBROUTINE generate_shape

        call compute_area1

        call compute_area2

        call compute_area3

      END SUBROUTINE generate_shape
!*
      END PROGRAM Select_Type_Basic02
