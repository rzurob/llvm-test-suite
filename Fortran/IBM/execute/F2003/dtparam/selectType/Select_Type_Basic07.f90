!*  ===================================================================
!*
!*  DATE                       : July  23, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : USE Association with ONLY and RENAME
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
!*#                FIRST MODULE : DECLARATIONS OF TYPES              #
!*####################################################################
      MODULE Mod1
      IMPLICIT NONE

      INTEGER, PARAMETER :: k1 = KIND(0.0), len1 = 20
      REAL, PARAMETER ::  seed1=10.0, seed2=5.0

      TYPE Shape (k1,len1)
        INTEGER, KIND :: k1 = KIND(0.0)
        INTEGER, LEN :: len1 = 20

        REAL(k1) :: area
        CHARACTER(len1) :: name
      END TYPE Shape

      TYPE, EXTENDS(Shape) :: Square
        REAL(k1) :: width
      END TYPE Square

      TYPE, EXTENDS(Square) :: Rectangle
        REAL(k1) :: height
      END TYPE Rectangle

      END MODULE Mod1

!*####################################################################
!*#                SECOND MODULE : DECLARATIONS OF SUBPROGRAMS       #
!*####################################################################
      MODULE Mod2
      USE Mod1, Sq => Square, Rec => Rectangle
      IMPLICIT NONE

      CONTAINS

      SUBROUTINE compute_area1(My_shape)
      CLASS(*), INTENT(INOUT), POINTER :: My_shape
      TYPE(Sq(4,20)), TARGET, ALLOCATABLE :: A_square

      ALLOCATE(A_square)

      A_square%width = seed1

      My_shape => A_square
      IF ( .NOT. ASSOCIATED(My_shape)) STOP 10

      SELECT TYPE (A => My_shape)
        CLASS IS (Sq(4,*))
            A%name = 'a square'
            print *, 'My shape could be ', TRIM(A%name)

        CLASS IS (Rec(4,*))
            A%name = 'a rectangle'
            print *, 'My shape could be ', TRIM(A%name)

        TYPE IS (Sq(4,*))
           A%area = compute_square_area(A%width)
           A%name = 'a square'
           print *, 'My shape is ', TRIM(A%name), ' and the area is', FLOOR(A%area)

        TYPE IS (Rec(4,*))
           A%height=seed2
           A%area = compute_rectangle_area(A%width,A%height)
           A%name = 'a rectangle'
           print *, 'My shape is ', TRIM(A%name), ' and the area is', FLOOR(A%area)

        CLASS DEFAULT
           print *, 'area cannot be computed: Undefined Shape'
           STOP 11
      END SELECT
      END SUBROUTINE compute_area1
!*
      SUBROUTINE compute_area2(My_shape)
      CLASS(*), INTENT(INOUT), POINTER :: My_shape

      ALLOCATE(My_shape, source = Rec(4,20)(area=0.0,name = '',width=20.0,height=12.5))
      IF ( .NOT. ASSOCIATED(My_shape)) STOP 10

      SELECT TYPE (A => My_shape)
        TYPE IS (Sq(4,*))
           A%area = compute_square_area(A%width)
           A%name = 'a square'
           print *, 'My shape is ', TRIM(A%name), ' and the area is', FLOOR(A%area)

        TYPE IS (Rec(4,*))
           A%height=seed2
           A%area = compute_rectangle_area(A%width,A%height)
           A%name = 'a rectangle'
           print *, 'My shape is ', TRIM(A%name), ' and the area is', FLOOR(A%area)

        CLASS DEFAULT
           print *, 'area cannot be computed: Undefined Shape'
           STOP 11
      END SELECT
      END SUBROUTINE compute_area2
!*
      SUBROUTINE compute_area3(My_shape)
      CLASS(*), INTENT(INOUT), POINTER :: My_shape

      ALLOCATE(My_shape, source = Rec(4,20)(area=0.0,name = '',width=20.0,height=12.5))
      IF ( .NOT. ASSOCIATED(My_shape)) STOP 10

      SELECT TYPE (A => My_shape)
        CLASS IS (Sq(4,*))
            A%name = 'a square'
            print *, 'My shape could be ', TRIM(A%name)

        CLASS IS (Rec(4,*))
            A%name = 'a rectangle'
            print *, 'My shape could be ', TRIM(A%name)

        CLASS DEFAULT
           print *, 'area cannot be computed: Undefined Shape'
           STOP 11
      END SELECT
      END SUBROUTINE compute_area3

!*
!* compute the area of a square
!*
      PURE REAL FUNCTION compute_square_area(x) result (answer)
      REAL, INTENT(IN) ::  x

      answer=x**2

      END FUNCTION compute_square_area
!*
!* compute the area of a rectangle
!*
      PURE REAL FUNCTION compute_rectangle_area(x,y) result (answer)
      REAL, INTENT(IN) ::  x,y

      answer=x*y

      END FUNCTION compute_rectangle_area
!*
      END MODULE Mod2
!*
!* Main program
!*
      PROGRAM Select_Type_Basic07
      USE Mod2, ONLY: compute_area1, compute_area2, compute_area3
      IMPLICIT NONE

      CLASS(*), POINTER :: My_shape

      CALL compute_area1(My_shape)

      CALL compute_area2(My_shape)

      CALL compute_area3(My_shape)

      END PROGRAM Select_Type_Basic07
