!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : July  23, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : USE Association
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
      MODULE Mod1
      IMPLICIT NONE

      INTEGER, PARAMETER :: k1 = KIND(0.0) , name_len = 20

      TYPE Shape (k1,name_len)
        INTEGER, KIND :: k1
        INTEGER, LEN :: name_len

        REAL(KIND=k1) :: area
        CHARACTER(LEN=name_len) :: name = ''
      END TYPE Shape

      TYPE, EXTENDS(Shape) :: Square
        REAL(KIND=k1), PRIVATE :: width
      END TYPE Square

      TYPE, EXTENDS(Square) :: Rectangle
        REAL(KIND=k1), PRIVATE :: height
      END TYPE Rectangle
!*
      CONTAINS

      SUBROUTINE compute_square_area(My_seed,My_shape)

      INTEGER(KIND=4), INTENT(IN) :: My_seed
      CLASS(Square(k1,:)), INTENT(INOUT), ALLOCATABLE :: My_shape
      TYPE(Square(k1,name_len)) :: A_square

      A_square%width = My_seed

      ALLOCATE(My_shape, source = A_square)
      IF ( .NOT. ALLOCATED(My_shape)) STOP 10

      SELECT TYPE (A=>My_shape)
        TYPE IS (Square(k1,*))
          A%area = A%width**2
          A%name = 'a square'

        TYPE IS (Rectangle(k1,*))
           print *, 'My shape is not a rectangle'
           STOP 11

        CLASS DEFAULT
           print *, 'area cannot be computed: Undefined Shape'
           STOP 12
      END SELECT

      END SUBROUTINE compute_square_area

      END MODULE Mod1

      PROGRAM Select_Type_Basic05
      USE Mod1
      IMPLICIT NONE

      INTEGER(4) ::  My_seed = 10.0
      CLASS(Square(4,:)), ALLOCATABLE :: My_shape

      CALL compute_square_area(My_seed,My_shape)

      ASSOCIATE (A => My_shape)
          print *, 'My shape is ', TRIM(A%name), ' and the area is', FLOOR(A%area)
      END ASSOCIATE

      END PROGRAM Select_Type_Basic05
