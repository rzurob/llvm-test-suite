!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : July  18, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Argument Association
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
      PROGRAM Select_Type_Basic03
      IMPLICIT NONE

      TYPE Shape (k1,len1)
        INTEGER(1), KIND :: k1
        INTEGER(1), LEN :: len1

        REAL(k1) :: area
        CHARACTER(LEN=len1) :: name
      END TYPE Shape

      TYPE, EXTENDS(Shape) :: Square
        REAL(k1) :: width
      END TYPE Square

      TYPE, EXTENDS(Square) :: Rectangle
        REAL(k1) :: height
      END TYPE Rectangle
!*
! Object declarations
!*
      INTEGER(1), PARAMETER :: k1=KIND(0.0), len1 = 20
      INTEGER(1) ::  My_kind

      CLASS(*), POINTER :: My_shape
      TYPE(Square(k1,len1)), TARGET :: A_square
      TYPE(Square(8,50)), TARGET :: A_dsquare
!*
! Initialization
!*
      A_square%width = 10.0D0 + 0.3D-7
      A_dsquare%width = 10.0D0 + 0.3D-7

      A_square%name = 'a square'
      A_dsquare%name = 'a square'

      ALLOCATE(My_shape, source = A_square)
      IF ( .NOT. ASSOCIATED(My_shape)) ERROR STOP 10

      My_kind=k1

      CALL compute_area(My_kind,My_shape)

      ALLOCATE (My_shape , SOURCE = Rectangle(k1,len1)(area=0.0,name='rectangle',width=12.5000002D0,height=8.00000012D0))
      IF ( .NOT. ASSOCIATED(My_shape)) ERROR STOP 11

      CALL compute_area(My_kind,My_shape)

      My_kind=8

      ALLOCATE(My_shape, source = A_dsquare)
      IF ( .NOT. ASSOCIATED(My_shape)) ERROR STOP 12

      CALL compute_area(My_kind,My_shape)

      ALLOCATE (My_shape , SOURCE = Rectangle(2*k1,len1/2)(area=0.0,name='rectangle',width=12.5000002D0,height=8.00000012D0))
      IF ( .NOT. ASSOCIATED(My_shape)) ERROR STOP 13

      CALL compute_area(My_kind,My_shape)

      CONTAINS
!*
! area computation
!*
      SUBROUTINE compute_area(My_kind,My_shape)

      INTEGER(1), INTENT(IN) ::  My_kind
      CLASS(*), INTENT(INOUT), POINTER :: My_shape

      SELECT TYPE (A => My_shape)
        CLASS IS (Square(4,*))
          IF (My_kind .NE. 4) ERROR STOP 20
          print *, 'My shape may or may not be a square: More tests needed'

 		! Nested SELECT TYPE
            	SELECT TYPE (A)
                  TYPE IS (Square(4,*))
                    IF (My_kind .NE. 4) ERROR STOP 31
                    A%area = A%width**2

                    print *, 'My shape is ', TRIM(A%name), ' and the area is', CEILING(A%area)

                  TYPE IS (Rectangle(4,*))
                    IF (My_kind .NE. 4) ERROR STOP 32
                    A%area = A%width*A%height

                    print *, 'My shape is ', TRIM(A%name), ' and the area is', CEILING(A%area)

                  CLASS DEFAULT
                    print *, 'area cannot be computed: Undefined Shape'
                    STOP 33
                END SELECT
 		! END Nested SELECT TYPE

        CLASS IS (Rectangle(4,*))
          IF (My_kind .NE. 4) ERROR STOP 21
          print *, 'My shape may or may not be a rectangle: More tests needed'

 		! Nested SELECT TYPE
            	SELECT TYPE (A)
                  TYPE IS (Rectangle(4,*))
                    IF (My_kind .NE. 4) ERROR STOP 42
                    A%area = A%width*A%height

                    print *, 'My shape is ', TRIM(A%name), ' and the area is', CEILING(A%area)

                  CLASS DEFAULT
                    print *, 'area cannot be computed: Undefined Shape'
                    STOP 43
                END SELECT
 		! END Nested SELECT TYPE


        CLASS IS (Shape(8,*))
          IF (My_kind .NE. 8) ERROR STOP 22
          print *, 'My shape may be a square or a rectangle: More tests needed'

 		! Nested SELECT TYPE
            	SELECT TYPE (A)
                  TYPE IS (Square(8,*))
                    IF (My_kind .NE. 8) ERROR STOP 51
          	    A%area = A%width**2

                    print *, 'My shape is ', TRIM(A%name), ' and the area is precisely', CEILING(A%area)

                  TYPE IS (Rectangle(8,*))
                    IF (My_kind .NE. 8) ERROR STOP 52
                    A%area = A%width*A%height

                    print *, 'My shape is ', TRIM(A%name), ' and the area is precisely', CEILING(A%area)

                    CLASS DEFAULT
                       print *, 'area cannot be computed: Undefined Shape'
                       STOP 53
                END SELECT
 		! END Nested SELECT TYPE

        CLASS DEFAULT
           print *, 'area cannot be computed: Undefined Shape'
           STOP 23
      END SELECT
!*

      My_shape => NULL()

      END SUBROUTINE compute_area

      END PROGRAM Select_Type_Basic03
