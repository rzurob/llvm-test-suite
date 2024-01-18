!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Select_Type_Basic08 - SELECT TYPE 
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : July  24, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : 
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
!*####################################################################
!*#                FIRST MODULE : DECLARATIONS OF TYPES              #
!*####################################################################
      MODULE Mod1
      IMPLICIT NONE

      INTEGER, PARAMETER :: k1 = KIND(0.0), len1 = 20
      REAL, PARAMETER ::  seed1=10.0, seed2=5.0

      TYPE Shape (k1,len1)
        INTEGER, KIND :: k1  !4
        INTEGER, LEN :: len1 !20

        REAL(KIND=k1) :: area
        CHARACTER(LEN=len1) :: name = ''
      END TYPE Shape

      TYPE, EXTENDS(Shape) :: Square
        REAL(KIND=k1) :: width
      END TYPE Square

      TYPE, EXTENDS(Square) :: Rectangle
        REAL(KIND=k1) :: height
      END TYPE Rectangle

      TYPE, EXTENDS(Shape) :: Triangle
        REAL(KIND=k1) :: a
        REAL(KIND=k1) :: b
        REAL(KIND=k1) :: c
        REAL(KIND=k1) :: perimeter
      END TYPE Triangle


      END MODULE Mod1

!*####################################################################
!*#                SECOND MODULE : DECLARATIONS OF SUBPROGRAMS       #
!*####################################################################
      MODULE Mod2
      USE Mod1, Sq => Square, Rec => Rectangle, Tri => Triangle
      IMPLICIT NONE

      CONTAINS 
!*
! area computation depending on the shape 
!*
      SUBROUTINE compute_square_area

      CLASS(Sq(k1,len1)), ALLOCATABLE :: My_shape 
      TYPE(Sq(k1,len1)), ALLOCATABLE :: A_square

      ALLOCATE(A_square)
      
      A_square%width = 10.0
 
      ALLOCATE(My_shape, source=A_square)
 
      IF ( .NOT. ALLOCATED(My_shape)) STOP 10

      SELECT TYPE (My_shape)
        TYPE IS (Sq(k1,*))
          My_shape%area = My_shape%width**2
          My_shape%name = 'a square'

          print *, 'My shape is ', TRIM(My_shape%name), ' and the area is', FLOOR(My_shape%area)

        TYPE IS (Rec(k1,*))
           print *, 'You should not pick this' 
           STOP 11

        CLASSDEFAULT
           print *, 'Area cannot be computed: undefined Shape' 
           STOP 12
      END SELECT
!*
      DEALLOCATE(My_shape)

      ALLOCATE (My_shape , SOURCE = Rec(k1,len1)(area=0.0,name='name',width=12.5,height=8.0))
      IF ( .NOT. ALLOCATED(My_shape)) STOP 101

      SELECT TYPE (My_shape)
        TYPE IS (Sq(k1,*))
           print *, 'You should not pick this' 
           STOP 13

        TYPE IS (Rec(k1,*))
          My_shape%area = My_shape%width*My_shape%height
          My_shape%name = 'a rectangle'

          print *, 'My shape is ', TRIM(My_shape%name), ' and the area is', FLOOR(My_shape%area)

        CLASS IS (Sq(k1,*))
           print *, 'You should not pick this' 
           STOP 14

        CLASS IS (Rec(k1,*))
           print *, 'You should not pick this' 
           STOP 15

        CLASSDEFAULT
           print *, 'Area cannot be computed: undefined Shape'
           STOP 16
      END SELECT
!*

      END SUBROUTINE compute_square_area

      SUBROUTINE compute_triangle_area

      REAL :: p
      CLASS(Tri(k1,len1)), POINTER :: My_shape 
      TYPE(Tri(k1,len1)), TARGET :: A_triangle

      A_triangle%a = 20.0
      A_triangle%b = 10.0
      A_triangle%c = 22.36
!*
      A_triangle%area = -0.0
 
      My_shape => A_triangle       
      IF ( .NOT. ASSOCIATED(My_shape)) STOP 20

      SELECT TYPE (My_shape)

        TYPE IS (Tri(k1,*))
                My_shape%perimeter = (My_shape%a+My_shape%b+My_shape%c)/2.0
           ASSOCIATE ( p => My_shape%perimeter )
                My_shape%area = SQRT( p*(p-My_shape%a)*(p-My_shape%b)*(p-My_shape%c) )
           END ASSOCIATE

           My_shape%name = 'a triangle'
           
           print *, 'My shape is ', TRIM(My_shape%name), ' and the area is', FLOOR(My_shape%area)

        CLASS IS (Tri(k1,*)) 
           print *, 'You should not pick this' 
           STOP 21

        CLASSDEFAULT
           print *, 'Area cannot be computed: undefined Shape' 
           STOP 22
      END SELECT
!*
      END SUBROUTINE compute_triangle_area

!*
! generate pick a shape
!*
      SUBROUTINE generate_shape

        call compute_square_area 

        call compute_triangle_area 

      END SUBROUTINE generate_shape
!*
      END MODULE Mod2
!*
!* Main program
!*
      PROGRAM Select_Type_Basic08
      USE Mod2, ONLY: generate_shape
      IMPLICIT NONE

      CALL generate_shape

      END PROGRAM Select_Type_Basic08
