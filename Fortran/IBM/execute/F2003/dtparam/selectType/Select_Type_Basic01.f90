!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Select_Type_Basic01 - SELECT TYPE 
!*                               DTP-SELECT TYPE Construct
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : July  10, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : ASSOCIATE Construct inside a SELECT TYPE Construct
!*                               Unlimited polymorphic
!*                               Host association
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
PROGRAM Select_Type_Basic01
      IMPLICIT NONE 

      TYPE Shape (k1,l1)
        INTEGER, KIND :: k1 = KIND(0.0)
        INTEGER, LEN  :: l1 = 20

        REAL(KIND=k1) :: area
        CHARACTER(LEN=l1) :: name = ''
      END TYPE Shape

      TYPE, EXTENDS(Shape) :: Square(k2)
        INTEGER, KIND :: k2 = KIND(0.0)

        REAL(k2) :: width
      END TYPE Square

      TYPE, EXTENDS(Square) :: Rectangle(k3)
        INTEGER, KIND :: k3 = KIND(0.0)

        REAL(k3) :: height
      END TYPE Rectangle

      TYPE, EXTENDS(Shape) :: Triangle(k3)
        INTEGER, KIND :: k3 = KIND(0.0)

        REAL(k3) :: a  
        REAL(k3) :: b  
        REAL(k3) :: c  
        REAL(k3) :: perimeter 
      END TYPE Triangle

      INTEGER, PARAMETER :: knd1 = KIND(0.0), knd2 = KIND(0.0), knd3 = KIND(0.0), len1 = 20
      CLASS(*), POINTER :: My_shape

      CALL generate_shape

      CONTAINS 

      SUBROUTINE compute_area1
         CLASS(*), ALLOCATABLE :: My_shape  ! different from My_shape of the main program
         TYPE(Square(knd1,len1,knd2)), ALLOCATABLE :: A_square


         ALLOCATE(A_square)
      
         A_square%width = 10.0
         A_square%area = -0.0 ! Initialization
 
         ALLOCATE(My_shape, source=A_square)
         IF ( .NOT. ALLOCATED(My_shape)) STOP 10

         SELECT TYPE (My_shape)
           TYPE IS (Square(knd1,*,knd1))
             My_shape%area = My_shape%width**2
             My_shape%name = 'a square'

             print *, 'My shape is ', TRIM(My_shape%name), ' and the area is', FLOOR(My_shape%area)

           TYPE IS (Rectangle(knd1,*,knd1,knd1))
              print *, 'You should not pick this' 
              STOP 21

           CLASS IS (Square(knd1,*,knd1))
              print *, 'You should not pick this' 
              STOP 22

           CLASSDEFAULT
              print *, 'Area cannot be computed: undefined Shape' 
              STOP 23
         END SELECT
!*
         SELECT TYPE (My_shape)
           TYPE IS (Rectangle(knd1,*,knd1,knd1))
             print *, 'You should not pick this' 
             STOP 31

           CLASS IS (Square(knd1,*,knd1))
              print *, 'You should not pick this' 
              STOP 32

           TYPE IS (Square(knd1,*,knd1))
             My_shape%area = My_shape%width**2
             My_shape%name = 'a square'

             print *, 'My shape is ', TRIM(My_shape%name), ' and the area is', FLOOR(My_shape%area)

           CLASSDEFAULT
              print *, 'Area cannot be computed: undefined Shape' 
              STOP 33
         END SELECT
!*
         DEALLOCATE(My_shape)

         ALLOCATE (My_shape , SOURCE = Rectangle(knd1,len1,knd1,knd1)(area=0.0,name='name',width=12.5,height=8.0))
         IF ( .NOT. ALLOCATED(My_shape)) STOP 12

         SELECT TYPE (A => My_shape)
           TYPE IS (Square(knd1,*,knd1))
             STOP 41

           TYPE IS (Rectangle(knd1,*,knd1,knd1))
             A%area = A%width*A%height
             A%name = 'a rectangle'

             print *, 'My shape is ', TRIM(A%name), ' and the area is', FLOOR(A%area)

           CLASS IS (Square(knd1,*,knd1))
              STOP 42

           CLASS IS (Rectangle(knd1,*,knd1,knd1))
              STOP 43

           CLASSDEFAULT
              STOP 44
         END SELECT

         SELECT TYPE ( A => My_shape )
           CLASS IS (Square(knd1,*,knd1))
              STOP 51

           CLASS IS (Rectangle(knd1,*,knd1,knd1))
              print *, 'I picked a shape and it may be a rectangle'

           CLASSDEFAULT
              STOP 52 
         END SELECT

         SELECT TYPE ( A => My_shape )
           CLASS IS (Square(knd1,*,knd1))
              print *, 'I picked a shape and it may be square'

           CLASS IS (SHape(knd1,*))
              STOP 61

           CLASS IS (Triangle(knd1,*,knd1)) 
              STOP 62

           CLASSDEFAULT
              STOP 63
         END SELECT

        DEALLOCATE( My_shape )

      END SUBROUTINE compute_area1

      SUBROUTINE compute_area2
         ! knd1,knd2,len1 and My_shape known by Host Association
         TYPE(Triangle(knd1,len1,knd1)), TARGET :: A_triangle

         A_triangle%a = 20.0
         A_triangle%b = 10.0
         A_triangle%c = 22.36

         A_triangle%area = -0.0
 
         My_shape => A_triangle       
 
         IF ( .NOT. ASSOCIATED(My_shape)) STOP 13

         SELECT TYPE (My_shape)
           CLASS IS (Triangle(knd1,*,knd1)) 
              STOP 71

           TYPE IS (Triangle(knd1,*,knd1))
              My_shape%perimeter = (My_shape%a+My_shape%b+My_shape%c)/2.0
              ASSOCIATE ( p => My_shape%perimeter )
                   My_shape%area = SQRT( p*(p-My_shape%a)*(p-My_shape%b)*(p-My_shape%c) )
              END ASSOCIATE
              My_shape%name = 'a triangle'
              print *, 'My shape is ', TRIM(My_shape%name), ' and the area is', FLOOR(My_shape%area)

           CLASS DEFAULT
              STOP 72
         END SELECT

        NULLIFY( My_shape )

      END SUBROUTINE compute_area2

      SUBROUTINE generate_shape
        call compute_area1
        call compute_area2
      END SUBROUTINE generate_shape

END PROGRAM Select_Type_Basic01
