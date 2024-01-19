!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar to Scalar w/ unlimited polymorphic components
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module x

   type inner
      integer :: i
      contains
         procedure, pass :: isub
         generic :: operator(-) => isub
   end type

   interface operator (-)
      module procedure intsub
   end interface operator(-)

   contains

   type(inner) function isub ( a, b )
      class(inner), intent(in) :: a
      class(*), intent(in) :: b

      select type ( b )
         type is (integer )
            isub%i = a%i - b
         type is ( inner )
            isub%i = a%i - b%i
      end select

   end function

   integer function intsub ( a, b )
      integer, intent(in) :: a
      type(inner), intent(in) :: b

      intsub = a - b%i

   end function

end module


module m
   use x

   type base
      class(*), pointer :: u1
      class(*), allocatable :: u2
      contains
         procedure, pass :: bsub
         generic, public :: operator (-) => bsub
   end type

   contains

   type(base) function bsub ( a, b )
      class(base), intent(in) :: a, b

      select type ( g => a%u1 )
         type is (inner)
            allocate ( bsub%u1 , source = g - b%u1 )
         type is (integer)
            select type ( h => b%u1 )
               type is ( inner )
                  allocate ( bsub%u1 , source = g - h )
               type is ( integer )
                  allocate ( bsub%u1 , source = g - h )
            end select
      end select

      select type ( g => a%u2 )
         type is (inner)
            allocate ( bsub%u2 , source = g - b%u2 )
         type is (integer)
            select type ( h => b%u2 )
               type is ( inner )
                  allocate ( bsub%u2 , source = g - h )
               type is ( integer )
                  allocate ( bsub%u2 , source = g - h )
            end select
      end select

   end function

end module


program genericOperatorScalar014
   use m

   type (base) :: b1
   type(base), allocatable :: b2
   type(inner), target :: i1, i2

   integer, target :: i11, i12

   i1 = inner(10)
   b1 = base( i1 , 20_4 )
   i2 = inner(30)
   allocate ( b2, source = base (i2, inner (40_4) ) )

   b2 = b2 - b1

   select type ( g=>b2%u1 )
      type is ( inner )
         print *, g
   end select

   select type ( g=>b2%u2 )
      type is ( inner )
         print *, g
   end select

   b1 = b2 - b1

   select type ( g=>b1%u1 )
      type is ( inner )
         print *, g
   end select

   select type ( g=>b1%u2 )
      type is ( inner )
         print *, g
   end select

   i11 = 100
   i12 = 200

   b1 = base ( i11, 200 )
   deallocate ( b2 )
   allocate ( b2, source = base ( i12, 300 ) )

   b2 = b2 - b1

   select type ( g=>b2%u1 )
      type is ( integer )
         print *, g
   end select

   select type ( g=>b2%u2 )
      type is ( integer )
         print *, g
   end select

   b1 = base ( i2, i1 )
   deallocate ( b2 )
   allocate ( b2, source = base ( i1, i2 ) )

   b1 = b1 - b2
   select type ( g=>b1%u1 )
      type is ( inner )
         print *, g
   end select

   select type ( g=>b1%u2 )
      type is ( inner )
         print *, g
   end select

end
