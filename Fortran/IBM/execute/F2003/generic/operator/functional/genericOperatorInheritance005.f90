!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Inheritance
!*                                         two different types extending base type,
!*                                         and generic operator only available in one of them
!*                                         Both types shall be able to use the generic operator if it matches the interface
!*                                         base type has multiple different specific type bound that includes into the generic
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base
      integer i
      contains
         procedure, pass :: BaCone
         procedure, pass :: BaCtwo
   end type

   type, extends(base) :: c1
      contains
         generic :: operator(+) => BaCtwo
   end type

   type, extends(base) :: c2
      contains
         generic :: operator(+) => BaCone
   end type

   contains

      class(base) function BaCone (a, b)
         class(base), intent(in) :: a
         type(c1), intent(in)  :: b
         allocatable :: BaCone

         allocate ( BaCone, source = a )
         BaCone%i = BaCone%i + b%i

         print *, 'BaCone'

      end function

      class(base) function BaCtwo (a, b)
         class(base), intent(in) :: a
         type(c2), intent(in)  :: b
         allocatable :: BaCtwo

         allocate ( BaCtwo, source = a )
         BaCtwo%i = BaCtwo%i + b%i

         print *, 'BaCtwo'

      end function

end module

program genericOperatorInheritance005
   use m

   class(base), pointer :: b1

   class(c1), allocatable :: c11, c12
   class(c2), allocatable :: c21, c22

   allocate ( c11, source = c1(10) )
   allocate ( c21, source = c2(20) )

   allocate ( c12, source = c1(30) )
   allocate ( c22, source = c2(40) )

   allocate ( b1, source = c11 + c21 )
   print *, b1%i
   allocate ( b1, source = c11 + c22 )
   print *, b1%i
   allocate ( b1, source = c21 + c12 )
   print *, b1%i
   allocate ( b1, source = c22 + c11 )
   print *, b1%i

end program
