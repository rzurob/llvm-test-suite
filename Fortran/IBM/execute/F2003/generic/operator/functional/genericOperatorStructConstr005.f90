!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Operator: Scalar to Scalar with structure constructors within structure constructor (polymorphic components)
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

   type inner
      integer :: j = -999
      contains
         procedure :: iadd
         generic :: operator(+) => iadd
   end type

   type, extends(inner) :: innerc
      integer :: k = -999
      contains
         procedure :: iadd => icadd
   end type

   type base
      integer :: x = -999
      class(inner), allocatable :: i
      contains
         procedure :: badd
         generic :: operator (+) => badd
   end type

   interface
      type(base) function badd (a, b)
         import base
         class(base), intent(in) :: a,b
      end function
   end interface

   contains

      class(inner) function iadd ( a, b ) result(abc)
         class(inner), intent(in) :: a, b
         allocatable :: abc

         allocate ( abc, source = inner ( j = a%j + b%j ) )
      end function

      class(inner) function icadd ( a, b ) result(abc)
         class(innerc), intent(in) :: a
         class(inner), intent(in) ::  b
         allocatable :: abc

         select type ( b )
            type is ( innerc )
               allocate ( abc, source = innerc ( j = a%j + b%j, k = a%k + b%k ) )
         end select

      end function

end module

type(base) function badd (a, b)
   use m, only: base
   class(base), intent(in) :: a,b

   badd%x = a%x + b%x
   allocate ( badd%i, source = a%i + b%i )

end function

program genericOperatorStructConstr005
   use m

   class(base), allocatable :: b1
   class(base), pointer :: b2

   type(base) :: b3

   allocate ( b1, source = base ( x = 10, i = null() ) )
   allocate ( b2, source = base ( x = 30, i = innerc(j = 300, k = 3000) ) + base ( x = 40, i = innerc( j = 400, k = 4000) ) )

   allocate ( b1%i, source = innerc(j = 100, k = 1000) + innerc( j = 200, k = 2000) )

   select type ( g => b1%i )
      type is ( innerc )
         if ( ( b1%x /= 10 ) .or. ( g%j /= 300 ) .or. ( g%k /= 3000 ) ) error stop 1_4
      class default
         error stop 2_4
   end select

   select type ( g => b2%i )
      type is ( innerc )
         if ( ( b2%x /= 70 ) .or. ( g%j /= 700 ) .or. ( g%k /= 7000 ) ) error stop 3_4
      class default
         error stop 4_4
   end select

   b3 = base ( x = 50, i = innerc ( 500, 5000 ) ) + base ( x = 60, i = innerc ( 600, 6000 ) )

   select type ( g => b3%i )
      type is ( innerc )
         if ( ( b3%x /= 110 ) .or. ( g%j /= 1100 ) .or. ( g%k /= 11000 ) ) error stop 5_4
      class default
         error stop 6_4
   end select

end
