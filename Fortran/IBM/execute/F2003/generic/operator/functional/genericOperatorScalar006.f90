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
!*  DESCRIPTION                : Operator: Scalar with class hierarchy
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
      integer :: x = -999
      integer :: y = -999
      contains
         generic :: operator(-) => sub
         procedure :: sub
   end type

   type, extends(base) :: child
      integer :: z = -999
      contains
         procedure :: sub => subchild
         generic :: operator(-) => sub
   end type

   type, extends(child) :: gen3
      integer :: w
      contains
         procedure :: sub => subgen3
         generic :: operator(-) => sub
   end type

   contains

   class(base) function sub ( a, b )
      class(base), intent(in) :: a, b
      allocatable :: sub
      allocate ( base :: sub )
      sub%x = a%x -b%x
      sub%y = a%y -b%y

   end function

    class(base) function subchild ( a, b )
      class(base), intent(in)  ::  b
      class(child), intent(in) ::  a
      allocatable :: subchild

      allocate ( child :: subchild )
      select type ( subchild )
         class is ( child )
            select type ( b )
               class is ( child )
                  subchild%base = a%base - b%base
                  subchild%z = a%z - b%z
            end select
      end select

   end function

    class(base) function subgen3 ( a, b )
      class(base), intent(in) ::  b
      class(gen3), intent(in) ::  a
      allocatable :: subgen3

      select type ( b )
         class is ( gen3 )
            allocate ( subgen3, source = gen3 ( base = a%base - b%base, z = a%z - b%z , w = a%w - b%w ) )
      end select
   end function

end module

program genericOperatorScalar006
  use m

   class(base), pointer :: b1, b2, b3

   allocate ( b1, source = base( 20, 30 ) )
   allocate ( b2, source = base( 30, 50 ) )

   allocate ( b3, source = b2 - b1 )
   print *, b3%x, b3%y

   nullify ( b3, b2, b1 )
   allocate ( b1, source = child(100, 200, 300) )
   allocate ( b2, source = child(50, 100, 150 ) )
   allocate ( b3, source = b1 - b2 )

   select type ( b3 )
      type is ( child )
         print *, b3%x, b3%y, b3%z
   end select

   nullify ( b3, b2, b1 )
   allocate ( b1, source = gen3(10,20,30,40) )
   allocate ( b2, source = gen3(5,10,15,20) )
   allocate ( b3, source = b1 - b2 )

   select type ( b3 )
      type is ( gen3 )
         print *, b3%x, b3%y, b3%z, b3%w
   end select

end program
