!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             A private type-bound procedure is accessible
!*                                             only within the module containing the type definition.
!*
!*                                             child type extends private generic binding defined in base type that is in
!*                                             another module
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
      integer :: i
      contains
         private
         procedure, pass, public :: gtint
         generic :: gt => gtint
   end type

   contains

      logical function gtint(a, b)
         class(base), intent(in) :: a
         integer, intent(in) :: b

         gtint = ( a%i .gt. b )
      end function

      logical function compare ( a, b )
         class(base), intent(in) :: a
         integer, intent(in) :: b

         compare = a%gt(b)

      end function

end module

module n
   use m

   type,extends(base) :: child
      integer :: j
      contains
         procedure, pass, public :: gtreal
         generic, private :: gt => gtreal
   end type

   contains

      logical function gtreal(a, b)
         class(child), intent(in) :: a
         real, intent(in) :: b

         gtreal = ( a%i .gt. int(b,kind(a%i)) ) .and. ( a%j .gt. int(b,kind(a%j)) )

      end function

      logical function comparereal ( a, b )
         class(child), intent(in) :: a
         real, intent(in) :: b

         comparereal = a%gt(b)

      end function

end module

program genericGenericNameScalar015
   use n

   class(base), allocatable :: b1
   class(child), pointer :: c1

   allocate ( b1, source = base (100) )
   allocate ( c1, source = child( 4000, 5000 ) )

   if ( compare(b1,1000) ) error stop 1_4
   if ( .not. compare(b1,99) ) error stop 2_4

   if ( compare(c1,10000) )      error stop 3_4
   if ( .not. compare(c1,3999) ) error stop 4_4

   if ( comparereal(c1,10000.0) )      error stop 5_4
   if ( .not. comparereal(c1,3999.9) ) error stop 6_4

   deallocate ( b1 )
   allocate ( b1, source = child( 10,20 ) )

   if ( compare(b1,10000) )      error stop 7_4
   if ( .not. compare(b1,9) ) error stop 8_4

   select type ( b1 )
      class is ( child )
         if ( comparereal(b1,10000.0) )   error stop 9_4
         if ( .not. comparereal(b1,9.9) ) error stop 10_4
      class default
         error stop 11_4
   end select

end program