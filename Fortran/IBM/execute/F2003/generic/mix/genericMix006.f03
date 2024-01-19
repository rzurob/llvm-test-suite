!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : Mix generic type bounds
!*
!*  DESCRIPTION                : use generic interface as structure constructor and define assignment and operator
!*
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
      integer :: i, j

      contains
         procedure, pass    :: add
         procedure, pass(b) :: equal
         procedure, pass    :: assign

         generic :: operator(+)    => add
         generic :: operator(.eq.) => equal
         generic :: assignment(=)  => assign

   end type

   type, extends(base) :: child
      integer :: k

      contains

         procedure, pass    :: addc
         procedure, pass(b) :: equal => equalc

         generic :: operator(+)    => addc
         generic :: operator(.eq.) => equal

   end type

   ! mimic UD structure constructor

   interface base
      type(base) function noargbase()
         import base
      end function
      type(base) function oneargbase(i)
         import base
         integer, intent(in) :: i
      end function
      type(base) function twoargbase(i,j)
         import base
         integer, intent(in) :: i,j
      end function
   end interface

   interface child
      type(child) function noargchild()
         import child
      end function
      type(child) function oneargchild(i)
         import child
         integer, intent(in) :: i
      end function
      type(child) function twoargchild(i,j)
         import child
         integer, intent(in) :: i,j
      end function
      type(child) function threeargchild(i,j,k)
         import child
         integer, intent(in) :: i,j,k
      end function
   end interface

   contains

      class(base) function add ( a, b )
         class(base), intent(in) :: a
         type (base), intent(in) :: b

         allocatable :: add

         allocate ( base :: add )

         add%i = a%i + b%i
         add%j = a%j + b%j

      end function

      class(child) function addc ( a, b )
         class(child), intent(in) :: a
         type (child), intent(in) :: b

         allocatable :: addc

         allocate ( child :: addc )

         addc%base = a%base + b%base
         addc%k = a%k + b%k

      end function

      logical function equal ( a, b )
         class(base), intent(in)  :: b
         class(base), intent(in)  :: a

         equal = ( a%i == b%i ) .and. ( a%j .eq. b%j )
      end function

      logical function equalc ( a, b )
         class(child), intent(in)  :: b
         class(base), intent(in)  :: a

         select type ( a )
            type is ( child )
               equalc = ( a%base == b%base ) .and. ( a%k .eq. b%k )
            type is ( base )
               equalc = .false.
         end select

      end function

      recursive subroutine assign ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in)  :: b

         select type ( a )
            type is ( base )
               a%i = b%i
               a%j = b%j
            type is ( child )
               select type ( b )
                  type is ( base )
                     a%base = b        !<- should be a recursive call
                     a%k = 0
                  type is ( child )
                     a%base = b%base   !<- should be a recursive call
                     a%k = b%k
               end select
         end select

      end subroutine

end module


type(base) function noargbase()
   use m, only: base

   noargbase=base(-999,-999)

end function

type(base) function oneargbase(i)
   use m, only: base
   integer, intent(in) :: i

   oneargbase=base(i,-999)

end function

type(base) function twoargbase(i,j)
   use m, only: base
   integer, intent(in) :: i,j

   twoargbase%i=i
   twoargbase%j=j

end function

type(child) function noargchild()
   use m, only: child

   noargchild=child(-999,-999,-999)

end function

type(child) function oneargchild(i)
   use m, only: child
   integer, intent(in) :: i

   oneargchild=child(i,-999,-999)

end function

type(child) function twoargchild(i,j)
   use m, only: child
   integer, intent(in) :: i,j

   twoargchild=child(i,j,-999)

end function

type(child) function threeargchild(i,j,k)
   use m, only: child
   integer, intent(in) :: i,j,k

   threeargchild%i=i
   threeargchild%j=j
   threeargchild%k=k

end function

program genericMix006
   use m

   class(base), allocatable :: b1
   type(base) :: b2

   class(child), pointer :: c1
   type(child) :: c2

   allocate ( b1, c1 )

   b1 = base()
   b2 = b1 + base(1000,1000)

   if ( .not. ( b1 .eq. base(-999,-999) ) ) error stop 1_4
   if ( .not. ( b2 .eq. base(1,1) ) )       error stop 2_4

   c1 = child(1,2,3)
   if ( .not. ( c1 == child(1,2,3) ) )      error stop 3_4

   c1 = child(4,5)
   if ( .not. ( c1 == child(4,5) ) )        error stop 4_4

   c2 = c1 + child(-3,-3,1002)
   if ( .not. ( c2 == child(1,2,3) ) )      error stop 5_4

   c2 = child(6)
   if ( .not. ( c2 == child(6,-999,-999) ) ) error stop 6_4

   c2 = child() + child() + child(1000) + child(1000,1000) + child(i=0,j=2000,k=4000)
   if ( .not. ( c2 == child(2,3,4) ) )       error stop 7_4

   deallocate ( b1 )

   allocate ( b1, source = child() )

   select type ( b1 )
      type is ( child )
         if ( .not. ( b1 == child(-999,-999,-999) ) )       error stop 8_4
         b1 = child(1,2,3)
         if ( .not. ( b1 .eq. base(1,2) ) )                 error stop 9_4
   end select

end program
