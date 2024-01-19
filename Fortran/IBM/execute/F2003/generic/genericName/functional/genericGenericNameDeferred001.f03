!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: generic tb with deferred binding
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

   type, abstract :: base
      integer, allocatable :: i
      contains
         procedure(deferredinterfacenp), deferred, nopass   :: nopass
         procedure(deferredinterface2A), deferred, pass     :: twoargpassA
         procedure(deferredinterface2B), deferred, pass(b)  :: twoargpassB

         generic :: copy => nopass, twoargpassA, twoargpassB

   end type

   abstract interface
      subroutine deferredinterfacenp()
      end subroutine
   end interface

   abstract interface
      subroutine deferredinterface2A(a, b)
         import base
         class(base), intent(inout) :: a
         integer, intent(in) :: b
      end subroutine
   end interface

   abstract interface
      subroutine deferredinterface2B(a, b)
         import base
         class(base), intent(inout) :: a
         class(base), intent(in) :: b
      end subroutine
   end interface

end module

module n
   use m, only: A=> base

   type, extends(A) :: child
      integer :: j
      contains
         procedure, nopass   :: nopass
         procedure, pass     :: twoargpassA
         procedure, pass(b)  :: twoargpassB
   end type

   contains

      subroutine nopass()

         print *, 'wrong number of arguments specified'

      end subroutine

      subroutine twoargpassA(a, b)
         class(child), intent(inout) :: a
         integer, intent(in) :: b

         print *,'twoargpassA'

         a%i = b
         a%j = b

      end subroutine

      subroutine twoargpassB(a, b)
         class(A), intent(inout) :: a
         class(child), intent(in) :: b

         a%i = b%i
         print *,'twoargpassB'
         select type ( a )
            type is ( child )
               a%j = b%j
         end select

      end subroutine

end module

program genericGenericNameDeferred001
   use m, only:base
   use n, only:child

   class(base), allocatable :: b1
   class(base), pointer :: b2
   type(child) :: c1
   class(child), pointer :: c2

   allocate ( b1, source = child(100,200) )
   allocate ( b2, source = child(300,400) )

   c1 = child(500,600)
   allocate ( c2, source = child(700,800) )

   call b1%copy()
   call b2%copy()
   call c1%copy()
   call c2%copy()

   call b1%copy(b2)

   select type ( g => b1 )
      type is ( child )
         print *, g%i, g%j
   end select

   select type ( g => b2 )
      type is ( child )
         print *, g%i, g%j
   end select

   call c1%copy(b1)

   print *, c1%i, c1%j
   select type ( g => b1 )
      type is ( child )
         print *, g%i, g%j
   end select

   call b2%copy(c2)

   print *, c2%i, c2%j
   select type ( g => b2 )
      type is ( child )
         print *, g%i, g%j
   end select

   call b1%copy(1000)
   call b2%copy(2000)
   call c1%copy(3000)
   call c2%copy(4000)

   select type ( g => b1 )
      type is ( child )
         print *, g%i, g%j
   end select

   select type ( g => b2 )
      type is ( child )
         print *, g%i, g%j
   end select

   print *, c1%i, c1%j
   print *, c2%i, c2%j

end program
