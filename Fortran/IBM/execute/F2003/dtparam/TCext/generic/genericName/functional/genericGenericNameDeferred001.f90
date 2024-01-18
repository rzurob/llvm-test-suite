! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/genericName/functional/genericGenericNameDeferred001.f
! opt variations: -ql -qreuse=none

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

   type, abstract :: base(k1)    ! (4)
      integer, kind            :: k1
      integer(k1), allocatable :: i
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
         class(base(4)), intent(inout) :: a
         integer, intent(in) :: b
      end subroutine
   end interface

   abstract interface
      subroutine deferredinterface2B(a, b)
         import base
         class(base(4)), intent(inout) :: a
         class(base(4)), intent(in) :: b
      end subroutine
   end interface

end module

module n
   use m, only: A=> base

   type, extends(A) :: child    ! (4)
      integer(k1) :: j
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
         class(child(4)), intent(inout) :: A
         integer, intent(in) :: b

         print *,'twoargpassA'

         a%i = b
         a%j = b

      end subroutine

      subroutine twoargpassB(a, b)
         class(A(4)), intent(inout) :: A
         class(child(4)), intent(in) :: b

         a%i = b%i
         print *,'twoargpassB'
         select type ( A )
            type is ( child(4) )
               a%j = b%j
         end select

      end subroutine

end module

program genericGenericNameDeferred001
   use m, only:base
   use n, only:child

   class(base(4)), allocatable :: b1
   class(base(4)), pointer :: b2
   type(child(4)) :: c1
   class(child(4)), pointer :: c2

   allocate ( b1, source = child(4)(100,200) )
   allocate ( b2, source = child(4)(300,400) )

   c1 = child(4)(500,600)
   allocate ( c2, source = child(4)(700,800) )

   call b1%copy()
   call b2%copy()
   call c1%copy()
   call c2%copy()

   call b1%copy(b2)

   select type ( g => b1 )
      type is ( child(4) )
         print *, g%i, g%j
   end select

   select type ( g => b2 )
      type is ( child(4) )
         print *, g%i, g%j
   end select

   call c1%copy(b1)

   print *, c1%i, c1%j
   select type ( g => b1 )
      type is ( child(4) )
         print *, g%i, g%j
   end select

   call b2%copy(c2)

   print *, c2%i, c2%j
   select type ( g => b2 )
      type is ( child(4) )
         print *, g%i, g%j
   end select

   call b1%copy(1000)
   call b2%copy(2000)
   call c1%copy(3000)
   call c2%copy(4000)

   select type ( g => b1 )
      type is ( child(4) )
         print *, g%i, g%j
   end select

   select type ( g => b2 )
      type is ( child(4) )
         print *, g%i, g%j
   end select

   print *, c1%i, c1%j
   print *, c2%i, c2%j

end program
