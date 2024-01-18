!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 05/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Type-bound procedure overriding
!*                               ii) If the overridden binding is pure then the overridding binding shall also be pure
!*                                   - overridding is pure, overridden is not pure
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
      integer :: id
      contains
         procedure(inf), deferred, pass :: setid
   end type

   type, extends(base) :: child
      real :: rid
      contains
         procedure, pass :: setid
   end type

   interface
      subroutine inf(dtv)
         import base
         class(base), intent(inout) :: dtv
      end subroutine
   end interface

   contains

      pure subroutine setid(dtv)
         class(child), intent(inout) :: dtv
         dtv%id=5
      end subroutine

end module

program override002
   use m

   class(base), allocatable :: b1
   procedure(logical) :: precision_r4

   allocate ( b1, source = child ( 10, 11.0 ) )
   
   call b1%setid()
   
   select type ( b1 )
      type is ( child )
         if ( ( b1%id /= 5 ) .or. ( .not. precision_r4(b1%rid, 11.0)) ) error stop 1_4
   end select

end program
