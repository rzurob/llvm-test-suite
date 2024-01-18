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
         procedure(inf), deferred, pass :: getid
   end type

   type, extends(base) :: child
      real :: rid
      contains
         procedure, pass :: getid
   end type

   interface
      pure subroutine inf(dtv)
         import base
         class(base), intent(inout) :: dtv
      end subroutine
   end interface

   contains

      subroutine getid(dtv)
         class(child), intent(inout) :: dtv
         dtv%id=10
      end subroutine

end module

program override002d
end program
