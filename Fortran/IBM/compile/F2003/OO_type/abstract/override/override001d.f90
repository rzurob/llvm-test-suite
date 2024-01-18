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
!*                               i) Either both shall have a passed-object or neither shall
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
         procedure(inf), deferred, nopass :: getid
   end type

   type, extends(base) :: child
      real :: rid
      contains
         procedure, pass :: getid
   end type

   interface
      integer function inf(dtv)
         import base
         class(base), intent(in) :: dtv
      end function
   end interface

   contains

      integer function getid(dtv)
         class(child), intent(in) :: dtv
         getid=dtv%rid
      end function

end module

program override001d
end program
