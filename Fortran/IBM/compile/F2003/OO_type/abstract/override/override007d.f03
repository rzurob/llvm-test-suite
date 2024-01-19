!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Type-bound procedure overriding
!*                               vii)either shall be subroutines or both shall be functions with the same rsult characteristics
!*                                     - one is function, the other is subroutine
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module n

   type, abstract :: base
      integer :: id
      contains
         procedure(inf), deferred, pass :: setid
   end type

   type, extends(base) :: child
      real :: rid
      contains
         procedure, pass:: setid
   end type

   interface
      subroutine inf(dtv,j)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: j
      end subroutine
   end interface

   contains

      integer function setid(dtv,j)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: j
         setid = j
      end function

end module

program override007d
end program
