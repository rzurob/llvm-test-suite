!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Type-bound procedure overriding
!*                               v) passed-object dummy arguments, if any, shall correspond by name and position
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
         procedure(inf), deferred, pass(j) :: setid
   end type

   type, extends(base) :: child
      real :: rid
      contains
         procedure, pass(dtv):: setid
   end type

   interface
      subroutine inf(j,dtv)
         import base
         class(base), intent(inout) :: j
         integer, intent(in) :: dtv
      end subroutine
   end interface

   contains

      subroutine setid(j,dtv)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: j
      end subroutine

end module

program override005d1
end program