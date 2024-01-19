!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Type-bound procedure overriding
!*                               vi)dummy arguments that correspond by position shall have the same names and characteristics,
!*                                  except for the type of the passed-object dummy argument
!*                                     - non-passed-object has different name/type
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
         procedure(inf), deferred, pass(dtv) :: setid
   end type

   type, extends(base) :: child
      real :: rid
      contains
         procedure, pass(dtv):: setid  !<- compiler should complain about this binding
   end type

   type, extends(base) :: child1
      real :: zid
      contains
         procedure, pass(dtv):: setid => setid2  !<- compiler should complain about this binding
   end type

   interface
      subroutine inf(dtv,j)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: j
      end subroutine
   end interface

   contains

      subroutine setid(dtv,i)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: i
      end subroutine

      subroutine setid2(dtv,j)
         class(child1), intent(inout) :: dtv
         real, intent(in) :: j
      end subroutine

end module

program override006d
end program
