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
!*                                     - non-passed-object has different characteristics (intent, shape)
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
         procedure, pass(dtv):: setid => setid1
   end type

   type, extends(base) :: child1
      real :: zid
      contains
         procedure, pass(dtv):: setid => setid2
   end type

   type, extends(base) :: child2
      real :: yid
      contains
         procedure, pass(dtv):: setid => setid3
   end type

   type, extends(base) :: child3
      real :: yid
      contains
         procedure, pass(dtv):: setid => setid4
   end type

   interface
      subroutine inf(dtv,j)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: j
      end subroutine
   end interface

   contains

      subroutine setid1(dtv,j)
         class(child), intent(inout) :: dtv
         integer, intent(inout) :: j        !<= different intent
      end subroutine

      subroutine setid2(dtv,j)
         class(child1), intent(inout) :: dtv
         integer, intent(in) :: j(:)        !<= different shape
      end subroutine

      subroutine setid3(dtv,j)
         class(child2), intent(inout) :: dtv
         integer, intent(in), optional :: j  !<= is optional
      end subroutine

      subroutine setid4(dtv,j)
         class(child3), intent(inout) :: dtv
         integer, intent(in), pointer :: j   !<= is pointer
      end subroutine

end module

program override006d1
end program
