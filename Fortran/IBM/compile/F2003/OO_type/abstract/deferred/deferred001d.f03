!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Deferred Binding
!*                                  - define the same binding (between specific and deferred) within
!*                                    the same derived type
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

module n

   type, abstract :: base
      integer :: id = -999
      contains
         procedure(inf), deferred, pass :: setid
         procedure, pass                :: setid => setbaseid
   end type

   interface
      subroutine inf(dtv,j)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: j
      end subroutine
   end interface

   contains

      subroutine setbaseid(dtv,j)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: j
         dtv%id = j
      end subroutine

end module

program deferred001d
end program
