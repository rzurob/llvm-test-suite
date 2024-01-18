!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                               Syntax Check: C460 Each binding-name in binding-name-list
!*                                                  shall be the name of a specific binding of the type.
!*
!*                                             - binding is inherited from base type, ensure type base cannot use generic TB
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

   type base
      character(3) :: c
      contains
         procedure, pass :: write => writechild
   end type

   type, extends(base) :: child
      integer(4) :: i
      contains
         generic :: write(formatted) => write
   end type

   contains

      subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowritechild'

      end subroutine

end module

program C460_003a
   use m

   integer :: i = 0
   character(150) :: c = ''

   write(6, *, iostat = i, iomsg = c )  base ('IBM')
   if ( ( i /= 0 ) .or. ( c /= "" ) ) error stop 1_4

end program
