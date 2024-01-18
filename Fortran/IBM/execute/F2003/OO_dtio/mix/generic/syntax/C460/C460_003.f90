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
!*                                             - binding is inherited from base type
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

         select type ( dtv )
            type is ( child )
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%i
         end select

         iomsg = 'dtiowritechild'

      end subroutine

end module

program C460_003
   use m
   type ( child ) :: c1 = child ( 'ibm', 2005 )

   print *, c1

end program
