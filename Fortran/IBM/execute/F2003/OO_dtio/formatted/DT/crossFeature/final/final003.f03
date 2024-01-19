!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Final Subroutine: Ensure final subroutine is called when necessary inside DTIO
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
      integer(4) :: i = -999
      contains
         final :: finalbase
   end type

   type, extends(base) :: child
      character(3) :: c = 'xxx'
      contains
         final :: finalchild
   end type

   integer :: stat
   character(150) :: msg
   character(50) :: if(6)

   integer(4) :: idx

   contains

      subroutine finalbase(dtv)
         type(base), intent(inout) :: dtv
         write ( if(idx), "(A,I6)", iostat = stat, iomsg = msg )      'inside finalbase =>', dtv%i
         idx = idx + 1
      end subroutine

      subroutine finalchild(dtv)
         type(child), intent(inout) :: dtv
         write ( if(idx), "(A,I6,A4)", iostat = stat, iomsg = msg )      'inside finalchild =>', dtv%i, dtv%c
         idx = idx + 1
      end subroutine

end module

program final003
use m

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   type(base)  :: b1 = base(111)
   type(child) :: c1 = child(222,'ddd')
   open (1, file = 'final003.1', form='formatted', access='sequential' )
   idx = 1

   print *, 'Start'

   read (1, '(DT)', iostat = stat, iomsg = msg) b1
   read (1, '(DT)', iostat = stat, iomsg = msg) c1

   print *, if

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   select type ( dtv )
      type is ( base )
         dtv = base(123)
      type is ( child )
         dtv = child(123,'abc')
   end select

   iomsg = 'dtioread'

end subroutine
