!*  ===================================================================
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
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

   type base (kb)
      integer, kind :: kb
      integer(kb) :: i = -999
      contains
         final :: finalbase
   end type

   type, extends(base) :: child (lc)
      integer, len :: lc
      character(lc) :: c = 'xxx'
      contains
         final :: finalchild
   end type

   integer :: stat
   character(150) :: msg
   character(50) :: if(6)

   integer(4) :: idx

   contains

      subroutine finalbase(dtv)
         type(base(4)), intent(inout) :: dtv
         write ( if(idx), "(A,I6)", iostat = stat, iomsg = msg )      'inside finalbase =>', dtv%i
         idx = idx + 1
      end subroutine

      subroutine finalchild(dtv)
         type(child(4,*)), intent(inout) :: dtv
         write ( if(idx), "(A,I6,A4)", iostat = stat, iomsg = msg )      'inside finalchild =>', dtv%i, dtv%c
         idx = idx + 1
      end subroutine

end module

program final003kl
use m

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   type(base(4))  :: b1 = base(4)(111)
   type(child(4,3)) :: c1 = child(4,3)(222,'ddd')
   open (1, file = 'final003kl.1', form='formatted', access='sequential' )
   idx = 1

   print *, 'Start'

   read (1, '(DT)', iostat = stat, iomsg = msg) b1
   read (1, '(DT)', iostat = stat, iomsg = msg) c1

   print *, if

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   select type ( dtv )
      type is ( base(4) )
         dtv = base(4)(123)
      type is ( child(4,*) )
         dtv = child(4,3)(123,'abc')
   end select

   iomsg = 'dtioread'

end subroutine
