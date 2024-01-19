!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Array Constructor
!*                                    -  Array constructor with formatted i/o
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
      character(3) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         generic :: write(formatted) => write
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writec
   end type

   type, extends(child) :: gen3
      character(3) :: s = 'xxx'
      contains
         procedure, pass :: write => writeg
   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ")"
         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ",1X, I",v_list(2),")"

         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine writeg (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(30) :: fmt

         write (fmt, *) "(A", v_list(1), ",1X, I",v_list(2),",1X, A", v_list(3),")"

         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%s

         iomsg = 'dtiowriteg'

      end subroutine

end module

program arrayConstr001
   use m

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'arrayConstr001.1', form='formatted', access='sequential' )

   write ( 1, "(3(DT(3)))", iostat = stat, iomsg = msg )            (/ base(), base(), base() /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

   write ( 1, "(3(DT(3,4)))", iostat = stat, iomsg = msg )          (/ ( child(),i=1,3 ) /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4

   write ( 1, "(3(DT(3,4,3)))", iostat = stat, iomsg = msg )        (/ ( gen3(),i=4,6 ) /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) ) error stop 3_4

   write ( 1, "(4(DT(3)))", iostat = stat, iomsg = msg )          (/ base('abc'), base('def'), ( base('ghi'), i=1,2 )/)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 4_4

   write ( 1, "(3(DT(3,4)))", iostat = stat, iomsg = msg )        (/ ( child('IBM',2005),i=1,3 ) /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 5_4

   write ( 1, "(3(DT(4,4,4)))", iostat = stat, iomsg = msg )            (/ ( gen3('FTN',2003,'GRT'),i=4,6 ) /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) ) error stop 6_4

end program
