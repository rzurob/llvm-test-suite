!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Final Subroutine: Ensure DTIO can be invoked correctly during finalization
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
      integer(4) :: i = 0
      contains
         final :: finalbase
   end type

   type, extends(base) :: child
      character(3) :: c = 'xxx'
      contains
         final :: finalchild
   end type


   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(150) :: msg

   contains

      subroutine finalbase(dtv)
         type(base), intent(inout) :: dtv
         write ( 1, "(A)", advance='no', iostat = stat, iomsg = msg )      'inside finalbase =>'
         write ( 1, "(DT'_finalizingbase'(6))", iostat = stat, iomsg = msg )      dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
      end subroutine

      subroutine finalchild(dtv)
         type(child), intent(inout) :: dtv
         write ( 1, "(A)", advance='no', iostat = stat, iomsg = msg )      'inside finalchild =>'
         write ( 1, "(DT'_finalizingchild'(6,8))", iostat = stat, iomsg = msg )    dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
      end subroutine

end module

program final001
use m

   type(base)  :: b1
   type(child) :: c1
   type(base), allocatable :: b2
   class(base), pointer    :: b3

   open (1, file = 'final001.1', form='formatted', access='sequential' )

   b1 = base(1234)         !<- finalize b1, then base(1234)
   c1 = child(5678,'abc')  !<- finalize c1, then child(5678,'abc')

   write ( 1, "('allocatables and pointers')")
   allocate ( b2, source = base(2345) )        !<- finalize base(2345)
   allocate ( b3, source = child(3456,'def') ) !<- finalize child(3456,'def')

   deallocate( b2, b3 )  !<- finalize b2, b3

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list

   select type ( dtv )
      type is ( base )
         write ( fmt, "(A2,I1,A1)" ) '(I', v_list(1),')'
         write ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child )
         write ( fmt, "(A2,I1,A2,I1,A1)" ) '(I', v_list(1),',A',v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%c
   end select
   iomsg = 'dtiowrite'

end subroutine
