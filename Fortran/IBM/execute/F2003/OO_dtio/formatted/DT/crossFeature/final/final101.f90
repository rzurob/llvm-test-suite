!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Final Subroutine: Ensure DTIO can be invoked correctly during finalization (read)
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

   integer :: stat
   character(150) :: msg

   contains

      subroutine finalbase(dtv)
         type(base), intent(inout) :: dtv
         read ( 1, "(DT'_finalizingbase'(6))", iostat = stat, iomsg = msg )      dtv
         print *, 'inside finalbase'
         print *, dtv%i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
      end subroutine

      subroutine finalchild(dtv)
         type(child), intent(inout) :: dtv
         read ( 1, "(DT'_finalizingchild'(6,8))", iostat = stat, iomsg = msg )    dtv
         print *, 'inside finalchild'
         print *, dtv%i, dtv%c
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
      end subroutine

end module

program final101
use m

   type(base)  :: b1
   type(child) :: c1
   type(base), allocatable :: b2
   class(base), pointer    :: b3

   open (1, file = 'final101.1', form='formatted', access='sequential' )

   b1 = base()         !<- finalize b1, then base(1234)
   c1 = child()  !<- finalize c1, then child(5678,'abc')

   allocate ( b2, source = base() )        !<- finalize base(2345)
   allocate ( b3, source = child() ) !<- finalize child(3456,'def')

   deallocate( b2, b3 )  !<- finalize b2, b3

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
         write ( fmt, "(A2,I1,A1)" ) '(I', v_list(1),')'
         read ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child )
         write ( fmt, "(A2,I1,A2,I1,A1)" ) '(I', v_list(1),',A',v_list(2),')'
         read ( unit, fmt, iostat = iostat )    dtv%i, dtv%c
   end select
   iomsg = 'dtioread'

end subroutine
