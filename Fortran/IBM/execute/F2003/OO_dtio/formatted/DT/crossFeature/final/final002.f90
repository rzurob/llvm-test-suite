!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Final Subroutine: Ensure DTIO can be invoked correctly during finalization with array
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
         final :: finalbase, finalbasearray
   end type

   type, extends(base) :: child
      character(3) :: c = 'xxx'
      contains
         final :: finalchildarray
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

      subroutine finalbasearray(dtv)
         type(base), intent(inout) :: dtv(:)
         character(100) :: fmt

         write ( fmt, * ) "(", size(dtv),"(DT'finalizebasearray'(5)))"
         write ( 1, "(A)", advance='no', iostat = stat, iomsg = msg )      'inside finalbasearray =>'
         write ( 1, fmt, iostat = stat, iomsg = msg )                      dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )                  error stop 2_4
      end subroutine

      subroutine finalchildarray(dtv)
         type(child), intent(inout) :: dtv(:)
         write ( 1, "(A)", advance='no', iostat = stat, iomsg = msg )      'inside finalchildarray =>'
         write ( 1, "(DT'_finalizingchild'(6,8))", iostat = stat, iomsg = msg )    dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4
      end subroutine

end module

program final002
use m

   type(base)  :: b1(3)
   type(child) :: c1(3)
   type(base), allocatable :: b2(:)
   class(base), pointer    :: b3(:)

   open (1, file = 'final002.1', form='formatted', access='sequential' )

   write ( 1, "('b1:')")
   b1 = (/ base(1234), base(1235), base(1236) /)                      !<- finalize b1, then base(1234), base(1235), base(1236) as scalars
   write ( 1, "('c1:')")
   c1 = (/ child(2234,'abc'), child(2235,'def'), child(2236,'ghi') /) !<- finalize c1, then child(2234,'abc'), child(2235,'def'), child(2236,'ghi') as scalars
   write ( 1, "('b2:')")
   allocate( b2(4), source = (/ base(1), base(2), base(3), base(4) /) )
   write ( 1, "('b3:')")
   allocate( b3(4), source = (/ child(5,'ABC'), child(6,'DEF'), child(7,'GHI'), child(8,'JKL') /) )

   write ( 1, "('deallocate:')")
   deallocate( b2, b3 )

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
