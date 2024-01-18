! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.7.2: Slash Editing
!*                                        Try slash editing with r inside DTIO
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

module m1

   type :: base
      integer(4)    :: c1
      real(4)       :: c2
      character(3)  :: c3
   end type

   interface write(formatted)
      subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(formatted)
      subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program slash002
   use m1

   procedure(logical) :: precision_r4
   ! declaration of variables

   class(base), allocatable :: f1
   type(base), pointer      :: f2(:)
   type(base) , allocatable :: f3
   class(base), pointer     :: f4(:)

   integer :: stat
   character(200) :: msg

   open ( unit = 1, file = 'slash002.1', access='stream', form='formatted' )

   ! allocation of variables

   allocate (f1, source = base(1,2.0,'abc'))
   allocate (f2(2), source = (/ base(2,3.0,'def'), base(4,5.0,'ghi') /) )
   allocate (f3)
   allocate (base :: f4(2))

   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4
   write (1, *, iostat=stat, iomsg=msg)                f2
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4

   rewind 1

   read (1, *, iostat=stat, iomsg=msg)                 f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )    error stop 3_4
   read (1, *, iostat=stat, iomsg=msg)                 f4
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )    error stop 4_4

   print *, f3
   print *, f4

   ! check if values are read correctly

   if ( (  f3%c1 /= 1 ) .or. ( .not. precision_r4(f3%c2,2.0) ) .or. (  f3%c3 /= 'abc' ) )   error stop 5_4
   if ( (  f4(1)%c1 /= 2 ) .or. ( .not. precision_r4(f4(1)%c2,3.0)) .or. (  f4(1)%c3 /= 'def' ) .or. &
        (  f4(2)%c1 /= 4 ) .or. ( .not. precision_r4(f4(2)%c2,5.0)) .or. (  f4(2)%c3 /= 'ghi' ) )   error stop 6_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

10 format ( I1,3/,/,1X,F5.2 )
   read (unit, 10, iostat=iostat )                      dtv%c1, dtv%c2
   if (iostat /= 0 )   error stop 9_4
   read (unit, "(/,1X,A3,/)", iostat = iostat )     dtv%c3

   iomsg = 'dtioread'

end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(17) :: format
   format = "(I4,3/,/,1X,F5.2)"
   write (unit, fmt=format, iostat=iostat )             dtv%c1, dtv%c2
   if (iostat /= 0 )   error stop 9_4
   write (unit, "(/,1X,A3,/)", iostat = iostat )        dtv%c3

   iomsg = 'dtiowrite'

end subroutine
