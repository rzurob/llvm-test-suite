!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 21/03/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        have a large number of v-list items
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
      integer(4) :: i
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

   character(250) :: rbuffer, wbuffer
end module

program fdtedit106
use m

   class(base), allocatable :: b1
   class(base), pointer     :: b2

   integer :: stat
   character(150) :: msg

   character(500) :: rformat, wformat

   open (1, file = 'fdtedit106.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(100) )
   allocate ( b2, source = base(200) )

   write ( wformat, * ) "(DT'write'(",(i,",",i=1,49),50,"))"
   write ( rformat, * ) "(DT'read'(", (i,",",i=1,49),50,"))"

   write ( 1, wformat, iostat = stat, iomsg = msg )       b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   rewind 1

   read ( 1,  rformat, iostat = stat, iomsg = msg )   b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 2_4
   if ( b2%i /= 100 )                               error stop 3_4

   print *, rbuffer
   print *, wbuffer

   close ( 1, status='delete' )

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, rbuffer

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "DTread" ) error stop 4_4
   if ( ( size(v_list, 1) /= 50 ) .or. ( len(iotype) /= 6 ) ) error stop 5_4

   write ( rbuffer, * )                   iotype, v_list
   read ( unit, "(I4)", iostat = iostat ) dtv%i
   iomsg = 'dtioread'

end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, wbuffer

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "DTwrite" ) error stop 6_4
   if ( ( size(v_list, 1) /= 50 ) .or. ( len(iotype) /= 7 ) ) error stop 7_4

   write ( wbuffer, * )                    iotype, v_list
   write ( unit, "(I4)", iostat = iostat ) dtv%i
   iomsg = 'dtiowrite'

end subroutine
