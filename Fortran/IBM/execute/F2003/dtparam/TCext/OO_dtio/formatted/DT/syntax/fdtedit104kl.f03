!*  ===================================================================
!*
!*  DATE                       : 2007-06-08 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        with char-literal-constant and v-list present
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
      integer(kb) :: i
   end type

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

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program fdtedit104kl
use m

   class(base(4)), allocatable :: b1
   class(base(4)), pointer     :: b2

   integer :: stat
   character(150) :: msg
   character(14) :: format = "(DT'read'(4))"
   open (1, file = 'fdtedit104kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)(100) )
   allocate ( b2, source = base(4)(200) )

10 format (DT'write'(3))

   write ( 1, 10, iostat = stat, iomsg = msg )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   rewind 1

   read ( 1,  fmt=format, iostat = stat, iomsg = msg )   b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 2_4
   if ( b2%i /= 100 )                               error stop 3_4

   close ( 1, status='delete' )

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "DTread" ) error stop 4_4
   if ( ( size(v_list, 1) /= 1 ) .or. ( v_list(1) /= 4 ) .or. ( len(iotype) /= 6 ) ) error stop 5_4

   read ( unit, "(I4)", iostat = iostat ) dtv%i
   iomsg = 'dtioread'

end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "DTwrite" ) error stop 6_4
   if ( ( size(v_list, 1) /= 1 ) .or. ( v_list(1) /= 3 ) .or. ( len(iotype) /= 7 ) ) error stop 7_4

   write ( unit, "(I4)", iostat = iostat ) dtv%i

   iomsg = 'dtiowrite'

end subroutine