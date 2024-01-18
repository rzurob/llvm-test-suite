!*  ===================================================================
!*
!*  TEST CASE NAME             : recursive101kl
!*
!*  DATE                       : 2007-06-05 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        recursively read a non-polymorphic linked list (read)
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
      type(base(kb)), pointer :: next => null()
      integer(kb) :: i = -999
   end type

   interface read(formatted)
      recursive subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer(4) :: idx
   character(10) :: rbuffer(10)
end module

program recursive101kl
use m

   type(base(4)), allocatable, target :: head
   type(base(4)), pointer :: dummy

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT(5))"

   open (1, file = 'recursive101kl.1', form='formatted', access='sequential' )

   allocate ( head, source = base(4)() )
   allocate ( head%next, source = base(4)() )
   allocate ( head%next%next, source = base(4)() )
   allocate ( head%next%next%next, source = base(4)() )
   allocate ( head%next%next%next%next, source = base(4)() )

   idx = 1

   read ( 1, fmt, iostat = stat, iomsg = msg )              head
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   dummy => head
   do while (associated(dummy))
      print *, dummy%i
      dummy => dummy%next
   end do

   fmt = "(DT'_ll'(15))"

   read ( 1, fmt, iostat = stat, iomsg = msg )              head
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
   dummy => head
   do while (associated(dummy))
      print *, dummy%i
      dummy => dummy%next
   end do

   print *, rbuffer
end program

recursive subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, read(formatted), idx, rbuffer

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list

   write ( fmt, * ) '(I', v_list(1),',/)'
   read ( unit, fmt, iostat = iostat )    dtv%i
   idx = idx + 1

   if ( associated ( dtv%next ) ) then
      write ( fmt, * ) '(DT(',v_list(1),'))'
      read ( unit, fmt, iostat = iostat, iomsg = iomsg ) dtv%next
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioread' ) )  error stop 3_4
   end if

   iomsg = 'dtioread'

end subroutine
