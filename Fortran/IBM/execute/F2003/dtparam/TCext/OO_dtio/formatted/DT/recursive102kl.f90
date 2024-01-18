!*  ===================================================================
!*
!*  TEST CASE NAME             : recursive102kl
!*
!*  DATE                       : 2007-06-05 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        recursively read a polymorphic linked list (read)
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
      class(base(kb)), pointer :: next => null()
      integer(kb) :: i = -999
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: j = -999
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

program recursive102kl
use m

   class(base(4)), allocatable, target :: head
   class(base(4)), pointer :: dummy

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT(5,6),/)"

   open (1, file = 'recursive102kl.1', form='formatted', access='sequential' )

   allocate ( head, source = child(4,4)() )
   allocate ( head%next, source = child(4,4)() )
   allocate ( head%next%next, source = base(4)() )
   allocate ( head%next%next%next, source = child(4,4)() )
   allocate ( head%next%next%next%next, source = child(4,4)() )

   idx = 1

   read ( 1, fmt, iostat = stat, iomsg = msg )              head
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   dummy => head
   do while (associated(dummy))
      select type ( dummy )
         type is ( base(4) )
            print *, dummy%i
         type is ( child(4,4) )
            print *, dummy%i, dummy%j
      end select
      dummy => dummy%next
   end do

   fmt = "(DT'_ll'(4,5))"

   read ( 1, fmt, iostat = stat, iomsg = msg )              head
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   dummy => head
   do while (associated(dummy))
      select type ( dummy )
         type is ( base(4) )
            print *, dummy%i
         type is ( child(4,4) )
            print *, dummy%i, dummy%j
      end select
      dummy => dummy%next
   end do

   print *, rbuffer

end program

recursive subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, read(formatted), idx, rbuffer

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt
   write ( rbuffer(idx), * ) iotype, v_list

   write ( fmt, * ) '(I', v_list(1),')'
   read ( unit, fmt, iostat = iostat )    dtv%i
   idx = idx + 1
   select type ( dtv )
      type is ( child(4,4) )
         write ( fmt, * ) '(I', v_list(2),')'
         read ( unit, fmt, iostat = iostat )    dtv%j
   end select

   if ( associated ( dtv%next ) ) then
      write ( fmt, * ) '(/,DT(',v_list(1)+1,',',v_list(2)+1,'))'
      read ( unit, fmt, iostat = iostat, iomsg = iomsg ) dtv%next
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioread' ) )  error stop 3_4
   end if

   iomsg = 'dtioread'

end subroutine
