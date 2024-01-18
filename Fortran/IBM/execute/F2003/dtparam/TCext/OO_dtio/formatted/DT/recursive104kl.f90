!*  ===================================================================
!*
!*  TEST CASE NAME             : recursive104kl
!*
!*  DATE                       : 2007-06-05 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        derived type containing linked list component
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
      integer(kb) :: i = -9
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: j = -9
   end type

   type linkedlist (kll)
      integer, kind :: kll
      class(base(kll)), pointer :: head => null()
   end type

   interface read(formatted)

      subroutine readformattedll(dtv, unit, iotype, v_list, iostat, iomsg )
         import linkedlist
         class(linkedlist(4)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

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
   character(25) :: rbuffer(13)
end module

program recursive104kl
use m

   class(linkedlist(4)), allocatable :: ll
   class(base(4)), pointer :: dummy

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT(5,6),/)"

   open (1, file = 'recursive104kl.1', form='formatted', access='sequential' )

   allocate ( ll )

   allocate ( ll%head, source = child(4,4)() )
   allocate ( ll%head%next, source = child(4,4)() )
   allocate ( ll%head%next%next, source = base(4)() )
   allocate ( ll%head%next%next%next, source = child(4,4)() )
   allocate ( ll%head%next%next%next%next, source = child(4,4)() )

   idx = 1

   read ( 1, fmt, iostat = stat, iomsg = msg )                ll
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   dummy => ll%head
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

   read ( 1, fmt, iostat = stat, iomsg = msg )                ll
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   dummy => ll%head
   do while (associated(dummy))
      select type ( dummy )
         type is ( base(4) )
            print *, dummy%i
         type is ( child(4,4) )
            print *, dummy%i, dummy%j
      end select
      dummy => dummy%next
   end do

   i = 1
   do while ( i .lt. 13 )
      print *,rbuffer(i)
      i = i +1
   end do

end program

recursive subroutine readformattedll (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, linkedlist, read(formatted), idx, rbuffer

   class(linkedlist(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1
   write ( fmt, * ) '(/,DT"base"(', v_list(1),',', v_list(2),'))'
   read ( unit, fmt, iostat = iostat, iomsg = iomsg )    dtv%head

   if ( ( iostat /= 0 ) .or. ( iomsg /= 'baseread' ) )   error stop 3_4

   iomsg = 'dtioread'

end subroutine

recursive subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, read(formatted), idx, rbuffer

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1
   write ( fmt, * ) '(I', v_list(1),')'
   read ( unit, fmt, iostat = iostat )    dtv%i

   select type ( dtv )
      type is ( child(4,4) )
         write ( fmt, * ) '(I', v_list(2),')'
         read ( unit, fmt, iostat = iostat )    dtv%j
   end select

   if ( associated ( dtv%next ) ) then
      write ( fmt, * ) '(/,DT"base"(',v_list(1)+1,',',v_list(2)+1,'))'
      read ( unit, fmt, iostat = iostat, iomsg = iomsg ) dtv%next
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'baseread' ) )  error stop 4_4
   end if

   iomsg = 'baseread'

end subroutine
