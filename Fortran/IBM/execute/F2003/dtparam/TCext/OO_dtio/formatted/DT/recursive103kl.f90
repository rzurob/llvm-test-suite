!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : recursive103kl
!*
!*  PROGRAMMER                 : David Forster (derived from recursive103 by Robert Ma)
!*  DATE                       : 2007-06-05 (original: 21/03/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        recursively read a polymorphic linked list (contains inner type) (read)
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

   type data (kd)
      integer, kind :: kd
      integer(kd) :: i = -999
   end type

   type base (kb)
      integer, kind :: kb
      class(base(kb)), pointer :: next => null()
      type(data(kb)) :: d1 = data(kb)()
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      type(data(kc)) :: d2 = data(kc)()
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
   character(10) :: rbuffer(28)

end module

program recursive103kl
use m

   class(base(4)), pointer :: head, dummy

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT(-5,-6),/)"

   open (1, file = 'recursive103kl.1', form='formatted', access='sequential' )

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
            print *, dummy%d1%i
         type is ( child(4,4) )
            print *, dummy%d1%i, dummy%d2%i
      end select
      dummy => dummy%next
   end do

   fmt = "(DT(-4,-5))"

   read ( 1, fmt, iostat = stat, iomsg = msg )              head
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   dummy => head
   do while (associated(dummy))
      select type ( dummy )
         type is ( base(4) )
            print *, dummy%d1%i
         type is ( child(4,4) )
            print *, dummy%d1%i, dummy%d2%i
      end select
      dummy => dummy%next
   end do
   
   i = 1
   do while ( i .lt. 29 )
      print *,rbuffer(i)
      i = i +1
   end do

end program

recursive subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, read(formatted), idx, data, rbuffer

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt

   interface read(formatted)
      subroutine readformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         class(data(4)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   write ( fmt, * ) '(DT(', -1*v_list(1),'))'
   read ( unit, fmt, iostat = iostat )    dtv%d1

   select type ( dtv )
      type is ( child(4,4) )
         write ( fmt, * ) '(DT(', -1*v_list(2),'))'
         read ( unit, fmt, iostat = iostat )    dtv%d2
   end select

   if ( associated ( dtv%next ) ) then
      write ( fmt, * ) '(/,DT(',v_list(1)-1,',',v_list(2)-1,'))'
      read ( unit, fmt, iostat = iostat, iomsg = iomsg ) dtv%next
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioread' ) )  error stop 3_4
   end if

   iomsg = 'dtioread'

end subroutine

subroutine readformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data, idx, rbuffer

   class(data(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt
   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   write ( fmt, * ) '(I', v_list(1),')'
   read ( unit, fmt, iostat = iostat )    dtv%i

   iomsg = 'dtioread'

end subroutine
