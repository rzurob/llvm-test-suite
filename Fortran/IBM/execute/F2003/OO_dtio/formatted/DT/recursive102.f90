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

   type base
      class(base), pointer :: next => null()
      integer(4) :: i = -999
   end type

   type, extends(base) :: child
      integer(4) :: j = -999
   end type

   interface read(formatted)
      recursive subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
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

program recursive102
use m

   class(base), allocatable, target :: head
   class(base), pointer :: dummy

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT(5,6),/)"

   open (1, file = 'recursive102.1', form='formatted', access='sequential' )

   allocate ( head, source = child() )
   allocate ( head%next, source = child() )
   allocate ( head%next%next, source = base() )
   allocate ( head%next%next%next, source = child() )
   allocate ( head%next%next%next%next, source = child() )

   idx = 1

   read ( 1, fmt, iostat = stat, iomsg = msg )              head
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   dummy => head
   do while (associated(dummy))
      select type ( dummy )
         type is ( base )
            print *, dummy%i
         type is ( child )
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
         type is ( base )
            print *, dummy%i
         type is ( child )
            print *, dummy%i, dummy%j
      end select
      dummy => dummy%next
   end do
   	
   print *, rbuffer

end program

recursive subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, read(formatted), idx, rbuffer

   class(base), intent(inout) :: dtv
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
      type is ( child )
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
