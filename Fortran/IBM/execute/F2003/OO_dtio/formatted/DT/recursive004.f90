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

   type base
      class(base), pointer :: next => null()
      integer :: i
   end type

   type, extends(base) :: child
      integer :: j
   end type

   type linkedlist
      class(base), pointer :: head => null()
   end type

   interface write(formatted)

      subroutine writeformattedll(dtv, unit, iotype, v_list, iostat, iomsg )
         import linkedlist
         class(linkedlist), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

      recursive subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

   end interface

end module

program recursive004
use m

   class(linkedlist), allocatable :: ll

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT(5,6),/)"

   open (1, file = 'recursive004.1', form='formatted', access='sequential' )

   allocate ( ll )

   allocate ( ll%head, source = child(null(),100,101) )
   allocate ( ll%head%next, source = child(null(), 200, 201) )
   allocate ( ll%head%next%next, source = base(null(), 300) )
   allocate ( ll%head%next%next%next, source = child(null(), 400, 401) )
   allocate ( ll%head%next%next%next%next, source = child(null(), 500, 501) )

   write ( 1, fmt, iostat = stat, iomsg = msg )                ll
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   fmt = "(DT'_ll'(4,5))"

   write ( 1, fmt, iostat = stat, iomsg = msg )                ll
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program

recursive subroutine writeformattedll (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, linkedlist, write(formatted)

   class(linkedlist), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(22) :: fmt

   write ( unit, * ) 'Linked list: ', ' iotype:', iotype, ' v_list:', v_list

   write ( fmt, * ) '(/,DT"base"(', v_list(1),',', v_list(2),'))'
   write ( unit, fmt, iostat = iostat, iomsg = iomsg )    dtv%head

   if ( ( iostat /= 0 ) .or. ( iomsg /= 'basewrite' ) )   error stop 3_4

   iomsg = 'dtiowrite'

end subroutine

recursive subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, write(formatted)

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt
   
   write ( unit, *, iostat = iostat )      'iotype:', iotype
   write ( fmt, * ) '(I', v_list(1),')'
   write ( unit, fmt, iostat = iostat )    dtv%i

   select type ( dtv )
      type is ( child )
         write ( fmt, * ) '(I', v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%j
   end select

   if ( associated ( dtv%next ) ) then
      write ( fmt, * ) '(/,DT"base"(',v_list(1)+1,',',v_list(2)+1,'))'
      write ( unit, fmt, iostat = iostat, iomsg = iomsg ) dtv%next
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'basewrite' ) )  error stop 4_4
   end if

   iomsg = 'basewrite'

end subroutine
