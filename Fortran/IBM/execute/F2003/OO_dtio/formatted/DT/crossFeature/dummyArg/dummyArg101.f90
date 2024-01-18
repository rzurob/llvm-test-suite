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
!*                                        Argument Association: non-polymorphic scalar dummy argument (read)
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
      integer(4) :: i = -999
   end type

   type, extends(base) :: child
      integer(4) :: j = -999
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

   integer :: stat
   character(150) :: msg

   character(20) :: rbuffer(6)
   integer :: idx

   contains

      subroutine foo ( dtv )
         type(base), intent(inout) :: dtv
         character(13) :: fmt = "(DT'base'(5))"

         read (1, fmt, iostat = stat, iomsg = msg ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv )
         type(child), intent(inout) :: dtv
      10 format (DT'child'(8,9))

         read (1, 10, iostat = stat, iomsg = msg ) dtv

         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 2_4

      end subroutine

end module

program dummyArg101
use m

   type(base), allocatable :: b1
   type(base), pointer     :: b2
   type(base)              :: b3

   type(child), allocatable :: c1
   type(child), pointer     :: c2
   type(child)              :: c3

   open (1, file = 'dummyArg101.1', form='formatted', access='sequential' )

   allocate ( b1, source = base() )
   allocate ( b2, source = base() )
   allocate ( c1, source = child() )
   allocate ( c2, source = child() )

   idx = 1

   call foo ( b1 )
   call foo ( b2 )
   call foo ( b3 )

   call bar ( c1 )
   call bar ( c2 )
   call bar ( c3 )

   print *, rbuffer

   if ( b1%i /= 101 ) error stop 3_4
   if ( b2%i /= 102 ) error stop 4_4
   if ( b3%i /= 103 ) error stop 5_4
   if ( ( c1%i /= 201 ) .or. ( c1%j /= 211 ) ) error stop 6_4
   if ( ( c2%i /= 202 ) .or. ( c2%j /= 212 ) ) error stop 7_4
   if ( ( c3%i /= 203 ) .or. ( c3%j /= 213 ) ) error stop 8_4


end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, rbuffer, idx

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   select type ( dtv )
      type is ( base )
         write ( fmt, * ) '(I', v_list(1),')'
         read ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child )
         write ( fmt, * ) '(I', v_list(1),', I', v_list(2),')'
         read ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
   end select
   iomsg = 'dtioread'

end subroutine
