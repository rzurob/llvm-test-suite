!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        When editors in format specification are used up and there are
!*                                        still list item, runtime should revert to appropriate location of the format
!*                                        specification (read)
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
      integer(4)   :: i = -9
   end type

   type, extends(base) :: child
      integer(4)   :: j = -99
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

   character(10) :: rbuffer(6)
   integer(4)    :: idx

end module

program fdtedit109a1
use m

   class(base), allocatable  :: b1
   class(base), pointer      :: b2
   type(base)                :: b3
   class(child), allocatable :: c1
   class(child), pointer     :: c2
   type(child)               :: c3

   integer :: stat
   character(150) :: msg
   character(31) :: fmt1 = "(DT'_1'(1),DT'_2'(2),DT'_3'(3))"

   ! allocation of variables

   allocate ( b1, source = base( ) )
   allocate ( b2, source = child( ) )
   b3 = base( )

   allocate ( c1, source = child( ) )
   allocate ( c2, source = child( ) )
   c3 = child( )

   open (1, file = 'fdtedit109a1.1', form='formatted', access='sequential' )

   idx = 1

   read ( 1, fmt1, iostat = stat, iomsg = msg )      b1, b2, b3, c1, c2, c3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )   error stop 1_4

   if ( b1%i /= 101 )                                error stop 2_4

   select type ( b2 )
      type is (child)
         if ( ( b2%i /= 102 ) .or. ( b2%j /= 103 ) ) error stop 3_4
   end select

   if ( ( b3%i /= 104 )                      )       error stop 4_4

   if ( ( c1%i /= 201 ) .or. ( c1%j /= 202 ) )       error stop 5_4
   if ( ( c2%i /= 203 ) .or. ( c2%j /= 204 ) )       error stop 6_4
   if ( ( c3%i /= 205 ) .or. ( c3%j /= 206 ) )       error stop 7_4

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, idx, rbuffer

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   select type ( dtv )
      type is ( base )
         read ( unit, *, iostat = iostat ) dtv%i
      type is ( child )
         read ( unit, *, iostat = iostat ) dtv%i, dtv%j
   end select
   iomsg = 'dtioread'

end subroutine