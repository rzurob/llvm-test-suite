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
!*                                        specification (with parentheses inside the format statement) (write)
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
      integer(4)   :: i
   end type

   type, extends(base) :: child
      integer(4)   :: j
   end type

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

end module

program fdtedit109b
use m

   class(base), allocatable  :: b1
   class(base), pointer      :: b2
   type(base)                :: b3
   type(base), pointer       :: b4
   type(base), allocatable   :: b5
   class(child), allocatable :: c1
   class(child), pointer     :: c2
   type(child)               :: c3
   type(child), pointer      :: c4
   type(child), allocatable  :: c5

   integer :: stat
   character(150) :: msg

   character(37) :: fmt = "(DT'_1'(1),2(DT'_2'(2)),DT'_3'(3) )"

   ! allocation of variables

   allocate ( b1, source = base( 101 ) )
   allocate ( b2, source = child( 102, 103 ) )
   b3 = base( 104 )
   allocate ( b4, source = base( 105 ) )
   allocate ( b5, source = base( 106 ) )

   allocate ( c1, source = child( 201, 202 ) )
   allocate ( c2, source = child( 203, 204 ) )
   c3 = child( 205, 206 )
   allocate ( c4, source = child( 207, 208 ) )
   allocate ( c5, source = child( 209, 210 ) )

   open (1, file = 'fdtedit109b.1', form='formatted', access='sequential' )

   write ( 1, fmt, iostat = stat, iomsg = msg )      b1, b2, b3, b4, b5, c1, c2, c3, c4, c5
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is ( base )
         write ( unit, *, iostat = iostat ) ' BASE:', iotype, v_list, ' dtv%i=', dtv%i
      type is ( child )
         write ( unit, *, iostat = iostat ) ' CHILD:',iotype, v_list, ' dtv%i=', dtv%i, ' dtv%j=', dtv%j
   end select
   iomsg = 'dtiowrite'

end subroutine
