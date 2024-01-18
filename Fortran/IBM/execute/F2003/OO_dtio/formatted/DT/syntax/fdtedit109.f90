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
!*                                        When editors in format specification are used up and there are
!*                                        still list item, runtime should revert to appropriate location of the format
!*                                        specification (write)
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

program fdtedit109
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

   allocate ( b1, source = base( 101 ) )
   allocate ( b2, source = child( 102, 103 ) )
   b3 = base( 104 )

   allocate ( c1, source = child( 201, 202 ) )
   allocate ( c2, source = child( 203, 204 ) )
   c3 = child( 205, 206 )

   open (1, file = 'fdtedit109.1', form='formatted', access='sequential' )

   write ( 1, fmt1, iostat = stat, iomsg = msg )      b1, b2, b3, c1, c2, c3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 1_4

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
         write ( unit, *, iostat = iostat ) 'BASE:', iotype, v_list, 'dtv%i=', dtv%i
      type is ( child )
         write ( unit, *, iostat = iostat ) 'CHILD:',iotype, v_list, 'dtv%i=', dtv%i, 'dtv%j=', dtv%j
   end select
   iomsg = 'dtiowrite'

end subroutine
