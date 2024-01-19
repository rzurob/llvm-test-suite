!*  ===================================================================
!*
!*  DATE                       : 2007-06-08 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
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
   type base (kb)
      integer, kind :: kb
      integer(kb)   :: i
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc)   :: j
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program fdtedit109kl
use m

   class(base(4)), allocatable  :: b1
   class(base(4)), pointer      :: b2
   type(base(4))                :: b3
   class(child(4,4)), allocatable :: c1
   class(child(4,4)), pointer     :: c2
   type(child(4,4))               :: c3

   integer :: stat
   character(150) :: msg
   character(31) :: fmt1 = "(DT'_1'(1),DT'_2'(2),DT'_3'(3))"

   ! allocation of variables

   allocate ( b1, source = base(4)( 101 ) )
   allocate ( b2, source = child(4,4)( 102, 103 ) )
   b3 = base(4)( 104 )

   allocate ( c1, source = child(4,4)( 201, 202 ) )
   allocate ( c2, source = child(4,4)( 203, 204 ) )
   c3 = child(4,4)( 205, 206 )

   open (1, file = 'fdtedit109kl.1', form='formatted', access='sequential' )

   write ( 1, fmt1, iostat = stat, iomsg = msg )      b1, b2, b3, c1, c2, c3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 1_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is ( base(4) )
         write ( unit, *, iostat = iostat ) 'BASE:', iotype, v_list, 'dtv%i=', dtv%i
      type is ( child(4,4) )
         write ( unit, *, iostat = iostat ) 'CHILD:',iotype, v_list, 'dtv%i=', dtv%i, 'dtv%j=', dtv%j
   end select
   iomsg = 'dtiowrite'

end subroutine
