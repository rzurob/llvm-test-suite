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
!*                                        Associate Constructor: (Non-) Polymorphic Scalar Temporary Entities
!*                                        array constructor, structure constructor, function return
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
      real(4) :: i = -999.0
   end type

   type, extends(base) :: child
      real(4) :: j = -999.0
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

   integer :: stat
   character(150) :: msg

   contains

      class(base) function returnMe (dtv)
         class(base), intent(in) :: dtv
         allocatable :: returnMe
         allocate ( returnMe, source = dtv )
      end function

end module

program associate001a
use m

   class(base), allocatable  :: b1
   class(base), pointer      :: b2
   class(child), pointer     :: c1

   open (1, file = 'associate001a.1', form='formatted', access='sequential' )

   allocate ( b1, source = base  ( 101.0 ) )
   allocate ( b2, source = child ( 102.0 , 103.0 ) )
   allocate ( c1, source = child ( 201.0 , 202.0 ) )

   associate ( g => (/ b1, b1, b1 /) )  ! array constructor
      write ( 1, "(DT(7,2))", iostat = stat, iomsg = msg ) g  !<- format spec reversion
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end associate

   associate ( g => base(901.0), h => child ( 902.0, 903.0 ), i => base ( 904.0 ) ) ! structure constructor
      write ( 1, "(DT'g'(7,2),DT'h'(8,3,9,4), dt'i'(9,4))", iostat = stat, iomsg = msg ) g, h, i
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
   end associate

   associate ( g => returnMe ( b1 ), h => returnMe ( b2 ), i => returnMe ( c1 ) )
      write ( 1, "(DT'g'(7,2),DT'h'(8,3,9,4), dt'i'(9,4,9,4))", iostat = stat, iomsg = msg ) g, h, i
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4
   end associate

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list

   select type ( dtv )
      type is ( base )
         write ( fmt, "(A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child )
         write ( fmt, "(A2,I1,A1,I1,A2,I1,A1,I1,A1)" ) '(F', v_list(1),'.',v_list(2),',F',v_list(3),'.',v_list(4),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
   end select
   iomsg = 'dtiowrite'

end subroutine
