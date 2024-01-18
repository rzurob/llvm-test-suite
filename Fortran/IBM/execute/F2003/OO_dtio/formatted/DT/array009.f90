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
!*                                        Array unlimited polymorphic entity contains
!*                                        unlimited polymorphic component within select type
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
      integer(4) :: i
      class(*), pointer :: u => null()
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

program array009
use m

   class(*), target, allocatable :: u1(:)
   class(*), pointer             :: u2

   integer, target :: i1 = 555
   character(3), target :: c1 = 'GHI'

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT'_u1'(1))"

   open (1, file = 'array009.1', form='formatted', access='sequential' )

   allocate( u1(3), source = (/ base(101,null()), base(102,null()), base(103,null()) /) )
   allocate( u2, source = base(201,null()))

10 format (DT'_u2'(2,3))

   select type ( u1 )
      type is ( base )
         u1(1)%u => i1
         u1(2)%u => c1
         u1(3)%u => u2
         write ( 1, "(DT'_u1-1'(1),DT'_u1-2'(-2),DT'_u1-3'(3))", iostat = stat, iomsg = msg )   u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4

         u1(2)%u => null()

         write ( 1, "(2(DT'_u1'(-1,-2,-3)), DT)", iostat = stat, iomsg = msg )                  u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4
   end select

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( unit, '(/)' )
   write ( unit, * ) 'iotype: ', iotype, ' v_list:', v_list

   if ( associated(dtv%u) ) then
      write ( unit, "(/1X,A)" ) 'component u allocated'
      select type ( g => dtv%u )
         type is ( integer )
            write ( unit, "(/I4)" ) g
         type is ( character(*) )
            write ( unit, "(/A4)" ) g
         type is ( base )
            write ( unit, "(/I4)" ) g%i
      end select
   end if

   write ( unit, "(/I4)" ) dtv%i

   iomsg = 'dtiowrite'

end subroutine
