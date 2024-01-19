!*  ===================================================================
!*
!*  DATE                       : 2007-06-05 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Scalar unlimited polymorphic entity contains
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

   type base (kb)
      integer, kind :: kb
      integer(kb) :: i
      class(*), pointer :: u => null()
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

program scalar009kl
use m

   class(*), target, allocatable :: u1
   class(*), pointer             :: u2

   integer, target :: i1 = 555
   character(3), target :: c1 = 'GHI'

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT'_u1'(1))"

   open (1, file = 'scalar009kl.1', form='formatted', access='sequential' )

   allocate( u1, source = base(4)(123,null()))
   allocate( u2, source = base(4)(456,null()))

10 format (DT'_u2'(2,3))

   select type ( u1 )
      type is ( base(4) )
         u1%u => i1
         write ( 1, fmt, iostat = stat, iomsg = msg )                          u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4
         u1%u => c1
         write ( 1, "(DT'_u1'(2))", iostat = stat, iomsg = msg )               u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4
         u1%u => u2
         write ( 1, "(DT'_u1'(3,4))", iostat = stat, iomsg = msg )             u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )   error stop 3_4
         deallocate ( u1%u )
         write ( 1, "(DT'_u1'(5,6,7))", iostat = stat, iomsg = msg )           u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )   error stop 4_4
   end select

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( unit, * ) 'iotype: ', iotype, ' v_list:', v_list

   if ( associated(dtv%u) ) then
      write ( unit, "(/1X,A)" ) 'component u allocated'
      select type ( g => dtv%u )
         type is ( integer )
            write ( unit, "(/I4)" ) g
         type is ( character(*) )
            write ( unit, "(/A4)" ) g
         type is ( base(4) )
            write ( unit, "(/I4)" ) g%i
      end select
   end if

   write ( unit, "(/I4)" ) dtv%i

   iomsg = 'dtiowrite'

end subroutine
