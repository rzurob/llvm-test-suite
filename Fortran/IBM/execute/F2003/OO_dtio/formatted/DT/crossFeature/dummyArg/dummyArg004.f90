!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Argument Association: assumed shape array dummy argument
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

      subroutine foo ( dtv )
         type(base), intent(in) :: dtv(:)
         character(63) :: fmt = "(DT'_type_base1'(5),/,DT'_type_base2'(6),/, DT'_type_base3'(7))"

         write (1, fmt, iostat = stat, iomsg = msg ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv )
         class(base), intent(in) :: dtv(11:)
      10 format (DT'_class_base11'(4,5),/,DT'_class_base12'(5,6),/,DT'_class_base13'(6,7))

         write (1, 10, iostat = stat, iomsg = msg ) dtv

         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 2_4

      end subroutine

end module

program dummyArg004
use m

   type(base), allocatable  :: b1(:)
   type(base)               :: b2(3) = (/ base(111), base(112), base(113) /)
   class(base), pointer     :: b3(:)

   type(child), allocatable :: c1(:,:)
   type(child)              :: c2(3) = (/ child(211,212), child(213,214), child(215,216) /)
   class(child), pointer    :: c3(:)

   open (1, file = 'dummyArg004.1', form='formatted', access='sequential' )

   allocate ( b1(2), source = (/ base(101), base(102) /) )
   allocate ( b3(4),   source = (/ child(121,122), child(123,124), child(125,126), child(127,128) /) )

   allocate ( c1(2,2), source = reshape ( source = (/ child(201,202), child(203,204), child(205,206), child(207,208) /), shape = (/ 2, 2 /) ) )
   allocate ( c3(4:6), source = (/ child(221,222), child(223,224), child(225,226) /) )

   call foo ( b1 )
   call foo ( b2 )
   call foo ( b3 )
   call bar ( b1 )
   call bar ( b2 )
   call bar ( b3 )
   call bar ( reshape ( source = c1, shape = (/ 4 /) ) )
   call bar ( c2 )
   call bar ( c3 )

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
         write ( fmt, * ) '(I', v_list(1),')'
         write ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child )
         write ( fmt, * ) '(I', v_list(1),', I', v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
   end select
   iomsg = 'dtiowrite'

end subroutine
