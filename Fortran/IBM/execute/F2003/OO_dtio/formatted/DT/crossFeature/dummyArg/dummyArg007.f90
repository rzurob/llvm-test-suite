!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Argument Association: assumed shape array
!*                                        unlimited polymorphic dummy argument
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
         class(*), intent(in) :: dtv(:)
         character(51) :: fmt = ''
         fmt = "(DT'_foo1'(5,6),/,DT'_foo2'(6,7),/, DT'_foo3'(7,8))"
         select type ( dtv )
            class is ( base )
               write (1, fmt, iostat = stat, iomsg = msg ) dtv
         end select
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv )
         class(*), allocatable, intent(inout) :: dtv(:)
      10 format (DT'_bar1'(4,5),/,DT'_bar2'(5,6),/,DT'_bar3'(6,7))

         select type ( dtv )
            type is ( base )
               write (1, 10, iostat = stat, iomsg = msg ) dtv
            type is ( child )
               write (1, "(DT'_bar4'(5,6),/,DT'_bar5'(6,7),/, DT'_bar6'(7,8))", iostat = stat, iomsg = msg ) dtv
         end select

         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 2_4

      end subroutine

      subroutine boo ( dtv )
         class(*), pointer, intent(in) :: dtv(:)
      10 format (DT'_boo1'(4,5),/,DT'_boo2'(5,6),/,DT'_boo3'(6,7))

         select type ( g => dtv )
            class is ( base )
               write (1, 10, iostat = stat, iomsg = msg ) g
         end select

         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 3_4

      end subroutine

end module

program dummyArg007
use m

   type(base), allocatable  :: b1(:)
   type(base)               :: b2(3) = (/ base(111), base(112), base(113) /)
   class(base), pointer     :: b3(:)

   type(child), allocatable :: c1(:,:)
   type(child)              :: c2(3) = (/ child(211,212), child(213,214), child(215,216) /)
   class(child), pointer    :: c3(:)

   class(*), allocatable :: u1(:)
   class(*), pointer     :: u2(:)

   open (1, file = 'dummyArg007.1', form='formatted', access='sequential' )

   allocate ( b1(2), source = (/ base(101), base(102) /) )
   allocate ( b3(4),   source = (/ child(121,122), child(123,124), child(125,126), child(127,128) /) )

   allocate ( c1(2,2), source = reshape ( source = (/ child(201,202), child(203,204), child(205,206), child(207,208) /), shape = (/ 2, 2 /) ) )
   allocate ( c3(4:6), source = (/ child(221,222), child(223,224), child(225,226) /) )

   call foo ( b1 )
   call foo ( b2 )
   call foo ( b3 )
   call foo ( reshape (source = c1, shape = (/4/) ) )
   call foo ( c2 )
   call foo ( c3 )

   allocate ( u1(5), source = (/ b1, b2 /) )

   call foo ( u1 )
   call bar ( u1 )

   deallocate ( u1 )

   allocate ( u1(3), source = (/ c3 /) )
   call bar ( u1 )

   allocate ( u2(7), source = (/ c1, c2 /) )
   call boo ( u2 )

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
