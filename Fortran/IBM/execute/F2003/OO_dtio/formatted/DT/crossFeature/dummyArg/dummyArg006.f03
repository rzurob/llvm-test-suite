!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Argument Association: scalar unlimited polymorphic dummy argument
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
         class(*), intent(in) :: dtv
         character(51) :: fmt = ''
         fmt = "(DT'_foo1'(5,6))"
         select type ( dtv )
            class is ( base )
               write (1, fmt, iostat = stat, iomsg = msg ) dtv
         end select
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv )
         class(*), allocatable, intent(inout) :: dtv
      10 format (DT'_bar1'(4,5))

         select type ( dtv )
            type is ( base )
               write (1, 10, iostat = stat, iomsg = msg ) dtv
            type is ( child )
               write (1, "(DT'_bar4'(5,6))", iostat = stat, iomsg = msg ) dtv
         end select

         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 2_4

      end subroutine

      subroutine boo ( dtv )
         class(*), pointer, intent(in) :: dtv
      10 format (DT'_boo1'(4,5))

         select type ( g => dtv )
            class is ( base )
               write (1, 10, iostat = stat, iomsg = msg ) g
         end select

         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 3_4

      end subroutine

end module

program dummyArg006
use m

   type(base), allocatable  :: b1
   type(base)               :: b2 = base ( 111 )
   class(base), pointer     :: b3

   type(child), allocatable :: c1
   type(child)              :: c2 = child(211,212)
   class(child), pointer    :: c3

   class(*), allocatable :: u1
   class(*), pointer     :: u2

   open (1, file = 'dummyArg006.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(101) )
   allocate ( b3, source = child(121,122) )

   allocate ( c1, source = child(201,202) )
   allocate ( c3, source = child(221,222) )

   call foo ( b1 )
   call foo ( b2 )
   call foo ( b3 )
   call foo ( c1 )
   call foo ( c2 )
   call foo ( c3 )

   allocate ( u1, source = b1 )

   call foo ( u1 )
   call bar ( u1 )

   deallocate ( u1 )

   allocate ( u1, source = c3 )
   call bar ( u1 )

   allocate ( u2, source = c1 )
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
