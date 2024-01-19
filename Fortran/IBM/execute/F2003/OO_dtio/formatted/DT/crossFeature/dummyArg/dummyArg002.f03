!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Argument Association: polymorphic scalar dummy argument
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

   type, extends(child) :: gen3
      integer(4) :: k = -999
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
         class(base), intent(in) :: dtv
         character(17) :: fmt = "(DT'_foo'(5,6,7))"

         write (1, fmt, iostat = stat, iomsg = msg ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv )
         class(child), intent(inout) :: dtv
      10 format (DT'_bar'(8,9,10))

         write (1, 10, iostat = stat, iomsg = msg ) dtv

         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 2_4

      end subroutine

end module

program dummyArg002
use m

   type(base), allocatable  :: b1
   type(base)               :: b2 = base ( 102 )
   class(base), pointer     :: b3

   type(child), pointer      :: c1
   type(child)               :: c2 = child ( 202, 212 )
   class(child), allocatable :: c3

   type(gen3) :: g1 = gen3( 301, 311, 321 )
   class(gen3), pointer :: g2

   open (1, file = 'dummyArg002.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(101) )
   allocate ( b3, source = gen3(103, 113, 123) )

   allocate ( c1, source = child(201, 211) )
   allocate ( c3, source = gen3 (203, 212, 232) )
   allocate ( g2, source = gen3 (302, 312, 322) )

   call foo ( b1 )
   call foo ( b2 )
   call foo ( b3 )

   call foo ( c1 )
   call foo ( c2 )
   call foo ( c3 )

   call foo ( g1 )
   call foo ( g2 )

   call bar ( c1 )
   call bar ( c2 )
   call bar ( c3 )

   call bar ( g1 )
   call bar ( g2 )

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, gen3

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   select type ( dtv )
      type is ( base )
         write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list(1)
         write ( fmt, * ) '(I', v_list(1),')'
         write ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child )
         write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list(1:2)
         write ( fmt, * ) '(I', v_list(1),', I', v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
      type is ( gen3 )
         write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list(1:3)
         write ( fmt, * ) '(I', v_list(1),', I', v_list(2),', I',v_list(3),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j, dtv%k
   end select
   iomsg = 'dtiowrite'

end subroutine
