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
!*                                        Argument Association: assumed size array
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
         class(*), intent(in) :: dtv(2,*)
         character(88) :: fmt = ''
         fmt = "(DT'_foo1'(5,6),/,DT'_foo2'(6,7),/, DT'_foo3'(7,8),/, DT'_foo4'(8,9),/, DT'bad'(9,10))"
         select type ( dtv )
            class is ( base )
               write (1, fmt, iostat = stat, iomsg = msg ) dtv(1:2,1), dtv(1:2,2)
         end select
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 1_4

      end subroutine

end module

program dummyArg008
use m

   type(base), allocatable  :: b1(:)
   type(base)               :: b2(4) = (/ base(111), base(112), base(113), base(114) /)
   class(base), pointer     :: b3(:)

   type(child), allocatable :: c1(:,:)
   type(child)              :: c2(4) = (/ child(211,212), child(213,214), child(215,216), child(217,218) /)
   class(child), pointer    :: c3(:)

   class(*), allocatable :: u1(:)
   class(*), pointer     :: u2(:)

   open (1, file = 'dummyArg008.1', form='formatted', access='sequential' )

   allocate ( b1(4), source = (/ base(101), base(102), base(103), base(104) /) )
   allocate ( b3(4), source = (/ child(121,122), child(123,124), child(125,126), child(127,128) /) )

   allocate ( c1(2,2), source = reshape ( source = (/ child(201,202), child(203,204), child(205,206), child(207,208) /), shape = (/ 2, 2 /) ) )
   allocate ( c3(4:7), source = (/ child(221,222), child(223,224), child(225,226), child(227, 228) /) )

   call foo ( b1 )
   call foo ( b2 )
   call foo ( b3 )
   call foo ( reshape (source = c1, shape = (/4/) ) )
   call foo ( c2 )
   call foo ( c3 )

   allocate ( u1(4), source = (/ b1(1:2), b2(3:4) /) )

   call foo ( u1 )

   allocate ( u2(4), source = (/ c1(1:2,1), c2(3:4) /) )
   call foo ( u2 )

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
