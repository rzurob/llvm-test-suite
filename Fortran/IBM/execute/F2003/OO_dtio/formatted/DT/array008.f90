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
!*                                        array unlimited polymorphic entity within select type
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
      character(3) :: c
   end type

   type, extends(base) :: child
      integer(4) :: i
   end type

   type, extends(child) :: gen3
      integer(4) :: j
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

program array008
use m

   class(*), target, allocatable :: u1(:), u2(:,:)
   class(*), pointer             :: u3(:), u4(:,:)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(4(DT'_u1'(1),/))"

   open (1, file = 'array008.1', form='formatted', access='sequential' )

   allocate( u1(4), source = (/ gen3('abc',101,111), gen3('def',102,112), gen3('ghi',103,113), gen3('jkl', 104, 114) /) )
   allocate( u2(2,2), source = reshape ( source = (/ child('ABC',201), child('DEF',202), &
                                                     child('GHI',203), child('JKL',204) /), shape = (/2,2/) ) )
   u3 => u1(4:1:-1)
   u4 => u2(1:2,1:2)

10 format (DT'_u2-1'(2,3),/, DT'_u2-2'(3,4))
20 format (DT'_u3-1'(5),/,DT'_u3-2'(6),/,DT'_u3-3'(7))

   select type ( u1 )
      type is ( gen3 )
         write ( 1, fmt, iostat = stat, iomsg = msg )                  u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4
   end select

   select type ( u2 )
      class is ( base )
         write ( 1, 10, iostat = stat, iomsg = msg )                   u2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4
   end select

   select type ( u3 )
      class is ( child )
         write ( 1, 20, iostat = stat, iomsg = msg )                   u3
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )   error stop 3_4
   end select

   select type ( u4 )
      type is ( child )
         write ( 1, "(DT'_u4'(8))", iostat = stat, iomsg = msg )       u4
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )   error stop 4_4
         select type ( u3 )
            class is ( child )
               u4 = reshape (source=u3(4:1:-1), shape = (/2,2/) )
               write ( 1, "(DT'_u4'(9,10,11))", iostat = stat, iomsg = msg ) u4
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 5_4
         end select
   end select

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, gen3

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( unit, * ) 'iotype: ', iotype, ' v_list:', v_list

   select type ( dtv )
      class is ( base )
         write ( unit, "(/5X,A3)" ) dtv%c
   end select
   select type ( dtv )
      class is ( child )
         write ( unit, "(/5X,I3)" ) dtv%i
   end select
   select type ( dtv )
      class is ( gen3 )
         write ( unit, "(/5X,I3)" ) dtv%j
   end select

   iomsg = 'dtiowrite'

end subroutine
