!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar002akl
!*
!*  PROGRAMMER                 : David Forster (derived from scalar002a by Robert Ma)
!*  DATE                       : 2007-06-05 (original: 21/03/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        scalar polymorphic derived type variable with abstract type
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

   type, abstract :: base (kb)
      integer, kind :: kb
      integer(kb) :: i
      contains
         procedure(inf), pass, deferred :: getc
   end type

   type, extends(base) :: child (lc)
      integer, len :: lc
      character(lc) :: c
      contains
         procedure, pass :: getc
   end type

   interface
      character(3) function inf(dtv)
         import base
         class(base(4)), intent(in) :: dtv
      end function
   end interface

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

   contains

      function getc(dtv)
         class(child(4,*)), intent(in) :: dtv
         character(dtv%lc) :: getc
         getc = dtv%c
      end function

end module

program scalar002akl
use m

   class(base(4)), pointer      :: b1
   class(child(4,:)), allocatable :: c1
   class(child(4,:)), pointer     :: c2

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT'c1'(10))"

   open (1, file = 'scalar002akl.1', form='formatted', access='sequential' )

   allocate ( b1, source = child(4,3)(200,'abc') )
   allocate ( c1, source = child(4,3)(300,'def') )
   allocate ( c2, source = child(4,3)(400,'ghi') )

10 format (DT'b1')

   write ( 1, 10, iostat = stat, iomsg = msg )                b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, fmt, iostat = stat, iomsg = msg )    c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'c2'(10,5))", iostat = stat, iomsg = msg )  c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt

   write ( unit, * ) 'iotype:', iotype

   select type ( dtv )
      type is (child(4,*))
         if ( size(v_list) == 0 ) then
            write ( unit, * ) ' empty v_list'
            write ( unit, *, iostat = iostat )      dtv%i, dtv%getc()
         else if ( size(v_list) == 1 ) then
            write ( unit, * ) ' v_list:',v_list
            write ( fmt, * ) '(I', v_list(1),')'
            write ( unit, fmt, iostat = iostat )    dtv%i
            write ( unit, *, iostat = iostat )      dtv%getc()
         else if ( size(v_list) == 2 ) then
            write ( unit, * ) ' v_list:',v_list
            write ( fmt, * ) '(I', v_list(1),',A',v_list(2),')'
            write ( unit, fmt, iostat = iostat )    dtv%i, dtv%getc()
         end if
   end select

   iomsg = 'dtiowrite'

end subroutine
