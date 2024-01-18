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
!*                                        scalar polymorphic derived type variable with abstract type (read)
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

   type, abstract :: base
      integer(4) :: i
      contains
         procedure(inf), pass, deferred :: setc
   end type

   type, extends(base) :: child
      character(3) :: c
      contains
         procedure, pass :: setc
   end type

   interface
      subroutine inf(dtv,c)
         import base
         class(base), intent(inout) :: dtv
         character(3), intent(in) :: c
      end subroutine
   end interface

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: idx
   character(10) :: rbuffer(3)

   contains

      subroutine setc(dtv,c)
         class(child), intent(inout) :: dtv
         character(3),intent(in) :: c
         dtv%c = c
      end subroutine

end module

program scalar102a
use m

   class(base), pointer      :: b1
   class(child), allocatable :: c1
   class(child), pointer     :: c2

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT'c1'(10))"

   open (1, file = 'scalar102a.1', form='formatted', access='sequential' )

   allocate ( b1, source = child(-999,'xxx') )
   allocate ( c1, source = child(-999,'xxx') )
   allocate ( c2, source = child(-999,'xxx') )

10 format (DT'b1')

   idx = 1

   read ( 1, 10, iostat = stat, iomsg = msg )                b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, fmt, iostat = stat, iomsg = msg )    c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read ( 1, "(DT'c2'(10,5))", iostat = stat, iomsg = msg )  c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   
   print *, rbuffer
   
   select type ( b1 )
      type is ( child )
         if ( ( b1%i /= 200 ) .or. ( b1%c /= 'abc' ) )  error stop 4_4
   end select

   if ( ( c1%i /= 300 ) .or. ( c1%c /= 'def' ) )        error stop 5_4
   if ( ( c2%i /= 400 ) .or. ( c2%c /= 'ghi' ) )        error stop 6_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, rbuffer, idx

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt
   character(3) :: tmp
   
   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   select type ( dtv )
      type is (child)
         if ( size(v_list) == 0 ) then
            read ( unit, *, iostat = iostat )      dtv%i, tmp
            call dtv%setc(tmp)
         else if ( size(v_list) == 1 ) then
            write ( fmt, * ) '(I', v_list(1),')'
            read ( unit, fmt, iostat = iostat )    dtv%i
            read ( unit, *, iostat = iostat )      tmp
            call dtv%setc(tmp)
         else if ( size(v_list) == 2 ) then
            write ( fmt, * ) '(I', v_list(1),',A',v_list(2),')'
            read ( unit, fmt, iostat = iostat )    dtv%i, tmp
            call dtv%setc(tmp)
         end if
   end select

   iomsg = 'dtioread'

end subroutine
