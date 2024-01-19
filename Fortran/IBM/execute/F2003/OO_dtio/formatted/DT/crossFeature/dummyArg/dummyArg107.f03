!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Argument Association: assumed shape array
!*                                        unlimited polymorphic dummy argument (read)
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

   integer :: stat
   character(150) :: msg
   character(20) :: rbuffer(44)
   integer :: idx

   contains

      subroutine foo ( dtv )
         class(*), intent(inout) :: dtv(:)
         character(51) :: fmt = ''
         fmt = "(DT'_foo1'(5,6),/,DT'_foo2'(6,7),/, DT'_foo3'(7,8))"
         select type ( dtv )
            class is ( base )
               read (1, fmt, iostat = stat, iomsg = msg ) dtv
         end select
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv )
         class(*), allocatable, intent(inout) :: dtv(:)
      10 format (DT'_bar1'(4,5),/,DT'_bar2'(5,6),/,DT'_bar3'(6,7))

         select type ( dtv )
            type is ( base )
               read (1, 10, iostat = stat, iomsg = msg ) dtv
            type is ( child )
               read (1, "(DT'_bar4'(5,6),/,DT'_bar5'(6,7),/, DT'_bar6'(7,8))", iostat = stat, iomsg = msg ) dtv
         end select

         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 2_4

      end subroutine

      subroutine boo ( dtv )
         class(*), pointer, intent(inout) :: dtv(:)
      10 format (DT'_boo1'(4,5),/,DT'_boo2'(5,6),/,DT'_boo3'(6,7))

         select type ( g => dtv )
            class is ( base )
               read (1, 10, iostat = stat, iomsg = msg ) g
         end select
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 3_4

      end subroutine

end module

program dummyArg107
use m

   type(base), allocatable  :: b1(:)
   type(base)               :: b2(3)
   class(base), pointer     :: b3(:)

   type(child), allocatable :: c1(:)
   type(child)              :: c2(3)
   class(child), pointer    :: c3(:)

   class(*), allocatable :: u1(:)
   class(*), pointer     :: u2(:)

   open (1, file = 'dummyArg107.1', form='formatted', access='sequential' )

   allocate ( b1(2) )
   allocate ( child :: b3(4) )

   allocate ( c1(4) )
   allocate ( c3(4:6) )
   idx = 1
   call foo ( b1 )
   call foo ( b2 )
   call foo ( b3 )
   call foo ( c1 )
   call foo ( c2 )
   call foo ( c3 )

   print *, b1%i
   print *, b2%i
   select type ( b3 )
      type is ( child )
         print *, b3%i
         print *, b3%j
   end select

   print *, c1%i
   print *, c1%j
   print *, c2%i
   print *, c2%j
   print *, c3%i
   print *, c3%j

   allocate ( base :: u1(5) )

   call foo ( u1 )
   select type ( u1 )
      type is ( base )
         print *, u1%i
   end select

   call bar ( u1 )

   select type ( u1 )
      type is ( base )
         print *, u1%i
   end select

   deallocate ( u1 )

   allocate ( child :: u1(3) )
   call bar ( u1 )
   select type ( u1 )
      type is ( child )
         print *, u1%i
         print *, u1%j
   end select

   allocate ( child :: u2(7) )
   call boo ( u2 )
   select type ( u2 )
      type is ( child )
         print *, u2%i
         print *, u2%j
   end select

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, rbuffer, idx

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   select type ( dtv )
      type is ( base )
         write ( fmt, * ) '(I', v_list(1),')'
         read ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child )
         write ( fmt, * ) '(I', v_list(1),', I', v_list(2),')'
         read ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
   end select
   iomsg = 'dtioread'

end subroutine
