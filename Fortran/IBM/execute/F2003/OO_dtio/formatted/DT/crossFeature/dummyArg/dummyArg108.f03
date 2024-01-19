!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Argument Association: assumed size array
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
   character(20) :: rbuffer(32)
   integer :: idx

   contains

      subroutine foo ( dtv )
         class(*), intent(inout) :: dtv(2,*)
         character(88) :: fmt = ''
         fmt = "(DT'_foo1'(5,6),/,DT'_foo2'(6,7),/, DT'_foo3'(7,8),/, DT'_foo4'(8,9),/, DT'bad'(9,10))"
         select type ( dtv )
            class is ( base )
               read (1, fmt, iostat = stat, iomsg = msg ) dtv(1:2,1), dtv(1:2,2)
         end select
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 1_4

      end subroutine

end module

program dummyArg108
use m

   type(base), allocatable  :: b1(:)
   type(base)               :: b2(4)
   class(base), pointer     :: b3(:)

   type(child), allocatable :: c1(:,:)
   type(child)              :: c2(4)
   class(child), pointer    :: c3(:)

   class(*), allocatable :: u1(:)
   class(*), pointer     :: u2(:)

   open (1, file = 'dummyArg108.1', form='formatted', access='sequential' )

   allocate ( b1(4) )
   allocate ( b3(4) )

   allocate ( c1(2,2) )
   allocate ( c3(4:7) )

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

   allocate ( base :: u1(4) )

   call foo ( u1 )
   select type ( u1 )
      type is ( base )
         print *, u1%i
   end select

   allocate ( child :: u2(4) )
   call foo ( u2 )

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
