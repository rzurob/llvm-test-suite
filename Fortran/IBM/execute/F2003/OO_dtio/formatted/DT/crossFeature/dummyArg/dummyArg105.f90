!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Argument Association: assumed size array dummy argument (read)
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
   character(20) :: rbuffer(36)
   integer :: idx
   contains

      subroutine foo ( dtv )
         type(base), intent(inout) :: dtv(2,*)
         character(27) :: fmt

         fmt = "(DT'_foo'(5),/,DT'_foo'(6))"

         read (1, fmt, iostat = stat, iomsg = msg ) dtv(1:2,1:2)

         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv )
         class(base), intent(inout) :: dtv(3:4,*)
      10 format (DT'_bar'(4,5),/,DT'_bar'(5,6),/,DT'_bar'(6,7),/,DT'_bar'(7,8))

         read (1, 10, iostat = stat, iomsg = msg ) dtv(3:4,1:2)

         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 2_4

      end subroutine

end module

program dummyArg105
use m

   type(base), allocatable  :: b1(:,:)
   type(base)               :: b2(4)
   class(base), pointer     :: b3(:,:)

   type(child), allocatable :: c1(:,:)
   type(child)              :: c2(3:6)
   class(child), pointer    :: c3(:)

   open (1, file = 'dummyArg105.1', form='formatted', access='sequential' )

   allocate ( b1(2,2) )
   allocate ( child :: b3(2,2) )

   allocate ( c1(2,2) )
   allocate ( c3(4:7) )

   idx = 1

   call foo ( b1 )
   call foo ( b2(1:4) )
   call foo ( b3 )

   print *, b1%i
   print *, b2%i
   select type ( b3 )
      type is ( child )
         print *, b3%i
         print *, b3%j
   end select

   call bar ( b1 )
   call bar ( b2 )
   call bar ( b3 )

   print *, b1%i
   print *, b2%i
   select type ( b3 )
      type is ( child )
         print *, b3%i
         print *, b3%j
   end select

   call bar ( c1(1:2,1:2) )
   call bar ( c2(3:6) )
   call bar ( c3(4:7) )

   print *, c1%i
   print *, c1%j
   print *, c2%i
   print *, c2%j
   print *, c3%i
   print *, c3%j

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, idx, rbuffer

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
