!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg104akl
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Argument Association: assumed shape
!*                                        array dummy argument with pointer/allocatable attr. (read)
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

   type base (kb)
      integer, kind :: kb
      integer(kb) :: i
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: j = -999
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(150) :: msg
   character(20) :: rbuffer(11)
   integer :: idx

   contains

      subroutine foo ( dtv )
         type(base(4)), intent(inout), allocatable :: dtv(:)
         character(63) :: fmt = "(DT'_foo'(5),/,DT'_foo'(6),/, DT'_foo'(7))"

         read (1, fmt, iostat = stat, iomsg = msg ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 1_4

      end subroutine

      subroutine bar ( dtv )
         class(base(4)), intent(inout), pointer :: dtv(:)
      10 format (DT'_bar'(4,5),/,DT'_bar'(5,6),/,DT'_bar'(6,7))

         read (1, 10, iostat = stat, iomsg = msg ) dtv

         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 2_4

      end subroutine

end module

program dummyArg104akl
use m

   type(base(4)), allocatable  :: b1(:)
   type(base(4)), allocatable, target :: b2(:)
   class(base(4)), pointer     :: b3(:)

   open (1, file = 'dummyArg104akl.1', form='formatted', access='sequential' )

   allocate ( b1(2), source = (/ base(4)(101), base(4)(102) /) )
   allocate ( b2(4), source = (/ base(4)(121), base(4)(122), base(4)(123), base(4)(124) /) )
   idx =1
   call foo ( b1 )
   call foo ( b2 )

   print *, b1%i
   print *, b2%i

   b3 => b2(1:4:3)

   call bar ( b3 )

   print *, b2%i
   print *, b3%i

   allocate ( b3(3), source = (/ child(4,4)(131, 132), child(4,4)(133, 134), child(4,4)(135, 136) /) )
   call bar ( b3 )

   select type ( b3 )
      type is ( child(4,4) )
         print *, b3%i
         print *, b3%j
   end select
   print *, rbuffer
end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, rbuffer, idx

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt
   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   select type ( dtv )
      type is ( base(4) )
         write ( fmt, * ) '(I', v_list(1),')'
         read ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child(4,4) )
         write ( fmt, * ) '(I', v_list(1),', I', v_list(2),')'
         read ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
   end select
   iomsg = 'dtioread'

end subroutine
