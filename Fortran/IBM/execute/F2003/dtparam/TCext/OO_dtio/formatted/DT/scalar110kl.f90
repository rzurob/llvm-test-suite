!*  ===================================================================
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        io-implied-do with scalar derived type variable (read)
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

   type, extends( base ) :: child (kc)
      integer, kind :: kc
      integer(kc) :: j
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

   integer :: idx
   character(20) :: rbuffer(10)

end module

program scalar110kl
use m

   class(base(4)), allocatable :: b1
   type(base(4)), pointer     :: b2
   type(child(4,4))              :: b3 = child(4,4)(-999,-999)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT)"

   open (1, file = 'scalar110kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = child(4,4)(-999,-999) )
   allocate ( b2, source = base(4)(-999) )

10 format (DT'b2')

   idx = 1

   read ( 1, fmt, iostat = stat, iomsg = msg )              (b1, i = 1,3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   select type ( b1 )
      type is (child(4,4))
         if ( ( b1%i /= 100 ) .or. ( b1%j /= 101 ) )  error stop 2_4
   end select

   read ( 1, 10, iostat = stat, iomsg = msg )               (b2, i = 1,3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   if ( b2%i /= 200 )  error stop 4_4

   read ( 1, "(DT'b3-1'(10),DT'b3-2'(20))", iostat = stat, iomsg = msg )    (b3, i= 10,13)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 5_4
   if ( ( b3%i /= 300 ) .or. ( b3%j /= 301 ) )  error stop 6_4

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

   character(20) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   select type ( dtv )
      type is (base(4))
         read ( unit, '(I4)', iostat = iostat )    dtv%i
      type is (child(4,4))
         read ( unit, '(I4,I4)', iostat = iostat )    dtv%i, dtv%j
   end select

   iomsg = 'dtioread'

end subroutine

