!*  ===================================================================
!*
!*  DATE                       : 2007-06-08 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        When editors in format specification are used up and there are
!*                                        still list item, runtime should revert to appropriate location of the format
!*                                        specification (with parentheses inside the format statement) (read)
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
      integer(kb)   :: i = -9
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc)   :: j = -99
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

   character(10) :: rbuffer(6)
   integer(4)    :: idx

end module

program fdtedit109a2kl
use m

   class(base(4)), allocatable  :: b1
   class(base(4)), pointer      :: b2
   type(base(4))                :: b3
   class(child(4,4)), allocatable :: c1
   class(child(4,4)), pointer     :: c2
   type(child(4,4))               :: c3

   integer :: stat
   character(150) :: msg

   character(30) :: fmt1 = "(DT'_1'(1),2(DT'_2'(2)))"

   ! allocation of variables

   allocate ( b1, source = base(4)( ) )
   allocate ( b2, source = child(4,4)( ) )
   b3 = base(4)( )

   allocate ( c1, source = child(4,4)( ) )
   allocate ( c2, source = child(4,4)( ) )
   c3 = child(4,4)( )

   open (1, file = 'fdtedit109a2kl.1', form='formatted', access='sequential' )

   idx = 1
   read ( 1, fmt1, iostat = stat, iomsg = msg )      b1, b2, b3, c1, c2, c3

   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )   error stop 1_4

   if ( b1%i /= 101 )                                error stop 2_4

   select type ( b2 )
      type is (child(4,4))
         if ( ( b2%i /= 102 ) .or. ( b2%j /= 103 ) ) error stop 3_4
   end select

   if ( ( b3%i /= 104 )                      )       error stop 4_4

   if ( ( c1%i /= 201 ) .or. ( c1%j /= 202 ) )       error stop 5_4
   if ( ( c2%i /= 203 ) .or. ( c2%j /= 204 ) )       error stop 6_4
   if ( ( c3%i /= 205 ) .or. ( c3%j /= 206 ) )       error stop 7_4

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

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   select type ( dtv )
      type is ( base(4) )
         read ( unit, *, iostat = iostat ) dtv%i
      type is ( child(4,4) )
         read ( unit, *, iostat = iostat ) dtv%i, dtv%j
   end select

   iomsg = 'dtioread'

end subroutine
