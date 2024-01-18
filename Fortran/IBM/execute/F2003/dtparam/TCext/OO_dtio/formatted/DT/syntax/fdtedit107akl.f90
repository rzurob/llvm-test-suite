!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtedit107akl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtedit107a by Robert Ma)
!*  DATE                       : 2007-06-08 (original: 21/03/2005)
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
!*                                        multiple DT edit descriptor in an I/O statement (read)
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
      integer(kb) :: i = -1
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: j = -2
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

   character(20) :: rbuffer(4)

end module

program fdtedit107akl
use m

   class(base(4)), allocatable :: b1
   type(base(4))               :: b2 = base(4)(200)
   class(child(4,4)), pointer    :: c1
   type(child(4,4))              :: c2 = child(4,4)(30,40)

   integer :: stat
   character(150) :: msg
   character(39) :: fmt = "(DT(1),DT'b2'(2),DT'c1'(3),DT'c2'(4,5))"
   open (1, file = 'fdtedit107akl.1', form='formatted', access='sequential' )

   allocate ( b1, source = child(4,4)() )
   allocate ( c1, source = child(4,4)(10,20) )
   read ( 1, fmt, iostat = stat, iomsg = msg )       b1,b2,c1,c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )  error stop 1_4

   select type ( b1 )
      type is (child(4,4))
         if ( ( b1%i /= 101 ) .or. ( b1%j /= 102 ) ) error stop 2_4
   end select

   if ( ( b2%i /= 103 )                      )       error stop 3_4
   if ( ( c1%i /= 104 ) .or. ( c1%j /= 105 ) )       error stop 4_4
   if ( ( c2%i /= 106 ) .or. ( c2%j /= 107 ) )       error stop 5_4

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, rbuffer

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( rbuffer(v_list(1)), * ) iotype, v_list

   select type ( dtv )
      type is ( base(4) )
         read ( unit, *, iostat = iostat, iomsg = iomsg ) dtv%i
      type is ( child(4,4) )
         read ( unit, *, iostat = iostat, iomsg = iomsg ) dtv%i, dtv%j
   end select

   iomsg = 'dtiowrite'

end subroutine
