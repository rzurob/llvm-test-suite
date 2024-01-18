!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : structCompnt101kl
!*
!*  PROGRAMMER                 : David Forster (derived from structCompnt101 by Robert Ma)
!*  DATE                       : 2007-06-07 (original: 21/03/2005)
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
!*                                        Structure Component: Scalar Non-polymorphic Derived Type Component (read)
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

   type base (lb)
      integer, len :: lb
      character(lb) :: c = 'xxx'
   end type

   type, extends(base) :: child (lc)
      integer, len :: lc
      character(lc) :: cc = 'xxx'
   end type

   type container1 (lc1)
      integer, len :: lc1
      type(base(lc1)) :: b1
   end type

   type container2 (lc2)
      integer, len :: lc2
      type(base(lc2)), pointer :: b2
   end type

   type container3 (lc3a,lc3b)
      integer, len :: lc3a,lc3b
      type(child(lc3a,lc3b)), allocatable :: b3
   end type

   integer :: stat
   character(150) :: msg
   character(20) :: rbuffer(4)
   integer(4) :: idx

end module

program structCompnt101kl
use m

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(*)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   type(container1(3))               :: c1
   class(container2(:)), allocatable :: c2
   class(container3(:,:)), pointer     :: c3

   c1 = container1(3)(base(3)(''))
   allocate( c2, source = container2(3)(null()))
   allocate( c2%b2, source = base(3)('') )

   allocate( c3, source = container3(3,3)(child(3,3)('','')))

   open (1, file = 'structCompnt101kl.1', form='formatted', access='sequential' )
   idx = 1
   read ( 1, "(DT'_con1'(4))", iostat = stat, iomsg = msg )       c1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, "(DT'_con2'(5))", iostat = stat, iomsg = msg )       c2%b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read ( 1, "(DT'_con3'(6,7),DT'_con3base'(8))", iostat = stat, iomsg = msg )     c3%b3, c3%b3%base
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   print *, rbuffer
   
   if ( c1%b1%c /= 'ABC' ) error stop 4_4
   if ( c2%b2%c /= 'DEF' ) error stop 5_4
   if ( ( c3%b3%c /= 'MNO' ) .or. ( c3%b3%cc /= 'JKL' ) )error stop 6_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, rbuffer, idx

   class(base(*)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   select type ( dtv )
      type is ( base(*) )
         write ( fmt, "(A2,I1,A1)" ) '(A', v_list(1),')'
         read ( unit, fmt, iostat = iostat )    dtv%c
      type is ( child(*,*) )
         write ( fmt, "(A2,I1,A2,I1,A1)" ) '(A', v_list(1),',A',v_list(2),')'
         read ( unit, fmt, iostat = iostat )    dtv%c, dtv%cc
   end select
   iomsg = 'dtioread'

end subroutine
