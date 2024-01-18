!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : structCompnt107kl
!*
!*  PROGRAMMER                 : David Forster (derived from structCompnt107 by Robert Ma)
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
!*                                        Structure Component: Array Sequence Derived Type Component (read)
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
      sequence
      character(lb) :: c = 'xxx'
      character(lb) :: cc = 'xxx'
   end type

   type container (lc)
      integer, len :: lc
      sequence
      type(base(lc)), allocatable :: b1(:)
   end type

   integer :: stat
   character(150) :: msg
   character(20) :: rbuffer(8)
   integer(4) :: idx

end module

program structCompnt107kl
use m

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         type(base(*)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   type(container(3)) :: c1
   type(container(:)), allocatable :: c2

   allocate( c1%b1(4)  )
   allocate( c2, source = container(3)( (/ base(3)('',''), base(3)('',''), base(3)('',''), base(3)('','') /) ) )

   open (1, file = 'structCompnt107kl.1', form='formatted', access='sequential' )
   idx =1 
   read ( 1, "(DT'_con1-1'(4,5),/,DT'_con1-2'(5,6),/,DT'_con1-3'(6,7),/,DT'_con1-4'(7,8))", iostat = stat, iomsg = msg )       c1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, "(4(DT'_con2'(4,5),:,/))", iostat = stat, iomsg = msg )       c2%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4


   print *, c1%b1%c
   print *, c1%b1%cc
   print *, c2%b1%c
   print *, c2%b1%cc

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, rbuffer, idx

   type(base(*)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   write ( fmt, "(A2,I1,A2,I1,A1)" ) '(A', v_list(1),',A',v_list(2),')'
   read ( unit, fmt, iostat = iostat )    dtv%c, dtv%cc

   iomsg = 'dtioread'

end subroutine
