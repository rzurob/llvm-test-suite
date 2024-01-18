! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : slash002kl
!*
!*  DATE                       : 2007-07-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.2 Namelist Input Values
!*                                        When a slash is encountered, input statement should terminate.
!*                                        and any input data after the slash does NOT have to conform to rules of namelist input values
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
   type base (kb,lb1,lb2,lb3) ! kb,lb1,lb2,lb3=4,3,4,2
      integer, kind :: kb
      integer, len :: lb1,lb2,lb3
      integer(kb)   :: i(lb1) = -9 ! (/ -9, -9, -9 /)
      character(lb2) :: c(lb3)
   end type

end module

program slash002kl
   use m

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4,*,*,*)), intent(inout) :: dtv ! tcx: (4,*,*,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(150) :: msg = ''

   class(base(4,:,:,:)), allocatable  :: b1 ! tcx: (4,:,:,:)
   class(base(4,:,:,:)), pointer      :: b2 ! tcx: (4,:,:,:)

   namelist /nml/ b1, b2
   allocate(base(4,3,4,2):: b1, b2) ! tcx: base(4,3,4,2)

   open (1, file='slash002kl.1', form='formatted', access='sequential', blank='zero' )

   read (1, nml, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   if ( ( b1%i(1) /= 101 ) .or. ( b1%i(2) /= 102 ) .or. ( b1%i(3) /= 103 ) .or. ( b1%c(1) /= 'abcd' ) .or. ( b1%c(2) /= 'ABCD' ) ) error stop 2_4
   if ( ( b2%i(1) /= 201 ) .or. ( b2%i(2) /= 202 ) .or. ( b2%i(3) /= 203 ) .or. ( b2%c(1) /= 'efgh' ) .or. ( b2%c(2) /= 'EFGH' ) ) error stop 3_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(4,*,*,*)), intent(inout) :: dtv ! tcx: (4,*,*,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer(4) :: i(3)
   namelist /dtio/ i

   if ( iotype /= 'NAMELIST' )    error stop 4_4
   if ( size(v_list,1) /= 0 )     error stop 5_4

   read( unit, dtio, iostat = iostat )
   if ( iostat /= 0 )             error stop 6_4
   read( unit, "(A4,/,A4,/)", iostat = iostat )    dtv%c

   dtv%i = i

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb1,lb2,lb3) to invoke with (4,3,4,2) / declare with (4,*,*,*) - 4 changes
