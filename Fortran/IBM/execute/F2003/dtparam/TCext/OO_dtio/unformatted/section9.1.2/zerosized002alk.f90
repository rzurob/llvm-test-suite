! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : zerosized002alk
!*
!*  PROGRAMMER                 : David Forster (derived from zerosized002a by Robert Ma)
!*  DATE                       : 2007-09-10 (original: 11/04/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Ensure zero-sized type will invoke DTIO for array(sequential access read)
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
    type :: base (lbase_1) ! lbase_1=0
       integer, len :: lbase_1
       character(lbase_1) :: i
    end type

    type :: emptybase (keb) ! keb=1
       integer, kind :: keb
    end type

    type :: nonemptybase (knonemptybase_1) ! knonemptybase_1=1
       integer, kind :: knonemptybase_1
       character(knonemptybase_1) :: c = 'c'
    end type

end module

program zerosized002alk
use m

   interface read(unformatted)

      subroutine unformattedRead (dtv, unit, iostat, iomsg)
      use m
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine

      subroutine unformattedReadZero (dtv, unit, iostat, iomsg)
      use m
         class(emptybase(1)), intent(inout) :: dtv ! tcx: (1)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine

      subroutine unformattedReadNonEmpty (dtv, unit, iostat, iomsg)
      use m
         class(nonemptybase(1)), intent(inout) :: dtv ! tcx: (1)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine

   end interface

   ! declaration of variables

   integer :: stat

   class(base(:)), allocatable :: b1(:) ! tcx: (:)
   class(base(:)), pointer     :: b2(:,:) ! tcx: (:)
   type (base(0))              :: b3(2) ! tcx: (0)

   class(emptybase(1)), allocatable :: e1(:,:) ! tcx: (1)
   class(emptybase(1)), pointer     :: e2(:) ! tcx: (1)
   type (emptybase(1))              :: e3(2) ! tcx: (1)

   class(nonemptybase(1)), allocatable :: n1(:) ! tcx: (1)
   class(nonemptybase(1)), pointer     :: n2(:,:) ! tcx: (1)
   type (nonemptybase(1))              :: n3(0) ! tcx: (1)

   ! allocation of variables

   allocate (base(0):: b1(3), b2(3,3) ) ! tcx: base(0)
   allocate ( e1(2,2), e2(5) )
   allocate (nonemptybase(1):: n1(0), n2(0,0) ) ! tcx: nonemptybase(1)

   open(1, file='zerosized002alk.data', access='sequential', form='unformatted')

   ! write some arbitrary records to file, so end of file will not be reached.
   write (1, iostat = stat) "a"
   write (1, iostat = stat) "b"
   write (1, iostat = stat) "c"
   write (1, iostat = stat) "d"
   write (1, iostat = stat) "e"
   write (1, iostat = stat) "f"
   write (1, iostat = stat) "g"
   write (1, iostat = stat) "h"
   write (1, iostat = stat) "i"

   rewind 1

   read (1, iostat = stat)  b1
   if ( stat /= 999 ) error stop 101_4
   read (1, iostat = stat)  e1
   if ( stat /= 998 ) error stop 2_4
   read (1, iostat = stat)  n1                !<- shall not call DTIO since no effective items
   if ( stat /= 0 )   error stop 3_4

   read (1, iostat = stat)  b2
   if ( stat /= 999 ) error stop 4_4
   read (1, iostat = stat)  e2
   if ( stat /= 998 ) error stop 5_4
   read (1, iostat = stat)  n2                !<- shall not call DTIO since no effective items
   if ( stat /= 0 )   error stop 6_4

   read (1, iostat = stat)  b3
   if ( stat /= 999 ) error stop 7_4
   read (1, iostat = stat)  e3
   if ( stat /= 998 ) error stop 8_4
   read (1, iostat = stat)  n3                !<- shall not call DTIO since no effective items
   if ( stat /= 0 )   error stop 9_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iomsg=iomsg ) dtv%i

   iostat = 999    ! change to iostat so that we know DTIO is called

end subroutine

subroutine unformattedReadZero (dtv, unit, iostat, iomsg)
use m
   class(emptybase(1)), intent(inout) :: dtv ! tcx: (1)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   iostat = 998    ! change to iostat so that we know DTIO is called

end subroutine

subroutine unformattedReadNonEmpty (dtv, unit, iostat, iomsg)
use m
   class(nonemptybase(1)), intent(inout) :: dtv ! tcx: (1)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iomsg=iomsg ) dtv%c

   iostat = 997

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (0) / declare with (*) - 5 changes
! type: emptybase - added parameters (keb) to invoke with (1) / declare with (1) - 5 changes
! type: nonemptybase - added parameters (knonemptybase_1) to invoke with (1) / declare with (1) - 5 changes
