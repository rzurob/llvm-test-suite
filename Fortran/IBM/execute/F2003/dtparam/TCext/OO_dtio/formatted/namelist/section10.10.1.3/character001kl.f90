! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : character001kl
!*
!*  PROGRAMMER                 : David Forster (derived from character001 by Robert Ma)
!*  DATE                       : 2007-07-20 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.3 Namelist group object list items
!*                                        try end of record in the middle of character types
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
   type base (lb1,lb2) ! lb1,lb2=3,3
      integer, len :: lb1,lb2
      character(lb1)   :: c(lb2) = 'xxx' ! (/ 'xxx', 'xxx', 'xxx' /)
   end type
   type nodtio (ln1,ln2) ! ln1,ln2=3,3
      integer, len :: ln1,ln2
      character(ln1)   :: c(ln2) = 'xxx' ! (/ 'xxx', 'xxx', 'xxx' /)
   end type
end module

program character001kl
   use m

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(*,*)), intent(inout) :: dtv ! tcx: (*,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(150) :: msg = ''

   class(base(:,:)), allocatable  :: b1 ! tcx: (:,:)
   type(nodtio(:,:)), pointer     :: b2 ! tcx: (:,:)
   character(4)              :: c(2)
   class(base(:,:)), pointer      :: b3 ! tcx: (:,:)
   type(nodtio(:,:)), allocatable :: b4 ! tcx: (:,:)

   namelist /n1/ b1, b2, c
   namelist /n2/ b3, b4

   allocate (base(3,3):: b1, b3) ! tcx: base(3,3)
   allocate (nodtio(3,3):: b2, b4) ! tcx: nodtio(3,3)

   open (1, file='character001kl.1', form='formatted', access='sequential' )

   read (1, n1, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   read (1, n2, iostat = stat, iomsg = msg)

   if ( ( b1%c(1) /= 'abc' )  .or. ( b1%c(2) /= 'def' ) .or. ( b1%c(3) /= '/gh' ) ) error stop 1_4
   if ( ( b2%c(1) /= 'abc' )  .or. ( b2%c(2) /= 'd  ' ) .or. ( b2%c(3) /= 'gh ' ) ) error stop 2_4
   if ( ( c(1) /= 'ibm1' ) .or. ( c(2) /= 'fort' ) ) error stop 3_4
   if ( ( b3%c(1) /= 'IBM' )  .or. ( b3%c(2) /= 'xxx' ) .or. ( b3%c(3) /= '   ' ) ) error stop 4_4
   if ( ( b4%c(1) /= '   ' )  .or. ( b4%c(2) /= '   ' ) .or. ( b4%c(3) /= 'abc' ) ) error stop 5_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(*,*)), intent(inout) :: dtv ! tcx: (*,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   type(base(3,3))           :: dummy ! tcx: (3,3)
   namelist /dtio/ dummy

   if ( iotype /= 'NAMELIST' ) error stop 3_4
   if ( size(v_list,1) /= 0 )  error stop 4_4

   read( unit, dtio, iostat = iostat)

   dtv%c = dummy%c

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb1,lb2) to invoke with (3,3) / declare with (*,*) - 5 changes
! type: nodtio - added parameters (ln1,ln2) to invoke with (3,3) / declare with (*,*) - 2 changes
