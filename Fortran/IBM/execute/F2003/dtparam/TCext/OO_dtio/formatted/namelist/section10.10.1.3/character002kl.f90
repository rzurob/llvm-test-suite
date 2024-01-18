! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : character002kl
!*
!*  PROGRAMMER                 : David Forster (derived from character002 by Robert Ma)
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
!*                                        if not enough characters are supplied by input data,
!*                                        the remaining date shall be filled with blanks
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

   type base (lb1,lb2) ! lb1,lb2=7,3
      integer, len :: lb1,lb2
      character(lb1)   :: c(lb2) = 'xxxxxxx' ! (/ 'xxxxxxx', 'xxxxxxx', 'xxxxxxx' /)
   end type

   type nodtio (ln1,ln2) ! ln1,ln2=7,3
      integer, len :: ln1,ln2
      character(ln1)   :: c(ln2) = 'xxxxxxx' ! (/ 'xxxxxxx', 'xxxxxxx', 'xxxxxxx' /)
   end type

end module

program character002kl
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
   class(base(:,:)), pointer      :: b3 ! tcx: (:,:)
   type(nodtio(:,:)), allocatable :: b4 ! tcx: (:,:)

   namelist /n1/ b1, b2
   namelist /n2/ b3, b4

   allocate (base(7,3) :: b1, b3) ! tcx: base(7,3)
   allocate (nodtio(7,3) :: b2, b4) ! tcx: nodtio(7,3)

   open (1, file='character002kl.1', form='formatted', access='sequential' )

   read (1, n1, iostat = stat, iomsg = msg)
   read (1, n2, iostat = stat, iomsg = msg)

   if ( ( b1%c(1) /= 'abcdefg' )  .or. ( b1%c(2) /= 'ABCDE  ' ) .or. ( b1%c(3) /= 'xxxxxxx' ) ) error stop 1_4
   if ( ( b2%c(1) /= 'ABCDEFG' )  .or. ( b2%c(2) /= 'abc    ' ) .or. ( b2%c(3) /= 'xxxxxxx' ) ) error stop 2_4
   if ( ( b3%c(1) /= 'abcdefg' )  .or. ( b3%c(2) /= 'ABCDE  ' ) .or. ( b3%c(3) /= 'xxxxxxx' ) ) error stop 3_4
   if ( ( b4%c(1) /= 'ABCDEFG' )  .or. ( b4%c(2) /= 'abc    ' ) .or. ( b4%c(3) /= 'xxxxxxx' ) ) error stop 4_4
end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(*,*)), intent(inout) :: dtv ! tcx: (*,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   type(base(7,3))           :: dummy ! tcx: (7,3)
   namelist /dtio/ dummy

   if ( iotype /= 'NAMELIST' ) error stop 3_4
   if ( size(v_list,1) /= 0 )  error stop 4_4

   read( unit, dtio, iostat = iostat)

   dtv%c = dummy%c

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb1,lb2) to invoke with (7,3) / declare with (*,*) - 5 changes
! type: nodtio - added parameters (ln1,ln2) to invoke with (7,3) / declare with (*,*) - 2 changes
