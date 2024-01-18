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
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Character output with namelist formatting on external files
!*                                        character sequences are not separated by value separators
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

   type base (lb1,lb2) ! lb1,lb2=2,3
      integer, len :: lb1,lb2
      character(lb1) :: c1(lb2)
   end type

   type, extends(base) :: child (lc1,lc2) ! lc1,lc2=2,3
      integer, len :: lc1,lc2
      character(lc1) :: clc1(lc2)
   end type

end module

module m1
   use m
   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(*,*)), intent(in) :: dtv ! tcx: (*,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program character002kl
   use m1

   integer :: stat
   character(200) :: msg = ''
   class(base(:,:)), allocatable  :: b1 ! tcx: (:,:)
   class(base(:,:)), pointer      :: b2 ! tcx: (:,:)

   namelist /nml1/ b1, b2

   open (1, file='character002kl.1', form='formatted', access='sequential', blank='zero' )

   allocate( b1, source = child(2,3,2,3)((/'ab','cd','ef'/),(/'AB','CD','EF'/)) ) ! tcx: (2,3,2,3)
   allocate( b2, source = child(2,3,2,3)((/'gh','ij','kl'/),(/'GH','IJ','KL'/)) ) ! tcx: (2,3,2,3)

   write (1, NML=nml1, iostat=stat, iomsg=msg)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(*,*)), intent(in) :: dtv ! tcx: (*,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   type(base(2,3))  :: dumb ! tcx: (2,3)
   type(child(2,3,2,3)) :: dumc ! tcx: (2,3,2,3)

   namelist /b/  dumb
   namelist /c/  dumc

   if ( iotype /= "NAMELIST" ) error stop 9_4
   if ( size(v_list, 1) /= 0 ) error stop 10_4

   select type ( dtv )
      type is (base(*,*)) ! tcx: (*,*)
         dumb=dtv
         write ( unit, b, iostat = iostat )
      type is (child(*,*,*,*)) ! tcx: (*,*,*,*)
         dumc=dtv
         write ( unit, c, iostat = iostat )
   end select

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb1,lb2) to invoke with (2,3) / declare with (*,*) - 6 changes
! type: child - added parameters (lc1,lc2) to invoke with (2,3,2,3) / declare with (*,*,*,*) - 4 changes
