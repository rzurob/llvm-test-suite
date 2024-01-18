! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar007kl
!*
!*  PROGRAMMER                 : David Forster (derived from scalar007 by Robert Ma)
!*  DATE                       : 2007-07-08 (original: 11/08/2004)
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
!*                                        Try namelist formatting with class hierarchy (Output)
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

   type :: A (lA)
      integer, len :: lA
      character(lA) :: a1
   end type

   type, extends(A) :: B (kB)
      integer, kind :: kB
      integer(kB) :: b1
   end type

   type, extends(B) :: C (kC)
      integer, kind :: kC
      logical(kC), pointer    :: c1 => null()
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import A
         class(A(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program scalar007kl
   use m

   integer :: stat
   character(200) :: msg = ''

   class(A(:)), allocatable :: b1 ! tcx: (:)
   class(A(:)), pointer     :: b2 ! tcx: (:)
   logical, target :: g = .true.

   namelist /nml1/ b1
   namelist /nml1/ b2
   namelist /nml2/ b2

   open (1, file = 'scalar007kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = A(3)('ibm') ) ! tcx: (3)
   allocate ( b2, source = B(3,4)(a1='ibm',b1=888) ) ! tcx: (3,4)

   write (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   allocate ( b2, source = C(3,4,4)('ibm',888, g ) ) ! tcx: (3,4,4)
   write (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: A, B, C

   class(A(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   select type (dtv)
      type is (A(*)) ! tcx: (*)
         write (unit, "('a= ',A3,1X)", iostat=iostat )                                   dtv%a1
      type is (B(*,4)) ! tcx: (*,4)
         write (unit, "('a=',A3,1X, 'b=', I4, 1X)", iostat=iostat )                      dtv%a1, dtv%b1
      type is (C(*,4,4)) ! tcx: (*,4,4)
         write (unit, "('a=',A3,1X, 'b=', I4, 1X, 'c=', L4, 1X)", iostat=iostat )        dtv%a1, dtv%b1, dtv%c1
   end select

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: A - added parameters (lA) to invoke with (3) / declare with (*) - 6 changes
! type: B - added parameters (kB) to invoke with (4,3) / declare with (4,*) - 2 changes
! type: B - added parameters (kB) to invoke with (3,4) / declare with (*,4) - 2 changes
! type: C - added parameters (kC) to invoke with (3,4,4) / declare with (*,4,4) - 2 changes
