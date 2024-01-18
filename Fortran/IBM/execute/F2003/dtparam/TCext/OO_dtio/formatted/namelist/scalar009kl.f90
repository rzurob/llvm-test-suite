! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar009kl
!*
!*  DATE                       : 2007-07-08 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting implicit scalar objects (Output)
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

   type :: base (kb,lb)
      integer, kind :: kb
      integer, len :: lb
      character(lb) ::  c
      integer(kb)   ::  i
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4,*)), intent(in) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   implicit type(base(4,3))  (A-M) ! tcx: (4,3)
   implicit class(base(4,3)) (N-Z) ! tcx: (4,3)

   namelist /nml1/ b1, b2
   namelist /nml2/ z3, z4
   allocatable :: z3
   pointer     :: z4

end module

program scalar009kl
   use m
   implicit none

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'scalar009kl.1', form='formatted', access='stream' )

   b1 = base(4,3)(c='abc',i=1) ! tcx: (4,3)
   b2 = base(4,3)(c='def',i=2) ! tcx: (4,3)
   allocate(z3, source = base(4,3)(c='ghi',i=3) ) ! tcx: (4,3)
   allocate(z4, source = base(4,3)(c='jkl',i=4) ) ! tcx: (4,3)

   write (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4,*)), intent(in) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   write (unit, "('i= ',I4,1X, 'c= ', A3, 1X)", iostat=iostat )        dtv%i, dtv%c

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 8 changes
