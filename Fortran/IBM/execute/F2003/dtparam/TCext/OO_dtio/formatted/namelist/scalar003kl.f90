! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar003kl
!*
!*  PROGRAMMER                 : David Forster (derived from scalar003 by Robert Ma)
!*  DATE                       : 2007-07-07 (original: 11/08/2004)
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
!*                                        Try namelist formatting with polymorphic/nonpoly scalar pointer/allocatable (Output)
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
   type, abstract :: base (lb) ! lb=3
      integer, len :: lb
      character(lb), allocatable :: i
   end type

   type, extends(base) :: child (lc) ! lc=3
      integer, len :: lc
      character(lc), pointer :: i1
   end type


   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program scalar003kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(:)) , pointer     :: b1 ! tcx: (:)
   type(child(3,3))               :: b3 ! tcx: (3,3)
   type(child(3,3)) , pointer     :: b4 ! tcx: (:,:)
   character(3), target      :: c1 = 'IBM'

   namelist /nml/ b1, b3, b4

   open (1, file = 'scalar003kl.1', form='formatted', access='sequential' )
   allocate(child(3,3)::b1) ! tcx: (3,3)
   allocate(child(3,3)::b4) ! tcx: child(3,3)
   allocate(character(3):: b1%i, b3%i, b4%i) ! tcx: character(3)

   b1%i = 'abc'
   b3%i = 'def'
   b4%i = 'ghi'

   select type ( b1 )
      type is (child(*,*)) ! tcx: (*,*)
         b1%i1 => c1
         b3%i1 => b1%i1
         b4%i1 => b3%i1
   end select

   write (1, nml, iostat=stat, iomsg=msg)

   if ( ( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   write (unit, "('i= ',A3,1X)", iostat=iostat )             dtv%i

   select type ( i => dtv )
      class is (child(*,*)) ! tcx: (*,*)
         write (unit, "('i1= ',A3,1X)", iostat=iostat )      i%i1
      class default
         error stop 4_4
   end select

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 3 changes
! type: child - added parameters (lc) to invoke with (3,3) / declare with (*,*) - 5 changes
