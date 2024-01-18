! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : funcRetrn101akl
!*
!*  PROGRAMMER                 : David Forster (derived from funcRetrn101a by Robert Ma)
!*  DATE                       : 2007-07-05 (original: 11/08/2004)
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
!*                                        Try namelist formatting with polymorphic function return variable
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
      character(lb) :: c
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: i
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: unit = 1
   integer :: stat
   character(200) :: msg

contains

   class(base(:)) function read(i) ! tcx: (3)
      namelist /R/ read
      allocatable :: read
      integer, intent(in) :: i

      if ( i == 0 ) then
         allocate ( base(3) :: read ) ! tcx: (3)
         read (unit, R, iostat = stat, iomsg = msg )
      else
         allocate ( child(3,4) :: read ) ! tcx: (3,4)
         read (unit, R, iostat = stat, iomsg = msg )
      end if
   end function

end module

program funcRetrn101akl
   use m, newread => read

   class(base(:)), allocatable :: b1 ! tcx: (:)
   type(base(3))               :: b2 = base(3)  ('xxx') ! tcx: (3) ! tcx: (3)
   type(child(3,4))              :: c1 = child(3,4) ('xxx',-999) ! tcx: (3,4) ! tcx: (3,4)
   class(child(:,4)), pointer    :: c2 ! tcx: (:,4)

   open (1, file = 'funcRetrn101akl.1', form='formatted', access='sequential' )

   allocate ( b1, source = newread(0) )
   select type ( gg => newread(1) )
      class is (child(*,4)) ! tcx: (*,4)
         allocate ( c2, source = gg )
   end select

   if ( b1%c /= 'IBM' )                           error stop 1_4
   if ( ( c2%c /= 'FTN' ) .or. ( c2%i /= 123 ) )  error stop 2_4

   deallocate (b1)
   allocate(b1, source = child(3,4)('xxx',-999)) ! tcx: (3,4)

   select type ( g => newread(1) )
      type is (child(*,4)) ! tcx: (*,4)
         c1 = g
   end select

   if ( ( c1%c /= 'IBM' ) .or. ( c1%i /= 2005 ) )  error stop 3_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base,child

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   select type (dtv)
      type is (base(*)) ! tcx: (*)
         read (unit, "(A3)", iostat=iostat )            dtv%c
      type is (child(*,4)) ! tcx: (*,4)
         read (unit, *, iostat=iostat )      dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 8 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 8 changes
