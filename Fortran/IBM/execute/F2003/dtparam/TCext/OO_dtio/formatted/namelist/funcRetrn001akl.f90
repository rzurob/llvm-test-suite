! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : funcRetrn001akl
!*
!*  PROGRAMMER                 : David Forster (derived from funcRetrn001a by Robert Ma)
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
      contains
         procedure, pass :: write
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: i
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

   integer :: unit = 1
   integer :: stat
   character(200) :: msg

contains

   class(base(:)) function write(dtv) ! tcx: (:)
      class(base(*)), intent(in) :: dtv ! tcx: (*)
      allocatable :: write
      namelist /W/ write

      allocate ( write, source = dtv )
      write (unit, W, iostat = stat, iomsg = msg )

   end function

end module

program funcRetrn001akl
   use m

   class(base(:)), allocatable :: b1 ! tcx: (:)
   type(base(3))               :: b2 = base(3)  ('IBM') ! tcx: (3) ! tcx: (3)
   type(child(3,4))              :: c1 = child(3,4) ('FTN',123) ! tcx: (3,4) ! tcx: (3,4)
   class(child(:,4)), pointer    :: c2 ! tcx: (:,4)

   open (1, file = 'funcRetrn001akl.1', form='formatted', access='sequential' )

   allocate ( b1, source = b2%write()  ) !<- writes b2

   associate( gg => c1%write() )         !<- writes c1
      select type ( gg )
         class is (child(*,4)) ! tcx: (*,4)
            allocate ( c2, source = gg )
      end select
   end associate

   deallocate (b1)
   allocate(b1, source = child(3,4)('IBM',2005)) ! tcx: (3,4)

   select type ( g => b1 )
      type is (child(*,4)) ! tcx: (*,4)
         select type ( gg => g%write() )
            type is (child(*,4)) ! tcx: (*,4)
               c1 = gg
         end select
   end select

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base,child

   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 1_4
   if ( size(v_list, 1) /= 0 ) error stop 2_4

   select type (dtv)
      type is (base(*)) ! tcx: (*)
         write (unit, "('c=',A3)", iostat=iostat )                 dtv%c
      type is (child(*,4)) ! tcx: (*,4)
         write (unit, "('i=',I4,1X,'c=',A3)", iostat=iostat )      dtv%i, dtv%c
   end select

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 8 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 8 changes
