! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-05 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting inside final subroutine
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
      character(lb) :: c = 'xxx'
   contains
      final :: finalbase
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: i = -999
   contains
      final :: finalchild
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

   subroutine finalbase ( dtv )
      type(base(*)), intent(inout) :: dtv ! tcx: (*)
      namelist /FB/ dtv

      write ( unit, fb, iostat = stat, iomsg = msg )

      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end subroutine

   subroutine finalchild ( dtv )
      type(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
      namelist /FC/ dtv

      write ( unit, fc, iostat = stat, iomsg = msg )

      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
   end subroutine

end module

program final001kl
   use m

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(child(:,4)), pointer    :: c2 ! tcx: (:,4)

   open (1, file = 'final001kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(3)('IBM') ) ! tcx: (3)
   deallocate( b1 )
   allocate ( b1, source = child(3,4)('IBM',1001) ) ! tcx: (3,4)
   deallocate ( b1 )
   allocate ( child(3,4):: c2 ) ! tcx: child(3,4)
   deallocate ( c2 )


end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base,child

   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   select type (dtv)
      type is (base(*)) ! tcx: (*)
         write (unit, "('c=',A3)", iostat=iostat )                  dtv%c
      type is (child(*,4)) ! tcx: (*,4)
         write (unit, "('i=',I4,1X,'c=',A3)", iostat=iostat )      dtv%i, &
                                                                   dtv%c
   end select

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 6 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 4 changes
