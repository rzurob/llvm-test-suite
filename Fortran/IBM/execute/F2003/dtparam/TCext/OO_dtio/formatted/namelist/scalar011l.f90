! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar011l
!*
!*  DATE                       : 2007-07-31 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with (non-) polymorphic zero sized derived type scalar object
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

   type base (lb) ! lb=0
      integer, len :: lb
   end type

   type, extends(base) :: child (lc) ! lc=0
      integer, len :: lc
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

program scalar011l
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)
   type(base(0))               :: b3 ! tcx: (0)
   type(base(:)), pointer      :: b4 ! tcx: (:)
   type(base(:)), allocatable  :: b5    ! tcx: (:)

   namelist /nml/ b1, b2
   namelist /nml/ b3, b4, b5

   open (1, file = 'scalar011l.1', form='formatted', access='sequential' )
   allocate ( child(0,0) :: b1 ) ! tcx: (0,0)
   allocate ( base(0) :: b2, b4, b5 ) ! tcx: (0)

   write (1,NML=nml, iostat=stat, iomsg=msg)

   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

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

   select type (dtv)
      type is (base(*)) ! tcx: (*)
         write ( unit, "(A)", iostat = iostat ) 'base'
      type is (child(*,*)) ! tcx: (*,*)
         write ( unit, "(A)", iostat = iostat ) 'child'
   end select

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (0) / declare with (*) - 9 changes
! type: child - added parameters (lc) to invoke with (0,0) / declare with (*,*) - 2 changes
