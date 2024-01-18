! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar111akl
!*
!*  DATE                       : 2007-07-12 (original: 11/08/2004)
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

   type base (lb) ! lb=9
      integer, len :: lb
      character(0) :: c
   end type

   type, extends(base) :: child (lc) ! lc=0
      integer, len :: lc
      character(lc) :: c1
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

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)
   type(base(9))               :: b3 ! tcx: (9)
   type(base(:)), pointer      :: b4 ! tcx: (:)
   type(base(:)), allocatable  :: b5 ! tcx: (:)

   namelist /nml1/ b1
   namelist /nml2/ b2
   namelist /nml3/ b3
   namelist /nml4/ b4
   namelist /nml5/ b5

end module

program scalar111akl
   use m

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'scalar111akl.1', form='formatted', access='sequential' )
   allocate ( child(9,0) :: b1 ) ! tcx: (9,0)
   allocate ( base(9) :: b2, b4, b5 ) ! tcx: (9)

   read (1,NML=nml1, iostat=stat, iomsg=msg)

   if (( stat /=  1002 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   read (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  1001 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
   read (1,NML=nml3, iostat=stat, iomsg=msg)
   if (( stat /=  1001 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   read (1,NML=nml4, iostat=stat, iomsg=msg)
   if (( stat /=  1001 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4
   read (1,NML=nml5, iostat=stat, iomsg=msg)
   if (( stat /=  1001 ) .or. ( msg /= 'dtioread' ) ) error stop 5_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   select type (dtv)
      type is (base(*)) ! tcx: (*)
         read ( unit, * ) dtv%c
         iostat = 1001
      type is (child(*,*)) ! tcx: (*,0)
         read ( unit, * ) dtv%c, dtv%c1
         iostat = 1002
   end select

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (0) / declare with (0) - 9 changes
! type: base - added parameters (lb) to invoke with (9) / declare with (*) - 9 changes
! type: child - added parameters (lc) to invoke with (0) / declare with (*) - 2 changes
! type: child - added parameters (lc) to invoke with (9,0) / declare with (*,0) - 2 changes
