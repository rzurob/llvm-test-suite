! *********************************************************************
!*  ===================================================================
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

   type base (kb) ! kb=2
      integer, kind :: kb
   end type

   type, extends(base) :: child (kc) ! kc=2
      integer, kind :: kc
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(2)), intent(inout) :: dtv ! tcx: (2)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
      subroutine readformatted4(dtv, unit, iotype, v_list, iostat, iomsg ) ! tcx: readformatted4
         import base
         class(base(4)), intent(inout) :: dtv ! tcx: (2)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program scalar111kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(2)), allocatable :: b1 ! tcx: (2)
   class(base(2)), pointer     :: b2 ! tcx: (2)
   type(base(2))               :: b3 ! tcx: (2)
   type(base(2)), pointer      :: b4 ! tcx: (2)
   type(base(2)), allocatable  :: b5 ! tcx: (2)
   type(base(4))               :: b6 ! tcx: (4)

   namelist /nml1/ b1
   namelist /nml2/ b2
   namelist /nml3/ b3
   namelist /nml4/ b4
   namelist /nml5/ b5
   namelist /nml6/ b6

   open (1, file = 'scalar111kl.1', form='formatted', access='sequential' )
   allocate ( child(2,1) :: b1 ) ! tcx: (2,1)
   allocate ( child(2,2) :: b2 ) ! tcx: (2,2)
   allocate ( b4, b5 )

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  1021 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
   read (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  1022 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   read (1,NML=nml3, iostat=stat, iomsg=msg)
   if (( stat /=  1002 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4
   read (1,NML=nml4, iostat=stat, iomsg=msg)
   if (( stat /=  1002 ) .or. ( msg /= 'dtioread' ) ) error stop 5_4
   read (1,NML=nml5, iostat=stat, iomsg=msg)
   if (( stat /=  1002 ) .or. ( msg /= 'dtioread' ) ) error stop 6_4
   read (1,NML=nml6, iostat=stat, iomsg=msg)
   if (( stat /=  1004 ) .or. ( msg /= 'dtioread' ) ) error stop 7_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(2)), intent(inout) :: dtv ! tcx: (2)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 8_4
   if ( size(v_list, 1) /= 0 ) error stop 9_4

   select type ( dtv )
      type is (base(2)) ! tcx: (2)
         iostat = 1002
      type is (child(2,2))  ! tcx: (2,2)
         iostat = 1022
      type is (child(2,1))  ! tcx: (2,1)
         iostat = 1021
   end select
   iomsg = 'dtioread'

end subroutine


subroutine readformatted4 (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(4)), intent(inout) :: dtv ! tcx: (2)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 10_4
   if ( size(v_list, 1) /= 0 ) error stop 11_4

   select type ( dtv )
      type is (base(4)) ! tcx: (4)
         iostat = 1004
      type is (child(4,2))  ! tcx: (4,2)
         iostat = 1042
      type is (child(4,1))  ! tcx: (4,1)
         iostat = 1041
   end select
   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (2) / declare with (2) - 9 changes
! type: child - added parameters (kc) to invoke with (2,2) / declare with (2,2) - 2 changes
