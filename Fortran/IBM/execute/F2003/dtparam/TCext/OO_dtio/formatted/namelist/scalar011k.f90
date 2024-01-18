! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar011k
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

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(2)), intent(in) :: dtv ! tcx: (2)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
      subroutine writeformatted4(dtv, unit, iotype, v_list, iostat, iomsg ) ! tcx: writeformatted4
         import base
         class(base(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program scalar011k
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(2)), allocatable :: b1 ! tcx: (2)
   class(base(2)), pointer     :: b2 ! tcx: (2)
   type(base(2))               :: b3 ! tcx: (2)
   type(base(2)), pointer      :: b4 ! tcx: (2)
   type(base(2)), allocatable  :: b5 ! tcx: (2)
   type(base(4))               :: b6 ! tcx: base(4)

   namelist /nml/ b1, b2
   namelist /nml/ b3, b4, b5, b6

   open (1, file = 'scalar011k.1', form='formatted', access='sequential' )
   allocate ( child(2,1) :: b1 ) ! tcx: (2,2)
   allocate ( child(2,2) :: b2 ) ! tcx: (2,2)
   allocate ( b4, b5 )

   write (1,NML=nml, iostat=stat, iomsg=msg)

   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(2)), intent(in) :: dtv ! tcx: (2)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   select type (dtv)
      type is (base(2)) ! tcx: (2)
         write ( unit, "(A)", iostat = iostat ) 'base2'
      type is (child(2,2)) ! tcx: (2,2)
         write ( unit, "(A)", iostat = iostat ) 'child22'
      type is (child(2,1)) ! tcx: (2,1)
         write ( unit, "(A)", iostat = iostat ) 'child21'
   end select

   iomsg = 'dtiowrite'

end subroutine


subroutine writeformatted4 (dtv, unit, iotype, v_list, iostat, iomsg) ! tcx: writeformatted4
   use m, only: base, child

   class(base(4)), intent(in) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 5_4
   if ( size(v_list, 1) /= 0 ) error stop 6_4

   select type (dtv)
      type is (base(4)) ! tcx: (4)
         write ( unit, "(A)", iostat = iostat ) 'base4'
      type is (child(4,2)) ! tcx: (4,2)
         write ( unit, "(A)", iostat = iostat ) 'child42'
      type is (child(4,1)) ! tcx: (4,1)
         write ( unit, "(A)", iostat = iostat ) 'child41'
   end select

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (0) / declare with (0) - 9 changes
! type: base - added parameters (kb) to invoke with (2) / declare with (2) - 9 changes
! type: child - added parameters (kc) to invoke with (2,2) / declare with (2,2) - 2 changes
