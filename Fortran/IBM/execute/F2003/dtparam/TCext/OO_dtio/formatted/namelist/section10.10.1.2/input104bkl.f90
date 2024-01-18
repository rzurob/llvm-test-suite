! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : input104bkl
!*
!*  DATE                       : 2007-07-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist Input Values
!*                                        Derived type variable shall be expanded into intrinsic types
!*                                       (no dtio procedure involved, with array components)
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
   type base (kb,lb) ! kb,lb=4,3
      integer, kind :: kb
      integer, len :: lb
      integer(kb)   :: i(lb) = -9 ! (/ -9, -9, -9 /)
   end type

   type, extends(base) :: child (kc,lc) ! kc,lc=4,3
      integer, kind :: kc
      integer, len :: lc
      real(kc)      :: r(lc) = -9.0 ! (/ -9.0, -9.0, -9.0 /)
   end type

   type, extends(child) :: gen3 (lg1,lg2) ! lg1,lg2=3,3
      integer, len :: lg1,lg2
      character(lg1) :: c(lg2) = 'xxx' ! (/ 'xxx', 'xxx', 'xxx' /)
   end type
end module

program input104bkl
   use m

   integer :: stat
   character(150) :: msg = ''
   procedure(logical) :: precision_r4

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4,:)), allocatable  :: b1 ! tcx: (4,:)
   class(base(4,:)), pointer      :: b2 ! tcx: (4,:)
   class(child(4,:,4,:)), allocatable :: c1 ! tcx: (4,:,4,:)
   class(child(4,:,4,:)), pointer     :: c2 ! tcx: (4,:,4,:)
   class(gen3(4,:,4,:,:,:)), allocatable  :: g1 ! tcx: (4,:,4,:,:,:)

   namelist /n1/ b1, c1, g1
   namelist /n2/ b2, c2

   allocate(base(4,3):: b1) ! tcx: base(4,3)
   allocate(child(4,3,4,3):: c1) ! tcx: child(4,3,4,3)
   allocate(gen3(4,3,4,3,3,3):: g1) ! tcx: gen3(4,3,4,3,3,3)
   allocate(child(4,3,4,3):: b2) ! tcx: (4,3,4,3)
   allocate(gen3(4,3,4,3,3,3) :: c2) ! tcx: (4,3,4,3,3,3)

   open (1, file='input104bkl.1', form='formatted', access='sequential' )

   read (1, n1, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read (1, n2, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   print *, b1%i
   select type(b2)
      type is (child(4,*,4,*)) ! tcx: (4,*,4,*)
         print *, b2%i, b2%r
   end select
   print *, c1%i, c1%r
   select type (c2)
      type is (gen3(4,*,4,*,*,*)) ! tcx: (4,*,4,*,*,*)
         print *, c2%i, c2%r, c2%c
   end select
   print *, g1%i, g1%r, g1%c

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, gen3

   class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   type(base(4,3)) :: d1 ! tcx: (4,3)
   namelist /dtio1/ d1
   type(child(4,3,4,3)) :: d2 ! tcx: (4,3,4,3)
   namelist /dtio2/ d2
   type(gen3(4,3,4,3,3,3)) :: d3 ! tcx: (4,3,4,3,3,3)
   namelist /dtio3/ d3

   if ( iotype /= 'NAMELIST' ) error stop 3_4
   if ( size(v_list,1) /= 0 )  error stop 4_4

   select type(dtv)
      type is (base(4,*)) ! tcx: (4,*)
         read(unit, dtio1, iostat = iostat)
         dtv%i = d1%i
      type is (child(4,*,4,*)) ! tcx: (4,*,4,*)
         read(unit, dtio2, iostat = iostat)
         dtv%i = d2%i
         dtv%r = d2%r
      type is (gen3(4,*,4,*,*,*)) ! tcx: (4,*,4,*,*,*)
         read(unit, dtio3, iostat = iostat)
         dtv%i = d3%i
         dtv%r = d3%r
         dtv%c = d3%c
   end select

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: gen3 - added parameters (lg1,lg2) to invoke with (4,3,4,3,3,3) / declare with (4,*,4,*,*,*) - 2 changes
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 6 changes
! type: child - added parameters (kc,lc) to invoke with (4,3) / declare with (4,*) - 6 changes
! type: child - added parameters (kc,lc) to invoke with (4,3,4,3) / declare with (4,*,4,*) - 6 changes
! type: gen3 - added parameters (lg1,lg2) to invoke with (4,3,4,3,3,3) / declare with (4,*,4,*,*,*) - 5 changes
