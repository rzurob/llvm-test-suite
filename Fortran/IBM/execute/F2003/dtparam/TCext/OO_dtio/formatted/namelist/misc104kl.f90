! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-06 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Both pointer and pointer targets in the same namelist
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
   type base (kb)
      integer, kind :: kb
      integer(kb)   :: i
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      real(kc)      :: r
   end type

end module

program misc104kl
   use m

   integer :: stat
   character(150) :: msg = ''
   procedure (logical) :: precision_r4

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4)), pointer :: b1 ! tcx: (4)
   class(child(4,4)), pointer :: c1 ! tcx: (4,4)
   class(child(4,4)), allocatable, target :: c2 ! tcx: (4,4)

   namelist /nml/ c2, c1, b1  !<- in namelist data, c1 is read last. therefore, all c2, c1, b1 shall have the values of c1

   allocate ( c2, source= child(4,4)(r= -9.9999, i = -9999) ) ! tcx: (4,4)
   open (1, file='misc104kl.1', form='formatted', access='sequential' )

   c1 => c2
   b1 => c1

   read (1, nml, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   select type (b1)
      type is (child(4,4)) ! tcx: (4,4)
         if ( ( b1%i /= 9 ) .or. ( .not. precision_r4(b1%r, -9.0) ) ) error stop 1_4
         if ( ( c1%i /= 9 ) .or. ( .not. precision_r4(c1%r, -9.0) ) ) error stop 2_4
         if ( ( c2%i /= 9 ) .or. ( .not. precision_r4(c2%r, -9.0) ) ) error stop 3_4
   end select

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child
   class(base(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= 'NAMELIST' ) error stop 4_4
   if ( size(v_list,1) /= 0 )  error stop 5_4

   select type(dtv)
      type is (base(4)) ! tcx: (4)
         read(unit, *, iostat = iostat) dtv%i
      type is (child(4,4)) ! tcx: (4,4)
         read(unit, *, iostat = iostat) dtv%i, dtv%r
      class default
         error stop 4_4
   end select

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 4 changes
! type: child - added parameters (kc) to invoke with (4,4) / declare with (4,4) - 5 changes
