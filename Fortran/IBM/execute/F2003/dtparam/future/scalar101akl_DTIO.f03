! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-10 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic/nonpoly scalar allocatable (Input)
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
   type base (lb) ! lb=3
      integer, len :: lb
      character(lb), allocatable :: c
   end type

end module

module m1
   use m
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
end module


program scalar101akl
   use m1

   integer :: stat
   character(200) :: msg = ''
   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)
   type(base(3))               :: b3 ! tcx: (3)
   type(base(:)), allocatable  :: b4 ! tcx: (:)
   type(base(:)), pointer      :: b5 ! tcx: (:)

   namelist /nml/ b1, b2, b3, b4, b5
   open (1, file = 'scalar101akl_DTIO.1', form='formatted', access='sequential', status='old', BLANK='NULL' )

   allocate(b1, source = base(3)(null())) ! tcx: (3)
   allocate(b2, source = base(3)(null())) ! tcx: (3)
   allocate(b4, source = base(3)(null())) ! tcx: (3)
   allocate(b5, source = base(3)(null())) ! tcx: (3)

   allocate(character(3):: b1%c, b2%c, b3%c, b4%c, b5%c) ! tcx: character(3)

   read  (1,NML=nml, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   if ( b1%c /= 'xyz' ) error stop 2_4
   if ( b2%c /= 'def' ) error stop 3_4
   if ( b3%c /= 'jkl' ) error stop 4_4
   if ( b4%c /= 'mno' ) error stop 5_4
   if ( b5%c /= 'ghi' ) error stop 6_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m1, only: base

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 7_4
   if ( size(v_list, 1) /= 0 ) error stop 8_4

   read (unit, "(A3,1X)", iostat=iostat )      dtv%c

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 11 changes
