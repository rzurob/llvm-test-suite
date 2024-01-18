! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : rename002kl
!*
!*  DATE                       : 2007-07-06 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        rename: local-name is a namelist-group-name
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type base (lb)
      integer, len :: lb
      character(lb) :: c
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

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)
   type(base(3))               :: b3 = base(3)('ibm') ! tcx: (3) ! tcx: (3)

   namelist /nml1/ b1, b2
   namelist /nml2/ b3, b2, b1

end module


program rename002kl
   use m1, n1 => nml1, n2 => nml2, b11 => b1, b12 => b2

   integer :: stat
   character(200) :: msg

   open (unit = 3, file ='rename002kl.1', form='formatted', access='sequential')

   ! allocation of variables

   allocate (b11, source = base(3)('abc') ) ! tcx: (3)
   allocate (b12, source = base(3)('def') ) ! tcx: (3)

   ! unformatted I/O operations

   write ( 3, n1, iostat = stat, iomsg = msg )
   if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )            error stop 1_4
   write ( 3, n2, iostat = stat, iomsg = msg )
   if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )            error stop 2_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m1, only: base

   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   write (unit, "('c= ', A3, 1X)", iostat=iostat )        dtv%c

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 8 changes
