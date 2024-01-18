! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : groupobj001dkl
!*
!*  DATE                       : 2007-07-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist group object names
!*                                        Within the input data, each name shall correspond
!*                                        to a particular namelist group object name
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
      character(lb) :: i
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
end module

program groupobj001dkl
   use m

   integer :: stat
   character(200) :: msg
   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)

   namelist /nml/ b1, b2
   allocate (base(3):: b1, b2) ! tcx: base(3)

   open (1, file='groupobj001dkl.1', form='formatted', access='sequential' )
   open (2, file='groupobj001dkl.2', form='formatted', access='sequential' )

   read (1, nml, iostat = stat, iomsg = msg)
   print *, stat, msg

   if ( ( b1%i /= 'abc' ) .or. ( b2%i /= 'def' ) )  error stop 1_4

   read (2, nml, iostat = stat, iomsg = msg)
   print *, stat, msg

   if ( ( b1%i /= 'abc' ) .or. ( b2%i /= 'def' ) )  error stop 1_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= 'NAMELIST' ) error stop 2_4
   if ( size(v_list,1) /= 0 )  error stop 3_4

   read (unit, "(A3)", iostat=iostat )  dtv%i

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 4 changes
