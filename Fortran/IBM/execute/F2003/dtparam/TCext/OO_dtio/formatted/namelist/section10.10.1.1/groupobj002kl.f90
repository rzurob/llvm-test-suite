! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist group object names
!*                                        Subscript, strides, and substring range shall be
!*                                        optionally signed with no kind type parameter
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
   type base (kb) ! kb=4
      integer, kind :: kb
      integer(kb) :: i
   end type

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
end module

program groupobj002kl
   use m

   integer :: stat
   character(150) :: msg
   class(base(4)), allocatable :: b1(:) ! tcx: (4)
   class(base(4)), pointer     :: b2(:) ! tcx: (4)
   class(base(4)), pointer     :: b3(:,:) ! tcx: (4)

   namelist /nml/ b1, b2, b3

   allocate(b1(-3:3), b2(-2:2), b3(0,0))

   open (1, file='groupobj002kl.1', form='formatted', access='sequential' )

   read (1, nml, iostat = stat, iomsg = msg)

   print *, b1%i
   print *, b2%i
   print *, b3%i

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= 'NAMELIST' ) error stop 3_4
   if ( size(v_list,1) /= 0 )  error stop 4_4

   read (unit, "(I4)", iostat=iostat )  dtv%i

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 5 changes
