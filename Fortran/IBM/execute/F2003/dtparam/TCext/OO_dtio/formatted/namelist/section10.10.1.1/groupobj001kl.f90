! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : groupobj001kl
!*
!*  PROGRAMMER                 : David Forster (derived from groupobj001 by Robert Ma)
!*  DATE                       : 2007-07-20 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist group object names
!*                                        Input data being object components of non-polymorphic entities
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
      character(lb) :: c = 'xxx'
      integer(kb)   :: i = 999
   end type

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
end module

program groupobj001kl
   use m

   integer :: stat
   character(200) :: msg
   type(base(4,3))               :: b3 ! tcx: (4,3)
   type(base(4,:)), allocatable  :: b4 ! tcx: (4,:)
   type(base(4,:)), pointer      :: b5 ! tcx: (4,:)

   namelist /nml/ b3, b4, b5
   allocate ( base(4,3):: b4, b5 ) ! tcx: base(4,3)

   open (1, file='groupobj001kl.1', form='formatted', access='sequential' )

   read (1, nml, iostat = stat, iomsg = msg)


   if ( ( b3%i /= 999  ) .or. ( b3%c /= 'def' ) )  error stop 1_4
   if ( ( b4%i /= 2002 ) .or. ( b4%c /= 'xxx' ) )  error stop 2_4
   if ( ( b5%i /= 3003 ) .or. ( b5%c /= 'ghi' ) )  error stop 3_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= 'NAMELIST' ) error stop 4_4
   if ( size(v_list,1) /= 0 )  error stop 5_4

   read (unit, "(A3, 1X, I4)", iostat=iostat )  dtv%c,dtv%i

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 5 changes
