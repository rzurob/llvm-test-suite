! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : logical001kl
!*
!*  PROGRAMMER                 : David Forster (derived from logical001 by Robert Ma)
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
!*  DESCRIPTION                : Testing: Section 10.10.1.3 Namelist group object list items
!*                                        try logical with namelist formatting
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
      logical(kb) :: true(lb) = .true. ! (/ .true.  , .true., .true.  /)
      logical(kb) :: false(lb) = .false. ! (/ .false. , .false., .false. /)
   end type
end module

program logical001kl
   use m

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

   integer :: stat
   character(150) :: msg = ''

   class(base(4,:)), allocatable  :: b1 ! tcx: (4,:)
   class(base(4,:)), pointer      :: b2 ! tcx: (4,:)
 
   namelist /n1/ b1, b2
   
   allocate (base(4,3):: b1, b2) ! tcx: base(4,3)

   open (1, file='logical001kl.1', form='formatted', access='sequential' )

   read (1, n1, iostat = stat, iomsg = msg)
   
   print *,b1%true
   print *,b1%false
   print *,b2%true
   print *,b2%false
   
end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   logical :: true(3), false(3)

   namelist /dtio/ true

   if ( iotype /= 'NAMELIST' ) error stop 1_4
   if ( size(v_list,1) /= 0 )  error stop 2_4

   read( unit, dtio, iostat = iostat)
   false = (.not. true)
   dtv%true = true
   dtv%false = false

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 4 changes
