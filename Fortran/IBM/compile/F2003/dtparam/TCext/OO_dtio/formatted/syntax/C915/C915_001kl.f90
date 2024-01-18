! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : C915_001kl
!*
!*  PROGRAMMER                 : David Forster (derived from C915_001 by Robert Ma)
!*  DATE                       : 2007-07-23 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: C915
!*                                        A namelist group name shall not appear when there is input/output item
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
      real(kb), allocatable :: i
      real(kb), pointer     :: j
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(formatted)
      subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
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

program C915_001kl
   use m
   class(base(4)), allocatable :: b1 ! tcx: (4)
   class(base(4)), allocatable :: b2 ! tcx: (4)
   type(base(4))               :: b3 ! tcx: (4)

   integer :: stat
   character(200) :: msg

   namelist /nml1/ b1
   namelist /nml1/ b2, b3

   allocate(b1, b1%i, b1%j)
   allocate(b2, b2%i, b2%j)
   allocate(b3%i, b3%j)

   b1%i = 4.0
   b1%j = 8.0
   b2%i = 12.0
   b2%j = 16.0
   b3%i = 20.0
   b3%j = 24.0

   open (1, file = 'C915_001kl.1', form='formatted', access='sequential' )

   write (1, nml1, iostat=stat, iomsg=msg)         b1
   write (1, nml=nml1, iostat=stat, iomsg=msg)     b2
   rewind 1
   read (1, nml=nml1, iostat=stat, iomsg=msg)      b2
   read (1, nml1, iostat=stat, iomsg=msg)          b3

   ! close file

   close ( 1, status = 'delete' )

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   real(4) :: x, y
   namelist /nml/ x,y

   read (unit, nml, iostat=iostat )     dtv%i, dtv%j

   iomsg = 'dtioread'

end subroutine


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4)), intent(in) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   real(4) :: x, y
   namelist /nml/ x,y

   x = dtv%i
   y = dtv%j

   write (unit, nml, iostat=iostat ) dtv%i, dtv%j

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 7 changes
