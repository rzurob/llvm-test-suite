! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : C914_001kl
!*
!*  PROGRAMMER                 : David Forster (derived from C914_001 by Robert Ma)
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
!*  DESCRIPTION                : Testing: C914
!*                                        A namelist group name shall be a name of a namelist group
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

program C914_001kl
   use m
   class(base(4)), allocatable :: b1 ! tcx: (4)
   class(base(4)), pointer     :: b2 ! tcx: (4)
   type(base(4))               :: b3 ! tcx: (4)

   integer :: stat
   character(200) :: msg

   allocate(b1, b1%i, b1%j) ! tcx: base(4)
   allocate(b2, b2%i, b2%j) ! tcx: base(4)
   allocate(b3%i, b3%j) ! tcx: base(4)

   b1%i = 4.0
   b1%j = 8.0
   b2%i = 12.0
   b2%j = 16.0
   b3%i = 20.0
   b3%j = 24.0

   open (1, file = 'C914_001kl.1', form='formatted', access='sequential' )

   write (1, nml=msg, iostat=stat, iomsg=msg)
   write (1, nml=stat, iostat=stat, iomsg=msg)
   rewind 1
   read (1, nml=b1, iostat=stat, iomsg=msg)
   read (1, nml=(/b1,b2/), iostat=stat, iomsg=msg)

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

   read (unit, nml=iomsg, iostat=iostat )     !<- nml= being a character

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

   write (unit, nml=nml1, iostat=iostat )     !<- namelist does not exist

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 7 changes
