! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : protected101kl
!*
!*  PROGRAMMER                 : David Forster (derived from protected101 by Robert Ma)
!*  DATE                       : 2007-07-06 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with data object with protected attribute (input)
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
      complex(kb) :: c
   contains
      procedure, nopass :: readMe
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

   class(base(4)), pointer, protected :: b1 ! tcx: (4)
   class(base(4)), allocatable, protected :: b2 ! tcx: (4)
   type(base(4)), protected :: b3 ! tcx: (4)

   namelist /n1/ b1
   namelist /n2/ b2
   namelist /n3/ b3

   contains

   subroutine start()
      allocate(b1, b2)
   end subroutine

   subroutine readMe (unit, c)
      integer, intent(in) :: unit, c
      integer :: stat = 0
      character(150) :: msg = ''

      select case (c)
         case(1)
            read (unit, n1, iostat=stat, iomsg=msg)
         case(2)
            read (unit, n2, iostat=stat, iomsg=msg)
         case(3)
            read (unit, n3, iostat=stat, iomsg=msg)
         case default
            error stop 1_4
      end select

      if ( ( stat /= 0 ).or. ( msg /= 'dtioread') ) error stop 2_4

   end subroutine

end module

program protected101kl
use m

   class(base(4)), allocatable :: dummy ! tcx: (4)
   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'protected101kl.1', form='formatted', access='sequential' )
   allocate (dummy)
   call start()

   call dummy%readMe(1, 1)   !<- read b1
   call dummy%readMe(1, 2)   !<- read b2
   call dummy%readMe(1, 3)   !<- read b3

   print *, b1%c
   print *, b2%c
   print *, b3%c

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   namelist /nml/ c
   complex(4) :: c

   if ( iotype /= "NAMELIST" ) error stop 1_4
   if ( size(v_list, 1) /= 0 ) error stop 2_4

   read (unit, nml, iostat=iostat )

   dtv%c = c

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 6 changes
