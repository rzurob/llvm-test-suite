! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : save101kl
!*
!*  PROGRAMMER                 : David Forster (derived from save101 by Robert Ma)
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
!*                                        Try namelist formatting with save attribute (input)
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

   type :: data (kd)
      integer, kind :: kd
      integer(kd) :: j
   end type

   type :: base (kb1,kb2)
      integer, kind :: kb1,kb2
      integer(kb1) :: i
      type(data(kb2)) :: d ! tcx: (kb2)
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4,4)), intent(inout) :: dtv ! tcx: (4,4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

contains

   subroutine readDummy(unit)
      integer, intent(in) :: unit

      class(base(4,4)), allocatable, save :: dummy ! tcx: (4,4)
      class(base(4,4)), allocatable, save :: olddummy ! tcx: (4,4)

      namelist /nml/ dummy
      integer :: stat
      character(200) :: msg

      if ( .not. allocated(dummy) ) then
      	 allocate(dummy, source = base(4,4)(100,data(4)(200)) ) ! tcx: (4) ! tcx: (4,4)
      	 allocate(olddummy, source = dummy )
      else
         read ( unit, nml, iostat=stat, iomsg = msg)
      	 if ( olddummy%i /= (dummy%i - 1 ) )          error stop 1_4
      	 if ( olddummy%d%j /= (dummy%d%j - 1 ) )      error stop 2_4
      	 if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

      	 olddummy%i = dummy%i
      	 olddummy%d%j = dummy%d%j

      end if

      print *, dummy%i, dummy%d%j

   end subroutine

end module

program save101kl
   use m

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'save101kl.1', form='formatted', access='stream' )

   call readDummy(1)
   call readDummy(1)
   call readDummy(1)
   call readDummy(1)
   call readDummy(1)

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4,4)), intent(inout) :: dtv ! tcx: (4,4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   read (unit, "(I4)", iostat=iostat )              dtv%i
   if ( iostat /= 0 ) error stop 6_4
   read (unit, *, iostat= iostat )                  dtv%d

   iomsg = 'dtioread'

end subroutine



! Extensions to introduce derived type parameters:
! type: data - added parameters (kd) to invoke with (4) / declare with (4) - 2 changes
! type: base - added parameters (kb1,kb2) to invoke with (4,4) / declare with (4,4) - 5 changes
