! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : save001akl
!*
!*  PROGRAMMER                 : David Forster (derived from save001a by Robert Ma)
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
!*                                        Try namelist formatting with save attribute
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
      integer(kd) :: i
   end type

   type :: base (kb)
      integer, kind :: kb
      class(data(kb)), allocatable :: d ! tcx: (kb)
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

contains

   subroutine writeDummy(unit)
      integer, intent(in) :: unit

      class(base(4)), pointer, save :: dummy ! tcx: (4)
      namelist /nml/ dummy
      integer :: stat
      character(200) :: msg

      if ( .not. associated(dummy) ) then
      	 allocate(dummy, source = base(4)(data(4)(10)) ) ! tcx: (4) ! tcx: (4)
      else
      	 dummy%d%i = dummy%d%i + 1
      end if

      write ( unit, nml, iostat=stat, iomsg = msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   end subroutine

end module

program save001akl
   use m

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'save001akl.1', form='formatted', access='stream' )

   call writeDummy(1)
   call writeDummy(1)
   call writeDummy(1)
   call writeDummy(1)
   call writeDummy(1)

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, data

   interface write(formatted)
      subroutine writeformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         class(data(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4)), intent(in) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   write (unit,*, iostat=iostat, iomsg=iomsg )              dtv%d

   if ( ( iostat /= 0 ) .or. ( iomsg /= 'datawrite' ) ) error stop 1_4

   iomsg = 'dtiowrite'

end subroutine

subroutine writeformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: data

   class(data(4)), intent(in) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, "(1X,I4)", iostat=iostat )              dtv%i

   iomsg = 'datawrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: data - added parameters (kd) to invoke with (4) / declare with (4) - 4 changes
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 4 changes
