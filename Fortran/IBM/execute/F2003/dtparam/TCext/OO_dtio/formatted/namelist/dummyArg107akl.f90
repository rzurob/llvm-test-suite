! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg107akl
!*
!*  PROGRAMMER                 : David Forster (derived from dummyArg107a by Robert Ma)
!*  DATE                       : 2007-07-05 (original: 11/08/2004)
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
!*                                        Try namelist formatting for derived type object external subroutine with internal sub(Host Association)
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

   type :: base (kb,lb)
      integer, kind :: kb
      integer, len :: lb
      character(lb) ::  c = 'xxx'
      integer(kb)   ::  i = -999
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

   class(base(4,:)), pointer :: b2 ! tcx: (4,:)

end module

program dummyArg107akl
   use m

   interface
      subroutine readBase(dtv, unit)
         import base
         class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
         integer, intent(in)     :: unit
      end subroutine
   end interface

   integer :: stat
   character(200) :: msg = ''
   class(base(4,:)), allocatable :: b1 ! tcx: (4,:)
   type(base(4,3))               :: b3 ! tcx: (4,3)
   type(base(4,:)), pointer      :: b4 ! tcx: (4,:)

   open (1, file = 'dummyArg107akl.1', form='formatted', access='stream' )

   allocate(b1, source = base(4,3)() ) ! tcx: (4,3)
   allocate(b2, source = base(4,3)() ) ! tcx: (4,3)
   b3 = base(4,3)() ! tcx: (4,3)
   allocate(b4, source = b3 )

   call readBase(b1,1)
   if ( ( b1%i /= 1234 ) .or. ( b1%c /= 'abc' ) ) error stop 1_4
   call readBase(b2,1)
   if ( ( b2%i /= 2345 ) .or. ( b2%c /= 'def' ) ) error stop 2_4
   call readBase(b3,1)
   if ( ( b3%i /= 3456 ) .or. ( b3%c /= 'ghi' ) ) error stop 3_4
   call readBase(b4,1)
   if ( ( b4%i /= 4567 ) .or. ( b4%c /= 'jkl' ) ) error stop 4_4

end program

subroutine readBase(dtv, unit)
   use m
   class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   integer :: stat
   character(200) :: msg

   namelist /nml/ dtv
   call internalreadBase(unit)

contains

   subroutine internalreadBase(unit)
      integer, intent(in) :: unit
      read ( unit, nml, iostat=stat, iomsg = msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 5_4
   end subroutine

end subroutine

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   read (unit, "(I4,1X,A3)", iostat=iostat )        dtv%i, dtv%c


   iomsg = 'dtioread'

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 11 changes
