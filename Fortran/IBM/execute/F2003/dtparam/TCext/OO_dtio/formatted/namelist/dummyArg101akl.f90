! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg101akl
!*
!*  PROGRAMMER                 : David Forster (derived from dummyArg101a by Robert Ma)
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
!*                                        Try namelist formatting for derived type object which is a explicit array dummy argument
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

   type :: base (lb)
      integer, len :: lb
      character(lb) ::  c = 'xxx'
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc)   ::  i = -999
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

   integer :: unit = 1

contains

   subroutine readBase(dtv)
      class(base(*)), intent(inout) :: dtv(3) ! tcx: (*)
      integer :: stat
      character(200) :: msg

      namelist /nml/ dtv
      read ( unit, nml, iostat=stat, iomsg = msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   end subroutine

end module

program dummyArg101akl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(:)), allocatable :: b1(:) ! tcx: (:)
   class(base(:)), pointer     :: b2(:) ! tcx: (:)
   type(base(3))               :: b3(3) ! tcx: (3)
   type(child(:,4)), pointer     :: b4(:) ! tcx: (:,4)

   open (unit, file = 'dummyArg101akl.1', form='formatted', access='stream' )

   allocate( base(3) :: b1(3) ) ! tcx: child(3,4)
   allocate( child(3,4) :: b2(3) ) ! tcx: (3,4)
   allocate( child(3,4) :: b4(3) ) ! tcx: child(3,4)

   call readBase(b1)
   call readBase(b2)
   call readBase(b3)
   call readBase(b4)
   
   if ( ( b1(1)%c /= 'abc' ) .or. ( b1(2)%c /= 'def' ) .or. ( b1(3)%c /= 'ghi' ) )    error stop 2_4
   
   select type ( b2 )
      type is (child(*,4)) ! tcx: (*,4)
         if ( ( b2(1)%c /= 'ABC' ) .or. ( b2(2)%c /= 'DEF' ) .or. ( b2(3)%c /= 'GHI' ) .or. &
              ( b2(1)%I /= 2001 ) .or. ( b2(2)%I /= 2002 ) .or. ( b2(3)%I /= 2003 ) ) error stop 3_4
   end select
   
   if ( ( b3(1)%c /= 'abc' ) .or. ( b3(2)%c /= 'xxx' ) .or. ( b3(3)%c /= 'ghi' ) )    error stop 4_4
   if ( ( b4(1)%c /= 'ABC' ) .or. ( b4(2)%c /= 'xxx' ) .or. ( b4(3)%c /= 'GHI' ) .or. &
        ( b4(1)%I /= 4001 ) .or. ( b4(2)%I /= -999 ) .or. ( b4(3)%I /= 4003 ) )       error stop 5_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type ( dtv )
      type is ( base(*) ) ! tcx: (*)
         read (unit, "(A3,1X)", iostat=iostat )        dtv%c
      type is ( child(*,4) ) ! tcx: (*,4)
         read (unit, "(I4,1X,A3,1X)", iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 7 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 4 changes
