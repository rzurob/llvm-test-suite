! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-05 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object with internal subroutine (Host Association)
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

end module

program dummyArg107kl
   use m
   class(base(4,:)), pointer :: b1 ! tcx: (4,:)
   class(base(4,:)), allocatable :: b2 ! tcx: (4,:)

   namelist /nmlb1b2/ b1, b2
   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'dummyArg107kl.1', form='formatted', access='stream' )

   allocate(b1, source = base(4,3)() ) ! tcx: (4,3)
   allocate(b2, source = base(4,3)() ) ! tcx: (4,3)

   call readB1B2(1)
   if ( ( b1%i /= 1234 ) .or. ( b1%c /= 'ABC' ) .or. ( b2%i /= 7890 ) .or. ( b2%c /= 'DEF' ) ) error stop 1_4

   rewind 1

   call readB1B2v2(1, b2, b1)   !<- order reversed
   if ( ( b2%i /= 1234 ) .or. ( b2%c /= 'ABC' ) .or. ( b1%i /= 7890 ) .or. ( b1%c /= 'DEF' ) ) error stop 2_4

contains

   subroutine readB1B2v2(unit, b1, b2)
      integer, intent(in) :: unit
      class(base(4,*)), intent(inout) ::  b1, b2 ! tcx: (4,*)
      integer :: stat
      character(200) :: msg

      namelist /nmlb1b2/ b1, b2       !<- name made accessible through host association!
      read ( unit, nmlb1b2, iostat=stat, iomsg = msg)
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   end subroutine

   subroutine readB1B2(unit)
      integer, intent(in) :: unit
      integer :: stat
      character(200) :: msg
      read ( unit, nmlb1b2, iostat=stat, iomsg = msg)
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4
   end subroutine

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 5_4
   if ( size(v_list, 1) /= 0 ) error stop 6_4

   read (unit, "(I4,1X,A3)", iostat=iostat )        dtv%i, dtv%c

   iomsg = 'dtioread'

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 7 changes
