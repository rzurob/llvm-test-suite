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
!*                                        Try namelist formatting for derived type object which is a dummy argument (input)
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

contains

   subroutine readBase(unit, dtv)
      integer, intent(in) :: unit
      class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)

      integer :: stat
      character(200) :: msg = ''

      namelist /nml/ dtv
      read ( unit, nml, iostat=stat, iomsg = msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   end subroutine

end module

program dummyArg101kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4,:)), allocatable :: b1 ! tcx: (4,:)
   type(base(4,3))               :: b3 ! tcx: (4,3)
   type(base(4,:)), pointer      :: b4 ! tcx: (4,:)

   open (1, file = 'dummyArg101kl.1', form='formatted', access='stream' )

   allocate(base(4,3)::b1) ! tcx: base(4,3)
   allocate(base(4,3)::b2) ! tcx: base(4,3)
   b3 =  base(4,3)() ! tcx: (4,3)
   allocate(base(4,3)::b4) ! tcx: base(4,3)

   call readBase(1,b1)
   call readBase(1,b2)
   call readBase(1,b3)
   call readBase(1,b4)

   if ( ( b1%i /= 123 ) .or. ( b1%c /= 'abc' ) ) error stop 1_4
   if ( ( b2%i /= 234 ) .or. ( b2%c /= 'def' ) ) error stop 2_4
   if ( ( b3%i /= 345 ) .or. ( b3%c /= 'ghi' ) ) error stop 3_4
   if ( ( b4%i /= 456 ) .or. ( b4%c /= 'jkl' ) ) error stop 4_4

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

   read (unit, "(I3,1X,A3)", iostat=iostat )        dtv%i, dtv%c

   iomsg = 'dtioread'

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 8 changes
