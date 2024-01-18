! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg105kl
!*
!*  DATE                       : 2007-07-05 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object with internal subroutine (Host Association)
!*                                        on input statement
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

   class(base(:)), pointer :: b2 ! tcx: (:)

contains

   subroutine readBase(dtv, unit)
      class(base(*)), intent(inout) :: dtv ! tcx: (*)
      integer, intent(in) :: unit

      integer :: stat
      character(200) :: msg

      namelist /nml/ dtv

      call internalreadBase(unit, dtv)

      contains

      subroutine internalreadBase(unit, dtv)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         read ( unit, nml, iostat=stat, iomsg = msg)
         if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
      end subroutine

   end subroutine

end module

program dummyArg105kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(:)), allocatable :: b1 ! tcx: (:)
   type(base(3))               :: b3 ! tcx: (3)
   type(base(:)), pointer      :: b4 ! tcx: (:)

   open (1, file = 'dummyArg105kl.1', form='formatted', access='stream' )

   allocate(child(3,4):: b1,b2 ) ! tcx: (3,4)
   b3 = base(3)() ! tcx: (3)
   allocate(base(3):: b4) ! tcx: base(3)

   call readBase(b1,1)
   call readBase(b2,1)
   call readBase(b3,1)
   call readBase(b4,1)

   select type (b1)
      type is (child(*,4)) ! tcx: (*,4)
         if ( ( b1%i /= 123 ) .or. ( b1%c /= 'abc' ) ) error stop 2_4
      class default
         error stop 3_4
   end select
   select type (b2)
      type is (child(*,4)) ! tcx: (*,4)
         if ( ( b2%i /= 234 ) .or. ( b2%c /= 'def' ) ) error stop 4_4
      class default
         error stop 5_4
   end select
   if ( b3%c /= 'ghi' ) error stop 6_4
   if ( b4%c /= 'jkl' ) error stop 7_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 8_4
   if ( size(v_list, 1) /= 0 ) error stop 9_4

   select type (dtv)
      type is (base(*)) ! tcx: (*)
         read (unit, "(A3)", iostat=iostat )        dtv%c
      type is (child(*,4)) ! tcx: (*,4)
         read (unit, "(I3,1X,A3)", iostat=iostat )  dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 10 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 4 changes
