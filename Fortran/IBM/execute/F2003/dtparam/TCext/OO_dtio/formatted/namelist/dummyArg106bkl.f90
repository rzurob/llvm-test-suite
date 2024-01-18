! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg106bkl
!*
!*  DATE                       : 2007-07-05 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object with module subroutine (Use and Host Association)
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

   type, extends(base) :: child (kb)
      integer, kind :: kb
      integer(kb)   ::  i = -999
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

   class(base(:)), pointer :: b1 ! tcx: (:)

   namelist /nmlb1b2/ b1

contains

   subroutine readB1B2(unit, b2)
      integer, intent(in) :: unit
      class(base(*)), intent(inout) :: b2 ! tcx: (*)

      namelist /nmlb1b2/ b1, b2     !<- only b1, b2 will be written to file

      integer :: stat
      character(200) :: msg
      read ( unit, nmlb1b2, iostat=stat, iomsg = msg)
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   end subroutine

end module

program dummyArg106bkl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(:)), allocatable :: b2 ! tcx: (:)
   type(base(3)) :: b3   = base(3)('IBM') ! tcx: (3) ! tcx: (3)
   open (1, file = 'dummyArg106bkl.1', form='formatted', access='sequential' )

   allocate(b1, source = base(3) () ) ! tcx: (3)
   allocate(b2, source = child(3,4)() ) ! tcx: (3,4)

   call readB1B2(1, b2)
   select type(b2)
      type is (child(*,4)) ! tcx: (*,4)
         print *, b1%c, b2%c, b2%i
         if ( (b1%c /='abc') .or. (b2%c /= 'def') .or. (b2%i /= 1234) ) error stop 2_4
      class default
         error stop 3_4
   end select

   allocate(b1, source = child(3,4)() ) ! tcx: (3,4)
   call readB1B2(1, b3)

   select type(b1)
      type is (child(*,4)) ! tcx: (*,4)
         if ( (b1%c /='ABC') .or. (b1%i /= 2345) .or. (b3%c /= 'DEF') ) error stop 4_4
      class default
         error stop 5_4
   end select

   read ( 1, nmlb1b2, iostat=stat, iomsg = msg)  !<- only reads b1
   select type(b1)
      type is (child(*,4)) ! tcx: (*,4)
         if ( (b1%c /='xyz') .or. (b1%i /= 7777) ) error stop 6_4
      class default
         error stop 7_4
   end select

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
         read (unit, *, iostat=iostat )                     dtv%c
      type is (child(*,4)) ! tcx: (*,4)
         read (unit, *, iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 9 changes
! type: child - added parameters (kb) to invoke with (3,4) / declare with (*,4) - 6 changes
