! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg108kl
!*
!*  DATE                       : 2007-07-05 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object with module subroutine
!*                                        with pointer/allocatable dummy arguments (input)
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

contains

   subroutine readB1B2(unit, b1, b2)
      integer, intent(in) :: unit
      class(base(*)), pointer, intent(inout) :: b1 ! tcx: (*)
      class(base(*)), allocatable, intent(inout) :: b2 ! tcx: (*)

      namelist /nmlb1b2/ b1, b2

      integer :: stat
      character(200) :: msg

      read ( unit, nmlb1b2, iostat=stat, iomsg = msg)

   end subroutine

end module

program dummyArg108kl
   use m
   class(base(3)), pointer :: b1 ! tcx: (:)
   class(base(3)), allocatable :: b2 ! tcx: (:)

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'dummyArg108kl.1', form='formatted', access='stream' )

   allocate(b1, source = base(3)()  ) ! tcx: (3)
   allocate(b2, source = child(3,4)() ) ! tcx: (3,4)

   call readB1B2(1, b1, b2)

   select type (b2)
      type is (child(*,4)) ! tcx: (*,4)
         if ( ( b1%c /= 'ibm' ) .or. ( b2%c /= 'IBM' ) .or. ( b2%i /= 1234 ))   error stop 1_4
      class default
         error stop 2_4
   end select

   deallocate(b1, b2)
   allocate(b1, source = child(3,4)() ) ! tcx: (3,4)
   allocate(b2, source = base(3)()  ) ! tcx: (3)

   call readB1B2(1, b1, b2)

   select type (b1)
      type is (child(*,4)) ! tcx: (*,4)
         if ( ( b1%c /= 'DEF' ) .or. ( b1%i /= 2005 ).or. ( b2%c /= 'ABC' ) )   error stop 2_4
      class default
         error stop 3_4
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

   !if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   select type (dtv)
      type is (base(*)) ! tcx: (*)
         read (unit, "(A3)", iostat=iostat )         dtv%c
      type is (child(*,4)) ! tcx: (*,4)
         read (unit, "(A3,1X,I4)", iostat=iostat )   dtv%c, dtv%i
   end select

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 9 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 5 changes
