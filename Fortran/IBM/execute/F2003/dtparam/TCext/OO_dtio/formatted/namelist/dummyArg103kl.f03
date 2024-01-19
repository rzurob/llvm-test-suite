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
!*                                        Try namelist formatting for derived type object which is a dummy argument
!*                                        Subroutine being type bound procedure (for input)
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
   contains
      procedure, pass :: readBase
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc)   ::  i  = -999
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

   integer function readBase(dtv, unit)
      class(base(*)), intent(inout) :: dtv ! tcx: (*)
      integer, intent(in) :: unit

      integer :: stat
      character(200) :: msg = ''

      namelist /nml/ dtv
      read ( unit, nml, iostat=readBase, iomsg = msg)
      if (( msg /= 'dtioread' )) error stop 1_4

   end function

end module

program dummyArg103kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(:)), allocatable :: b1 ! tcx: (:)
   type(base(3))               :: b3 ! tcx: (3)
   type(base(:)), pointer      :: b4 ! tcx: (:)

   open (1, file = 'dummyArg103kl.1', form='formatted', access='stream' )

   allocate(child(3,4) :: b1 ) ! tcx: (3,4)
   allocate(base(3) :: b2) ! tcx: base(3)
   b3 =  base(3)(c='ghi') ! tcx: (3)
   allocate(b4, source = base(3)(c='jkl') ) ! tcx: (3)

   if ( b1%readBase(1) /= 0 ) error stop 2_4
   if ( b2%readBase(1) /= 0 ) error stop 3_4
   if ( b3%readBase(1) /= 0 ) error stop 4_4
   if ( b4%readBase(1) /= 0 ) error stop 5_4

   select type (b1)
      type is (child(*,4)) ! tcx: (*,4)
         if ( ( b1%c /= 'abc' ) .or. (b1%i /= 1234 ) ) error stop 6_4
      class default
         error stop 7_4
   end select

   if ( b2%c /= 'def' ) error stop 8_4
   if ( b3%c /= 'ghi' ) error stop 9_4
   if ( b4%c /= 'jkl' ) error stop 10_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 11_4
   if ( size(v_list, 1) /= 0 ) error stop 12_4

   select type (dtv)
      type is (base(*)) ! tcx: (*)
         read (unit, "(A3)", iostat=iostat )        dtv%c
      type is (child(*,4)) ! tcx: (*,4)
         read (unit, "(I4,1X,A3)", iostat=iostat )  dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 10 changes
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 10 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 3 changes
