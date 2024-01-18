! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg001akl
!*
!*  DATE                       : 2007-07-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
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
      character(lb) ::  c
   end type

   type, extends(base) :: child (kb)
      integer, kind :: kb
      integer(kb)   ::  i
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: unit = 1

contains

   subroutine writeBase(dtv)
      class(base(*)), intent(in) :: dtv(3) ! tcx: (*)
      integer :: stat
      character(200) :: msg

      namelist /nml/ dtv
      write ( unit, nml, iostat=stat, iomsg = msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end subroutine

end module

program dummyArg001akl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(:)), allocatable :: b1(:) ! tcx: (:)
   class(base(:)), pointer     :: b2(:) ! tcx: (:)
   type(base(3))               :: b3(3) ! tcx: (3)
   type(base(:)), pointer      :: b4(:) ! tcx: (:)

   open (unit, file = 'dummyArg001akl.1', form='formatted', access='stream' )

   allocate(b1(3), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi') /) ) ! tcx: (3), (3), (3)
   allocate(b2(3), source = (/ ( child(3,4)(c='IBM',i=j), j=1,3 ) /) ) ! tcx: (3,4)
   b3 = base(3)(c='jkl') ! tcx: (3)
   allocate(b4(3), source = (/ base(3)(c='mno'), base(3)('pqr'), base(3)('stu') /) ) ! tcx: (3), (3), (3)

   call writeBase(b1)
   call writeBase(b2)
   call writeBase(b3)
   call writeBase(b4)

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type ( dtv )
      type is ( base(*) ) ! tcx: (*)
         write (unit, "('c= ',A3,1X)", iostat=iostat )        dtv%c
      type is ( child(*,4) ) ! tcx: (*,4)
         write (unit, "('i= ',I4,1X,'c= ',A3,1X)", iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtiowrite'

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 15 changes
! type: child - added parameters (kb) to invoke with (3,4) / declare with (*,4) - 2 changes
