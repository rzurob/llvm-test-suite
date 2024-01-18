! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg009kl
!*
!*  DATE                       : 2007-07-05 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object with type bound procedures
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
      contains
         procedure, pass :: writeme => writebase
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc)   ::  i
      contains
         procedure, pass :: writeme => writechild
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

contains

   subroutine writebase(b, unit)
      class(base(*)), intent(in) :: b ! tcx: (*)
      integer, intent(in) :: unit
      namelist /basenml/ b
      integer :: stat
      character(200) :: msg

      write (unit, basenml, iostat=stat, iomsg = msg)
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end subroutine

   subroutine writechild(b, unit)
      class(child(*,4)), intent(in) :: b ! tcx: (*,4)
      integer, intent(in) :: unit
      namelist /childnml/ b
      integer :: stat
      character(200) :: msg

      write (unit, childnml, iostat=stat, iomsg = msg)
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   end subroutine
end module

program dummyArg009kl
   use m
   class(base(:)), pointer       :: b1 ! tcx: (:)
   class(base(:)), allocatable   :: b2 ! tcx: (:)
   type(base(3))                 :: b3 ! tcx: (3)
   class(child(:,4)), allocatable  :: b4 ! tcx: (:,4)
   class(child(:,4)), pointer      :: b5 ! tcx: (:,4)

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'dummyArg009kl.1', form='formatted', access='stream' )

   allocate(b1, source = base(3)(c='abc')      ) ! tcx: (3)
   allocate(b2, source = child(3,4)(c='def',i=2) ) ! tcx: (3,4)
   b3 = base(3)('ghi') ! tcx: (3)
   allocate(b4, source = child(3,4)('jkl', 4) ) ! tcx: (3,4)
   allocate(b5, source = child(3,4)('mno', 5) ) ! tcx: (3,4)

   call b1%writeme(1)
   call b2%writeme(1)
   call b3%writeme(1)
   call b4%writeme(1)
   call b5%writeme(1)

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   select type (dtv)
      type is (base(*)) ! tcx: (*)
         write (unit, "('c= ',A3,1X)", iostat=iostat )        dtv%c
      type is (child(*,4)) ! tcx: (*,4)
         write (unit, "('i= ',I4, 1X,'c= ',A3,1X)", iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 9 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 7 changes
