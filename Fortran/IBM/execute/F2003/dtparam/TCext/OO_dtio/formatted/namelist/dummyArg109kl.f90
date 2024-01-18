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
!*                                        Try namelist formatting for derived type object with type bound procedures (input)
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

   type, abstract :: base (lb)
      integer, len :: lb
      character(lb) ::  c = 'xxx'
      contains
         procedure, pass :: readme => readbase
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc)   ::  i = -999
      contains
         procedure, pass :: readme => readchild
   end type

   type, extends(base) :: badchild (lbc)
      integer, len :: lbc
      character(lbc) :: c1 = 'XXX'
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

   subroutine readbase(b, unit)
      class(base(*)), intent(inout) :: b ! tcx: (*)
      integer, intent(in) :: unit
      namelist /basenml/ b
      integer :: stat
      character(200) :: msg
      read (unit, basenml, iostat=stat, iomsg = msg)
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   end subroutine

   subroutine readchild(b, unit)
      class(child(*,4)), intent(inout) :: b ! tcx: (*,4)
      integer, intent(in) :: unit
      namelist /childnml/ b
      integer :: stat
      character(200) :: msg

      read (unit, childnml, iostat=stat, iomsg = msg)
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
   end subroutine

end module

program dummyArg109kl
   use m
   class(base(:)), pointer       :: b1 ! tcx: (:)
   class(base(:)), allocatable   :: b2 ! tcx: (:)
   type(child(3,4))                :: b3 ! tcx: (3,4)
   type(badchild(3,3))             :: b4 ! tcx: (3,3)
   class(badchild(:,:)), allocatable  :: b5 ! tcx: (:,:)
   class(child(:,4)), pointer      :: b6 ! tcx: (:,4)

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'dummyArg109kl.1', form='formatted', access='stream' )

   allocate(badchild(3,3)::b1) ! tcx: (3,3)
   allocate(child(3,4)::b2) ! tcx: (3,4)
   b3 = child(3,4)() ! tcx: (3,4)
   b4 = badchild(3,3)() ! tcx: (3,3)
   allocate(badchild(3,3):: b5) ! tcx: badchild(3,3)
   allocate(child(3,4):: b6) ! tcx: child(3,4)

   call b1%readme(1)
   call b2%readme(1)
   call b3%readme(1)
   call b4%readme(1)
   call b5%readme(1)
   call b6%readme(1)

   select type (b1)
      type is (badchild(*,*)) ! tcx: (*,*)
         if (( b1%c /= 'abc' ) .or. ( b1%c1 /= 'ABC' ) ) error stop 1_4
      class default
         error stop 2_4
   end select

   select type (b2)
      type is (child(*,4)) ! tcx: (*,4)
         if (( b2%c /= 'def' ) .or. ( b2%i /= 1234 ) )   error stop 3_4
      class default
         error stop 4_4
   end select

   if (( b3%c /= 'ghi' ) .or. ( b3%i /= 2345 ) )     error stop 5_4
   if (( b4%c /= 'jkl' ) .or. ( b4%c1 /= 'JKL' ) )   error stop 6_4
   if (( b5%c /= 'mno' ) .or. ( b5%c1 /= 'MNO' ) )   error stop 7_4
   if (( b6%c /= 'pqr' ) .or. ( b6%i /= 3456 ) )     error stop 8_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child, badchild

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 9_4
   if ( size(v_list, 1) /= 0 ) error stop 10_4

   select type (dtv)
      type is (child(*,4)) ! tcx: (*,4)
         read (unit, "(A3,1X,I4)", iostat=iostat )  dtv%c, dtv%i
      type is (badchild(*,*)) ! tcx: (*,*)
         read (unit, "(A3,1X,A3)", iostat=iostat )  dtv%c, dtv%c1
      class default
         error stop 11_4
   end select

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 5 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 7 changes
! type: badchild - added parameters (lbc) to invoke with (3,3) / declare with (*,*) - 6 changes
