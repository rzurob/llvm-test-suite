! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : recursive102akl
!*
!*  DATE                       : 2007-07-06 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2
!*                                        Try linked list data structure with recursive DTIO with namelist formatting
!*                                        and namelist formatting inside DTIO with class hierarchy and container structure
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
      class(base(:)), pointer :: next => null() ! tcx: (:)
      character(lb) :: c = 'xxx'
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: i
   end type

   type linkedlist
      class(base(:)), pointer :: head ! tcx: (:)
   end type

   interface read(formatted)
      subroutine readformattedll(dtv, unit, iotype, v_list, iostat, iomsg )
         import linkedlist
         class(linkedlist), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
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

end module

program recursive102akl
   use m

   integer :: stat
   character(200) :: msg = ''

   class(linkedlist), pointer :: l1
   class(linkedlist), allocatable, target :: l2
   class(base(:)), allocatable, target :: b1, b2, b3, b4, b5, b6 ! tcx: (:)
   class(base(:)), pointer :: dummy ! tcx: (:)

   namelist /myll/ l1, l2

   open (1, file = 'recursive102akl.1', form='formatted', access='stream' )

   allocate(l1,l2)
   allocate(b1, source = base(3) (c='abc')) ! tcx: (3)
   allocate(b2, source = child(3,4)(c='def',i=1001)) ! tcx: (3,4)
   allocate(b3, source = base(3) (c='ghi')) ! tcx: (3)
   allocate(b4, source = child(3,4)(c='jkl',i=1002)) ! tcx: (3,4)
   allocate(b5, source = base(3) (c='mno')) ! tcx: (3)
   allocate(b6, source = child(3,4)(c='pqr',i=1003)) ! tcx: (3,4)

   ! first linked list
   b1%next => b2
   b2%next => b3

   ! second linked list
   b4%next => b5
   b5%next => b6

   l1%head => b1
   l2%head => b4

   read (1, myll, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'linkedlistread' ) ) error stop 1_4

   dummy => l1%head
   do while (associated(dummy))
      select type ( dummy )
         type is ( base(*) ) ! tcx: (*)
            print *, dummy%c
         type is ( child(*,4) ) ! tcx: (*,4)
            print *, dummy%c, dummy%i
      end select
      dummy => dummy%next
   end do

   dummy => l2%head
   do while (associated(dummy))
      select type ( dummy )
         type is ( base(*) ) ! tcx: (*)
            print *, dummy%c
         type is ( child(*,4) ) ! tcx: (*,4)
            print *, dummy%c, dummy%i
      end select
      dummy => dummy%next
   end do

   l2%head => b5
   l1 => l2

   read (1, myll, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'linkedlistread' ) ) error stop 2_4

   dummy => l1%head
   do while (associated(dummy))
      select type ( dummy )
         type is ( base(*) ) ! tcx: (*)
            print *, dummy%c
         type is ( child(*,4) ) ! tcx: (*,4)
            print *, dummy%c, dummy%i
      end select
      dummy => dummy%next
   end do

   dummy => l2%head
   do while (associated(dummy))
      select type ( dummy )
         type is ( base(*) ) ! tcx: (*)
            print *, dummy%c
         type is ( child(*,4) ) ! tcx: (*,4)
            print *, dummy%c, dummy%i
      end select
      dummy => dummy%next
   end do
end program

subroutine readformattedll(dtv, unit, iotype, v_list, iostat, iomsg )
   use m, only: base, linkedlist, read(formatted), readformatted

   class(linkedlist), intent(inout) :: dtv
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   class(base(:)), allocatable :: lldummy ! tcx: (:)
   namelist /linkedlist/ lldummy

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   allocate( lldummy, source = dtv%head )

   read ( unit, linkedlist, iostat = iostat, iomsg = iomsg )

   if ( ( iostat /= 0 ) .or. ( iomsg /= 'itemread' ) ) error stop 5_4

   iomsg = 'linkedlistread'

end subroutine

recursive subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child, read(formatted)

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   class(base(:)), allocatable :: dummy ! tcx: (:)
   namelist /dtionext/ dummy

   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   select type(dtv)
      type is (base(*)) ! tcx: (*)
         read (unit, *, iostat=iostat ) dtv%c
      type is (child(*,4)) ! tcx: (*,4)
         read (unit, *, iostat=iostat ) dtv%c, dtv%i
   end select

   if ( iostat /= 0  ) error stop 8_4

   if ( associated(dtv%next) ) then
      allocate(dummy, source = dtv%next)
      read(unit, dtionext, iostat= iostat, iomsg = iomsg )
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'itemread' ) ) error stop 9_4
   end if

   iomsg = 'itemread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 16 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 8 changes
