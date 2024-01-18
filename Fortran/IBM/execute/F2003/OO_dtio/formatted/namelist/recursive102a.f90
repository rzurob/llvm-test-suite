!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: recursive102a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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

   type :: base
      class(base), pointer :: next => null()
      character(3) :: c = 'xxx'
   end type

   type, extends(base) :: child
      integer(4) :: i
   end type

   type linkedlist
      class(base), pointer :: head
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
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program recursive102a
   use m

   integer :: stat
   character(200) :: msg = ''

   class(linkedlist), pointer :: l1
   class(linkedlist), allocatable, target :: l2
   class(base), allocatable, target :: b1, b2, b3, b4, b5, b6
   class(base), pointer :: dummy

   namelist /myll/ l1, l2

   open (1, file = 'recursive102a.1', form='formatted', access='stream' )

   allocate(l1,l2)
   allocate(b1, source = base (c='abc'))
   allocate(b2, source = child(c='def',i=1001))
   allocate(b3, source = base (c='ghi'))
   allocate(b4, source = child(c='jkl',i=1002))
   allocate(b5, source = base (c='mno'))
   allocate(b6, source = child(c='pqr',i=1003))

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
         type is ( base )
            print *, dummy%c
         type is ( child )
            print *, dummy%c, dummy%i
      end select
      dummy => dummy%next
   end do

   dummy => l2%head
   do while (associated(dummy))
      select type ( dummy )
         type is ( base )
            print *, dummy%c
         type is ( child )
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
         type is ( base )
            print *, dummy%c
         type is ( child )
            print *, dummy%c, dummy%i
      end select
      dummy => dummy%next
   end do

   dummy => l2%head
   do while (associated(dummy))
      select type ( dummy )
         type is ( base )
            print *, dummy%c
         type is ( child )
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

   class(base), allocatable :: lldummy
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

   class(base), intent(inout) :: dtv
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   class(base), allocatable :: dummy
   namelist /dtionext/ dummy

   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   select type(dtv)
      type is (base)
         read (unit, *, iostat=iostat ) dtv%c
      type is (child)
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
