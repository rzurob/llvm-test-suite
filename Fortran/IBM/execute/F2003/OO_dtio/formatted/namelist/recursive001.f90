!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: recursive001.f
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

   interface write(formatted)
      recursive subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program recursive001
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), pointer :: head
   class(base), allocatable, target :: b1, b2, b3, b4, b5, b6
   namelist /linkedlist/ head

   open (1, file = 'recursive001.1', form='formatted', access='stream' )

   allocate(b1, source = base(c='abc'))
   allocate(b2, source = base(c='def'))
   allocate(b3, source = base(c='ghi'))
   allocate(b4, source = base(c='jkl'))
   allocate(b5, source = base(c='mno'))
   allocate(b6, source = base(c='pqr'))

   ! first linked list
   b1%next => b2
   b2%next => b3

   ! second linked list
   b4%next => b5
   b5%next => b6

   head => b1

   write (1, linkedlist, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   head => b4

   write (1, linkedlist, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   head => b5

   write (1, linkedlist, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end program


recursive subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, write(formatted)

   class(base), intent(in) :: dtv
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   write (unit, *, iostat=iostat )   dtv%c
   if ( iostat /= 0  ) error stop 4_4

   if ( associated(dtv%next) ) then
      write(unit, *, iostat= iostat, iomsg = iomsg ) dtv%next
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowrite' ) ) error stop 5_4
   end if

   iomsg = 'dtiowrite'

end subroutine
