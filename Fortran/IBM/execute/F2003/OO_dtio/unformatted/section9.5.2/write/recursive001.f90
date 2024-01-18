! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2
!*                                        Try linked list data structure with recursive DTIO
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
      type(base), pointer :: next => null()
      character(3) :: c
   end type

    interface write(unformatted)
        module procedure writeunformatted
    end interface

contains

   recursive subroutine writeunformatted ( dtv, unit, iostat, iomsg )

      class(base), intent(in) :: dtv
      integer, intent(in) :: unit
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write (unit, iostat=iostat )   dtv%c
      if ( iostat /= 0  ) error stop 7_4

      if ( associated(dtv%next) ) then
         write(unit, iostat= iostat, iomsg = iomsg ) dtv%next
         if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowrite' ) ) error stop 8_4
      end if

      iomsg = 'dtiowrite'

   end subroutine

end module

program recursive001
   use m

   integer :: stat
   character(9) :: c1, c2
   character(6) :: c3
   character(200) :: msg = ''
   class(base), pointer :: head
   class(base), allocatable, target :: b1, b2, b3, b4, b5, b6

   open (1, file = 'recursive001.1', form='unformatted', access='stream' )

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

   write (1, iostat=stat, iomsg=msg)       head
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   head => b4

   write (1, iostat=stat, iomsg=msg)       head
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   head => b5

   write (1, iostat=stat, iomsg=msg)       head
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   rewind 1

   read (1, iostat=stat, iomsg = msg)      c1
   if ( ( c1 /= 'abcdefghi' ) )                     error stop 4_4

   read (1, iostat=stat, iomsg = msg)      c2
   if ( ( c2 /= 'jklmnopqr' ) )                     error stop 5_4

   read (1, iostat=stat, iomsg = msg)      c3
   if ( ( c3 /= 'mnopqr' ) )                        error stop 6_4

   close (1, status = 'delete' )

end program
