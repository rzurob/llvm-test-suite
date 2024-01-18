!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 9.5.2
!*                                        Try linked list data structure with recursive DTIO in Read
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
      character(3) :: c = 'xxx'
   end type

    interface read(unformatted)
        module procedure readunformatted
    end interface

contains

   recursive subroutine readunformatted ( dtv, unit, iostat, iomsg )

      class(base), intent(inout) :: dtv
      integer, intent(in) :: unit
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      read (unit, iostat=iostat )   dtv%c
      if ( iostat /= 0  ) error stop 7_4

      if ( associated(dtv%next) ) then
         read(unit, iostat= iostat, iomsg = iomsg ) dtv%next
         if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioread' ) ) error stop 8_4
      end if

      iomsg = 'dtioread'

   end subroutine

end module

program recursive001
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), pointer :: head
   class(base), allocatable, target :: b1, b2, b3, b4, b5, b6

   open (1, file = 'recursive001.1', form='unformatted', access='stream' )

   allocate(b1, source = base())
   allocate(b2, source = base())
   allocate(b3, source = base())
   allocate(b4, source = base())
   allocate(b5, source = base())
   allocate(b6, source = base())

   ! first linked list
   b1%next => b2
   b2%next => b3

   ! second linked list
   b4%next => b5
   b5%next => b6

   write (1, iostat=stat, iomsg = msg)      'abcdefghi'

   write (1, iostat=stat, iomsg = msg)      'jklmnopqr'

   write (1, iostat=stat, iomsg = msg)      'stuvwx'

   rewind 1

   head => b1

   read (1, iostat=stat, iomsg=msg)       head
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   if (( b1%c /= 'abc' ) .or. ( b2%c /= 'def' ) .or. ( b3%c /= 'ghi' ) ) error stop 2_4

   head => b4

   read (1, iostat=stat, iomsg=msg)       head
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   if (( b4%c /= 'jkl' ) .or. ( b5%c /= 'mno' ) .or. ( b6%c /= 'pqr' ) ) error stop 4_4

   head => b5

   read (1, iostat=stat, iomsg=msg)       head
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 5_4
   if (( b5%c /= 'stu' ) .or. ( b6%c /= 'vwx' ) )  error stop 6_4
   close (1, status = 'delete' )

end program
