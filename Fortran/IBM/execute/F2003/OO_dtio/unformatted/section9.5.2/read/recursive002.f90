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
! %GROUP: recursive002.f
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
!*                                        Try linked list data structure with class hierarchy and container with recursive DTIO
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
      integer(4) :: i = -999
   end type

   type :: linkedlist
      class(base), pointer :: head
   end type

   interface read(unformatted)
      subroutine readunformattedll(dtv, unit, iostat, iomsg )
         import linkedlist
         class(linkedlist), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
      recursive subroutine readunformatted(dtv, unit, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program recursive002
   use m

   integer :: stat

   character(200) :: msg = ''
   class(linkedlist), pointer :: ll
   class(base), pointer :: dummy

   allocate ( ll )
   allocate ( ll%head, source = base())
   allocate ( ll%head%next, source = child() )
   allocate ( ll%head%next%next, source = base() )
   allocate ( ll%head%next%next%next, source = child() )
   allocate ( ll%head%next%next%next%next, source = child () )
   	
   open (1, file = 'recursive002.1', form='unformatted', access='sequential' )

   write (1, iostat=stat, iomsg = msg)      'abc', 'def', 1001, 'ghi', 'jkl', 1002, 'mno', 1003
   write (1, iostat=stat, iomsg = msg)      'pqr', 'stu', 1004

   rewind 1

   read (1, iostat=stat, iomsg=msg)       ll
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   dummy => ll%head
   do while (associated(dummy))
      select type ( dummy )
         type is (base)
            print *, "BASE:  ", dummy%c
         type is (child)
            print *, "CHILD: ", dummy%c, dummy%i
      end select            
      dummy => dummy%next
   end do

   deallocate ( ll%head%next%next%next%next )
   deallocate ( ll%head%next%next%next )
   deallocate ( ll%head%next%next )

   ll%head%next%next => null()

   read (1, iostat=stat, iomsg=msg)       ll
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   dummy => ll%head
   do while (associated(dummy))
      select type ( dummy )
         type is (base)
            print *, "BASE:  ", dummy%c
         type is (child)
            print *, "CHILD: ", dummy%c, dummy%i
      end select            
      dummy => dummy%next
   end do

  close (1, status = 'delete' )

end program

subroutine readunformattedll ( dtv, unit, iostat, iomsg )
   use m, only: linkedlist, readunformatted, read(unformatted)

   class(linkedlist), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read(unit, iostat= iostat, iomsg = iomsg ) dtv%head
   if ( ( iostat /= 0 ) .or. ( iomsg /= 'itemread' ) ) error stop 5_4

   iomsg = 'dtioread'

end subroutine

recursive subroutine readunformatted ( dtv, unit, iostat, iomsg )
   use m, only: base, child, read(unformatted)

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is (base)
         read (unit, iostat=iostat )   dtv%c
      type is (child)
         read (unit, iostat=iostat )   dtv%c, dtv%i
   end select

   if ( iostat /= 0  ) error stop 6_4

   if ( associated(dtv%next) ) then
      read(unit, iostat= iostat, iomsg = iomsg ) dtv%next
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'itemread' ) ) error stop 7_4
   end if

   iomsg = 'itemread'

end subroutine
