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
! %GROUP: recursive001a.f
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
!*  DESCRIPTION                : Testing: Section 9.5.2 Output Statement
!*                                        Try linked list data structure with recursive DTIO with class hierarchy
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

    interface read(unformatted)
        module procedure readunformatted
    end interface

contains

   recursive subroutine readunformatted ( dtv, unit, iostat, iomsg )

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

      if ( iostat /= 0 ) error stop 7_4

      if ( associated(dtv%next) ) then
         read(unit, iostat= iostat, iomsg = iomsg ) dtv%next
         if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioread' ) ) error stop 8_4
      end if

      iomsg = 'dtioread'

   end subroutine

end module

program recursive001a
   use m

   integer :: stat

   character(200) :: msg = ''
   class(base), pointer :: head
   class(base), allocatable, target :: b1, b2, b3, b4, b5, b6

   open (1, file = 'recursive001a.1', form='unformatted', access='sequential' )

   allocate(b1, source = base ())
   allocate(b2, source = child())
   allocate(b3, source = base ())
   allocate(b4, source = child())
   allocate(b5, source = base ())
   allocate(b6, source = child())

   ! first linked list
   b1%next => b2
   b2%next => b3

   ! second linked list
   b4%next => b5
   b5%next => b6

   head => b1

   write ( 1, iostat=stat)   'abc','def',101,'ghi'
   if ( stat /= 0 ) error stop 1_4

   write ( 1, iostat=stat)   'jkl',102,'mno','pqr',103
   if ( stat /= 0 ) error stop 2_4

   write ( 1, iostat=stat)   'stu','vwx',104
   if ( stat /= 0 ) error stop 3_4

   rewind 1

   read (1, iostat=stat, iomsg=msg)       head
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4
   
   select type ( b2 )
      type is (child)
         if (( b1%c /= 'abc' ) .or. ( b2%c /= 'def' ) .or. ( b2%i /= 101 ) .or. ( b3%c /= 'ghi' ) ) error stop 5_4
   end select   
   
   head => b4

   read (1, iostat=stat, iomsg=msg)       head
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 6_4

   select type ( b4 )
      type is (child)
         select type ( b6 )
            type is (child)
               if (( b4%c /= 'jkl' ) .or. ( b4%i /= 102 ) .or. ( b5%c /= 'mno' ) .or. ( b6%i /= 103 ) .or. ( b6%c /= 'pqr' ) ) error stop 7_4
         end select
   end select   
   
   head => b5

   read (1, iostat=stat, iomsg=msg)       head
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 8_4
   
   select type ( b6 )
      type is (child)
         if ( ( b5%c /= 'stu' ) .or. ( b6%i /= 104 ) .or. ( b6%c /= 'vwx' ) ) error stop 9_4
   end select

   close (1, status = 'delete' )

end program

