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
! %GROUP: dummyArg005.f
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
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be an non-polymorphic array dummy argument
!*                               stream access
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type base
      character(3) :: c = ''
   end type
   type, extends(base) :: child
      character(3) :: cc = ''
   end type

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

contains

   subroutine myread(unit, stat, msg, a, b )
      type(base), intent(inout)  :: a(:)
      type(child), intent(inout) :: b(:)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      read(unit, iostat=stat, iomsg=msg) a,b
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   end subroutine

   subroutine myread1(unit, stat, msg, a, b )
      type(base), intent(inout)  :: a(2,*)
      type(child), intent(inout) :: b(2,2,*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      read(unit, iostat=stat, iomsg=msg) a(1:2,1:2), b(1:2,1:2,1:2)
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   end subroutine

end module

program dummyArg005
   use m1

   ! declaration of variables
   integer :: stat
   character(200) :: msg
   character(18)  :: cc1
   character(30)  :: cc2
   character(60)  :: cc3

   type(base)  :: b1(2) = (/ base('xxx'), base('xxx') /)
   type(base), pointer :: b2(:)
   type(base), allocatable :: b3(:,:)

   type(child) :: c1(2) = (/ child('xxx','xxx'), child('xxx','xxx') /)
   type(child), pointer :: c2(:)
   type(child), allocatable :: c3(:,:,:)

   allocate( b2(2) , source = b1 )
   allocate( b3(2,2), source = reshape ( source = (/ b1, b1 /) , shape = (/2,2/) ) )
   allocate( c2(4), source = reshape ( source = (/ c1, child('xxx','xxx'), child('xxx','xxx') /), shape = (/4/) ) )
   allocate( c3(2,2,2), source = reshape ( source = (/ c2, c2 /), shape = (/2,2,2/) ) )

   cc1 = 'ABCDEFabcdefghijkl'
   cc2 = 'abcdefABCDEFGHIJKLMNOPQRSTUVWX'
   cc3 = 'ABCDEFGHIJKLabcdefghijklmnopqrstuvwxabcdefghijklmnopqrstuvwx'

   open (unit = 1, file ='dummyArg005.data', form='unformatted', access='stream')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )              cc1
   write (1, iostat=stat, iomsg=msg )              cc2
   write (1, iostat=stat, iomsg=msg )              cc3

   rewind 1

   call myread  (1, stat, msg, b1, c1 )
   call myread  (1, stat, msg, b2, c2 )
   call myread1 (1, stat, msg, b3, c3 )

   print *, b1(1)%c
   print *, b1(2)%c
   print *, c1(1)%c
   print *, c1(1)%cc
   print *, c1(2)%c
   print *, c1(2)%cc

   print *, b2(1)%c
   print *, b2(2)%c
   print *, c2(1)%c
   print *, c2(1)%cc
   print *, c2(2)%c
   print *, c2(2)%cc
   print *, c2(3)%c
   print *, c2(3)%cc
   print *, c2(4)%c
   print *, c2(4)%cc
   
   print *, b3(1,1)%c
   print *, b3(2,1)%c
   print *, b3(1,2)%c
   print *, b3(2,2)%c
   print *, c3


   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( g => dtv )
      type is (base)
         read (unit, iostat=iostat ) g%c
      type is (child)
         read (unit, iostat=iostat ) g%c, g%cc
   end select
   iomsg = 'dtioread'
end subroutine
