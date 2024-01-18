!#######################################################################
! SCCS ID Information
! OO_dtio/unformatted/section9.5.2/read/array001a.f, xlftest.OO_dtio, tstdev, 1.2
! Extract Date/Time: 05/06/15 10:09:31
! Checkin Date/Time: 05/04/06 10:22:11
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: array001a.f
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
!*                               - Try input item to be polymorphic arrays with array section
!*                                 - vector subscripts, elements (with class hierarchy and abstract type)
!*                               Stream Access
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
   type, abstract :: base
      character(3) :: c = ''
      contains
         procedure, pass :: getC => getC
         procedure, pass :: setC
   end type

   type, extends(base) :: child
      character(3) :: cc = ''
      contains
         procedure, pass :: getC => getCC
   end type

contains

   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c
   end function

   function getCC (a)
      class(child), intent(in) :: a
      character(3) :: getCC
      getCC = a%cc
   end function

   subroutine setC (a, char)
      class(base), intent(inout) :: a
      character(3), intent(in) :: char
      a%c = char
   end subroutine

end module


program array001a
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base) , allocatable :: b1(:)
   class(child) , allocatable :: b2(:,:)
   class(base) , pointer     :: b3(:)
   class(child), pointer     :: b4(:,:)
   integer :: stat
   character(200) :: msg

   ! allocation of variables
   allocate ( b1(4), source = (/ child('xxx','XXX'), child('xxx','XXX'), child('xxx','XXX'), child('xxx','XXX') /) )
   allocate ( b2(2,2), source = reshape (source = (/ child('XXX','xxx'), child('XXX','xxx'), child('XXX','xxx'), child('XXX','xxx')  /), shape=(/2,2/) ) )
   allocate ( b3(4), source = (/ child('xxx', 'XXX'), child('xxx', 'XXX'), child('xxx', 'XXX'), child('xxx','XXX') /) )
   allocate ( b4(2,2), source = reshape (source = (/ child('xxx', 'xxx'), child('xxx','xxx'), child('xxx', 'xxx'), child('xxx','xxx')  /), shape=(/2,2/) ) )

   open (unit = 1, file ='array001a.data', form='unformatted', access='stream')

   ! unformatted I/O operations
   
   write (1, iostat=stat, iomsg=msg, pos=31 )             'ghiGHIdefDEFjklJKLabcABC'
   write (1, iostat=stat, iomsg=msg, pos=19 )             'ABCabcDEFdef'
   write (1, iostat=stat, iomsg=msg, pos=13 )             'stuSTU'
   write (1, iostat=stat, iomsg=msg, pos=1  )             'MNOmnoSTUstu'

   read (1, iostat=stat, iomsg=msg, pos=31 )              b1((/3,2,4,1/))
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 1_4
      msg = ''
   read (1, iostat=stat, iomsg=msg, pos=19 )              b2(1:2, 1)
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 2_4
      msg = ''
   read (1, iostat=stat, iomsg=msg, pos=13 )              b3(3)
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 3_4
      msg = ''
   read (1, iostat=stat, iomsg=msg, pos=1 )              b4(1:2:2, 1:2:1)
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 4_4
      msg = ''

   ! check if the values are set correctly
   
   if ( ( b1(1)%c /= 'abc' ) .or. ( b1(1)%getC() /= 'ABC' ) .or.            &       !<- getC() invokes getcc()
        ( b1(2)%c /= 'def' ) .or. ( b1(2)%getC() /= 'DEF' ) .or.            &
        ( b1(3)%c /= 'ghi' ) .or. ( b1(3)%getC() /= 'GHI' ) .or.            &
        ( b1(4)%c /= 'jkl' ) .or. ( b1(4)%getC() /= 'JKL' ) )               error stop 5_4

   if ( ( b2(1,1)%c /= 'ABC' ) .or. ( b2(1,1)%getC() /= 'abc' ) .or.        &
        ( b2(2,1)%c /= 'DEF' ) .or. ( b2(2,1)%getC() /= 'def' ) .or.        &
        ( b2(1,2)%c /= 'XXX' ) .or. ( b2(1,2)%getC() /= 'xxx' ) .or.        &
        ( b2(2,2)%c /= 'XXX' ) .or. ( b2(2,2)%getC() /= 'xxx' ) )           error stop 6_4

   if ( ( b3(1)%c /= 'xxx' ) .or. ( b3(1)%getC() /= 'XXX' ) .or.            &
        ( b3(2)%c /= 'xxx' ) .or. ( b3(2)%getC() /= 'XXX' ) .or.            &
        ( b3(3)%c /= 'stu' ) .or. ( b3(3)%getC() /= 'STU' ) .or.            &
        ( b3(4)%c /= 'xxx' ) .or. ( b3(4)%getC() /= 'XXX' ) )               error stop 7_4

   if ( ( b4(1,1)%c /= 'MNO' ) .or. ( b4(1,1)%getC() /= 'mno' ) .or.        &
        ( b4(2,1)%c /= 'xxx' ) .or. ( b4(2,1)%getC() /= 'xxx' ) .or.        &
        ( b4(1,2)%c /= 'STU' ) .or. ( b4(1,2)%getC() /= 'stu' ) .or.        &
        ( b4(2,2)%c /= 'xxx' ) .or. ( b4(2,2)%getC() /= 'xxx' ) )           error stop 8_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(3) :: temp

   read (unit, iostat=iostat ) temp

   if ( iostat /= 0 ) error stop 9_4

   call dtv%setC(temp)

   select type (dtv)
      type is (child)
         read (unit, iostat=iostat ) dtv%cc
   end select

   iomsg = 'dtio'

end subroutine
