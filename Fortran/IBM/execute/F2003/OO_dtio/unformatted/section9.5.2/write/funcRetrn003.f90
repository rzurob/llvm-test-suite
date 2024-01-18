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
! %GROUP: funcRetrn003.f
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
!*                               - Try output item to be a function return (type bound, 
!*                               Sequential Access
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
      contains
      procedure, pass :: returnMyself
   end type

   type, extends(base) :: child
      character(3) :: cc = ''
   end type

   contains

   function returnMyself (dtv)
      class(base), intent(in) :: dtv
      class(base), allocatable :: returnMyself

      allocate( returnMyself, source = dtv )

   end function

end module

program funcRetrn003
   use m1

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface
      function foo()
      import base
         class(base), allocatable :: foo
       end function
   end interface

   ! declaration of variables
   integer :: stat
   character(200) :: msg
   character(3)   :: c1
   character(6)   :: c2, c3, c4, c5

   class(base), allocatable  :: b1
   class(base), pointer      :: b2
   class(child), allocatable :: b3

   ! allocation of variables

   allocate(b1, source = base ('abc')      )
   allocate(b2, source = child('def','ghi'))
   allocate(b3, source = child('jkl','mno'))

   open (unit = 1, file ='funcRetrn003.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )     b1%returnMyself()
   write (1, iostat=stat, iomsg=msg )     b2%returnMyself()
   write (1, iostat=stat, iomsg=msg )     b3%returnMyself()
   write (1, iostat=stat, iomsg=msg )     bar()
   write (1, iostat=stat, iomsg=msg )     foo()

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4
   read (1, iostat=stat, iomsg=msg )              c5

   ! check if the values are set correctly

   if ( c1 /= 'abc' )                 error stop 1_4
   if ( c2 /= 'defghi' )              error stop 2_4
   if ( c3 /= 'jklmno' )              error stop 3_4
   if ( c4 /= 'pqrstu' )              error stop 4_4
   if ( c5 /= 'vwxxyz' )              error stop 5_4

   ! close the file appropriately

   close ( 1, status ='delete' )

contains
   function bar()
      class(base), allocatable :: bar
      allocate(bar, source = child('pqr','stu') )
   end function
end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is (base)
         write (unit, iostat=iostat) dtv%c
      type is (child)
         write (unit, iostat=iostat) dtv%c, dtv%cc
   end select
   
   iomsg = 'dtiowrite'

end subroutine

function foo()
use m1
   class(base), allocatable :: foo
   allocate(foo, source =  child('vwx','xyz') )
end function
