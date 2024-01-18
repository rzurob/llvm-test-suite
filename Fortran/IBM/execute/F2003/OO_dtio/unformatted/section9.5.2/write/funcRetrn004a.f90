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
! %GROUP: funcRetrn004a.f
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
!*                               - Try output item to be a non-polymorphic array function return (pointer, allocatable, and neither)
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
   end type

   type, extends(base) :: child
      character(3) :: cc = ''
   end type

contains

   function getNewBase(c)
      type(base) :: getNewBase(2)
      character(3) :: c
      getNewBase = base(c)
   end function
   
   function getNewChild(c)
      type(child) :: getNewChild(3)
      character(6) :: c
      getNewChild = child(c(1:3),c(4:6))
   end function

end module

program funcRetrn004a
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
      function getNewBasePtr(c)
         import base
         type(base), pointer :: getNewBasePtr(:)
         character(3) :: c
      end function
   end interface
   
   interface   
      function getNewChildAlloc(c)
         import child
         type(child), allocatable :: getNewChildAlloc(:)
         character(6) :: c
      end function
   end interface
   
   ! declaration of variables
   integer :: stat
   character(200) :: msg
   character(6)   :: c1
   character(18)  :: c2
   character(6)   :: c3
   character(12)  :: c4
    
   open (unit = 1, file ='funcRetrn004a.data', form='unformatted', access='sequential')
   
   ! unformatted I/O operations
   
   write (1, iostat=stat, iomsg=msg )             getNewBase('abc')
   write (1, iostat=stat, iomsg=msg )             getNewChild('defghi')
   write (1, iostat=stat, iomsg=msg )             getNewBasePtr('jkl')
   write (1, iostat=stat, iomsg=msg )             getNewChildAlloc('mnopqr')
   
   rewind 1
   
   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4
   
   ! check if the values are set correctly

   if ( c1 /= 'abcabc' )                   error stop 1_4
   if ( c2 /= 'defghidefghidefghi' )       error stop 2_4
   if ( c3 /= 'jkljkl' )                   error stop 3_4
   if ( c4 /= 'mnopqrpqrmno' )             error stop 4_4
   
   ! close the file appropriately
   
   close ( 1, status ='delete' )
  
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

function getNewBasePtr(c)
   use m1
   type(base), pointer :: getNewBasePtr(:)
   character(3) :: c
   allocate ( getNewBasePtr(2), source = (/ base(c), base(c) /) )
end function
   
function getNewChildAlloc(c)
   use m1
   type(child), allocatable :: getNewChildAlloc(:)
   character(6) :: c
   allocate(getNewChildAlloc(2), source = (/ child(c(1:3),c(4:6)), child(c(4:6),c(1:3)) /) )
 end function
