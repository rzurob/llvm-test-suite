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
! %GROUP: dummyArg001a.f
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
!*                               - Try input item to be an scalar dummy argument of pointer/allocatable 
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
         procedure, pass :: getC
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
   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c      
   end function   

   subroutine myRead(unit, stat, msg, a, b )
      class(base), intent(inout), allocatable :: a
      class(base), intent(inout), optional, pointer :: b
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg
      
      if (.not. present(b) ) then
         read(unit, iostat=stat, iomsg=msg) a
      else
      	 read(unit, iostat=stat, iomsg=msg) a,b
      end if       
   end subroutine

end module

program dummyArg001a
   use m1   
  
   ! declaration of variables
   class(base), allocatable :: b1
   class(base), pointer     :: b2

   integer :: stat
   character(200) :: msg
   character(4)  :: c1
   character(8)  :: c2
   
   ! allocation of variables
   allocate ( b1, source = base('xxx') )
   allocate ( b2, source = base('xxx') )
   
   open (unit = 1, file ='dummyArg001a.data', form='unformatted', access='sequential')
   
   ! unformatted I/O operations
   
   write ( 1, iostat = stat, iomsg = msg )                'ibm'
   write ( 1, iostat = stat, iomsg = msg )                'abcdef'
   
   rewind 1
   
   call myRead (1, stat, msg, b1 )
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 1_4
      msg = ''
      if ( b1%c /= 'ibm' )                                              error stop 2_4
   call myRead (1, stat, msg, b1, b2 )
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 3_4
      msg = ''   
      if ( ( b1%c /= 'abc' ) .or. ( b2%c /= 'def' ) )                   error stop 4_4
     
   ! close the file appropriately
   
   close ( 1, status ='delete' )
   
end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(3) :: temp 
   read (unit, iostat=iostat ) temp
   
   dtv%c = temp
   
   iomsg = 'dtio'

end subroutine