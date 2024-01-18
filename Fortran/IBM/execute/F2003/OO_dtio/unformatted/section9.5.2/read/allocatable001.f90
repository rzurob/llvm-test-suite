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
! %GROUP: allocatable001.f
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
!*                               - Try input item to be an allocatable
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
         procedure, pass :: setC
   end type
   
contains
   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c      
   end function   
   
   subroutine setC (a, char)
      class(base), intent(inout) :: a
      character(3), intent(in) :: char      
      a%c = char
   end subroutine   
end module


program allocatable001
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
   class(base), allocatable :: b1, b2
   class(base), allocatable  :: b3
   type(base),  allocatable :: b4
   integer :: stat
   character(200) :: msg
   character(3)  :: c1
   character(6)  :: c2
   character(9)  :: c3
   character(12) :: c4
   
   ! allocation of variables
   allocate ( b3, source = base('xxx') )
   allocate ( b4, source = base('XXX') )
   allocate ( b1, source = b3 )
   allocate ( b2, source = b4 )
   
   c1 = 'abc'
   c2 = 'abcdef'
   c3 = 'abcdefghi'
   c4 = 'abcdefghijkl'
   
   open (unit = 1, file ='allocatable001.data', form='unformatted', access='sequential')
      
   ! unformatted I/O operations
   
   write (1, iostat=stat, iomsg=msg )              c1
   write (1, iostat=stat, iomsg=msg )              c2
   write (1, iostat=stat, iomsg=msg )              c3
   write (1, iostat=stat, iomsg=msg )              c4
   
   rewind 1

   read (1, iostat=stat, iomsg=msg )              b1
      if ( ( msg /= 'dtio' ) .or. ( stat /= 0 ) )                           error stop 1_4
      if ( b1%c /= 'abc' )                                                  error stop 2_4
      msg = ''         
   read (1, iostat=stat, iomsg=msg )              b2, b1
      if ( ( msg /= 'dtio' ) .or. ( stat /= 0 ) )                           error stop 3_4
      if ( ( b1%c /= 'def') .or. ( b2%c /= 'abc') )                         error stop 4_4
      msg = ''          
   read (1, iostat=stat, iomsg=msg )              b3, b2, b1
      if ( ( msg /= 'dtio' ) .or. ( stat /= 0 ) )                           error stop 5_4
      if ( ( b1%c /= 'ghi') .or. ( b2%c /= 'def') .or. ( b3%c /= 'abc') )   error stop 6_4
      msg = ''        
   read (1, iostat=stat, iomsg=msg )              b4, b3, b2, b1
      if ( ( msg /= 'dtio' ) .or. ( stat /= 0 ) )                           error stop 7_4
      if ( ( b1%c /= 'jkl') .or. ( b2%c /= 'ghi') .or. ( b3%c /= 'def')     &
         .or. ( b4%c /= 'abc') )                                            error stop 8_4
      msg = '' 
      
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
   
   call dtv%setC(temp)
   
   iomsg = 'dtio'
        
end subroutine
