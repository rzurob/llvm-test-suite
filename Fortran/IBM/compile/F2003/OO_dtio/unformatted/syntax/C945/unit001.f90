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
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp unit001.f
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
!*  DESCRIPTION                : Testing: Section 9.8: FLUSH statement
!*                               - Try FLUSH statement without specifying unit #
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


program unit001
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
   
   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface
  
   ! declaration of variables
   class(base), allocatable :: b1, b2
   integer :: stat1, stat2
   character(200) :: msg1, msg2
   
   ! allocation of variables
   
   allocate (b1,b2)
   
   b1%c = 'ibm'
   b2%c = ''
   
   ! I/O operations   
   
   FLUSH (iostat=stat1)   !<- flush a file that does not exit and specify no unit #
   
   open (unit = 1, file ='unit001.data', form='unformatted', access='sequential')
   
   write (1, iostat=stat1, iomsg = msg1)    b1
   
   if ( ( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) )           error stop 1_4
   
   FLUSH (iomsg=msg1)     !<- flush a file that exists and specify no unit #
   
   rewind 1
   
   read (1, iostat=stat1, iomsg = msg1)    b2
   
   if ( ( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) )            error stop 2_4
   
   if ( b2%c /= 'ibm' ) error stop 3_4
      
   ! close the file appropriately
   
   close ( 1, status ='delete' )
   
end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, iostat=iostat, iomsg=iomsg ) dtv%c    
    
    FLUSH (iostat=iostat, iomsg = iomsg)   !<- flush inside DTIO and specify no unit #
    
    iomsg = 'dtio read'
        
end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()
    
    FLUSH (iostat=iostat, iomsg=iomsg )   !<- flush inside DTIO and specify no unit #
    
    iomsg = 'dtio write'
        
end subroutine
