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
!*                               - Try output item to be an allocatable
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
   class(base), allocatable  :: b3
   type(base),  allocatable :: b4
   integer :: stat
   character(200) :: msg
   character(4)  :: c1
   character(8)  :: c2
   character(12) :: c3
   character(16) :: c4
   
   ! allocation of variables
   allocate ( b3, source = base('ibm') )
   allocate ( b4, source = base('ftn') )
   allocate ( b1, source = b3 )    !<- 'ibm'
   allocate ( b2, source = b4 )    !<- 'ftn'

   
   open (unit = 1, file ='allocatable001.data', form='unformatted', access='sequential')
   
   ! unformatted I/O operations
   
   write (1, iostat=stat, iomsg=msg )             b1                 !<- write 'ibmZ' to file
   write (1, iostat=stat, iomsg=msg )             b1, b2             !<- write 'ibmZftnZ' to file
   write (1, iostat=stat, iomsg=msg )             b1, b2, b3         !<- write 'ibmZftnZibmZ' to file
   write (1, iostat=stat, iomsg=msg )             b1, b2, b3, b4     !<- write 'ibmZftnZibmZftnZ' to file
   
   rewind 1
   
   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4
   
   ! check if the values are set correctly
   
   if ( c1 /= 'ibmZ' )                    error stop 1_4
   if ( c2 /= 'ibmZftnZ' )                error stop 2_4
   if ( c3 /= 'ibmZftnZibmZ' )            error stop 3_4
   if ( c4 /= 'ibmZftnZibmZftnZ' )        error stop 4_4
   
   ! close the file appropriately
   
   close ( 1, status ='delete' )
   
end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z" 
        
end subroutine
