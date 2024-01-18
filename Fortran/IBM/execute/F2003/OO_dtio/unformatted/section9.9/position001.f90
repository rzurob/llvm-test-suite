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
! %GROUP: position001.f
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
!*  DESCRIPTION                : Testing: Secition 9.9 INQUIRE Statement
!*                               - POSITION= specifier: Try using INQUIRE stmt with POSITION= specifier in procedures
!*                                                     on unformatted I/O units
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
   
   procedure(character(10)) :: getPositionByFile
   procedure(character(10)) :: getPositionByUnit
   
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


program position001
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
   class(base), allocatable :: b1, b2, b3, b4
   integer :: stat1
   character(200) :: msg1

   integer, allocatable :: myunit1
   integer, allocatable :: myunit2
   integer, allocatable :: myunit3

   ! allocation of variables
   
   allocate (b1,b2,b3,b4)
   allocate (myunit1, source=1)
   allocate (myunit2, source=2)
   allocate (myunit3, source=3)
   
   b1%c = 'ibm'

   if ( getPositionByUnit(myunit1)         /= 'aUNDEFINED' )   error stop 1_4  !<- disconnected unit
   if ( getPositionByFile('position001.f') /= 'aUNDEFINED' )   error stop 2_4  !<- disconnected file
   
   open (myunit1, file='position001.1', form='unformatted', access='sequential', status='replace' )
   open (myunit2, file='position001.2', form='unformatted', access='direct', recl=3, status='replace' )
   open (myunit3, file='position001.3', form='unformatted', access='stream', position='rewind', status='replace' )
   
   if ( getPositionByUnit(myunit1) /= 'aASIS' )                error stop 3_4   !<- default position is ASIS  
   if ( getPositionByFile('position001.2') /= 'aUNDEFINED' )   error stop 4_4   !<- direct access is always undefined in position
   if ( getPositionByUnit(myunit3) /= 'aREWIND' )              error stop 5_4   !<- explicitly set to 'REWIND' in open statement
   
   ! I/O operations
   
   write (myunit1, iostat=stat1, iomsg=msg1 )         b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'aAPPEND' ) )             error stop 6_4

   write (myunit2, iostat=stat1, iomsg=msg1, rec=1 )  b1 
   if (( stat1 /= 0 ) .or. ( msg1 /= 'aUNDEFINED' ) )          error stop 7_4
   
   write (myunit3, iostat=stat1, iomsg=msg1, pos=4 )  b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'aAPPEND' ) )             error stop 8_4
   
   write (myunit3, iostat=stat1, iomsg=msg1, pos=1 )  b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'aASIS' ) )               error stop 9_4  
   
   rewind 1
   
   read  (myunit1, iostat=stat1, iomsg=msg1 )          b2
   if (( stat1 /= 0 ) .or. ( msg1 /= 'aASIS' ) )               error stop 10_4   !<- aASIS because inside DTIO, end of file record was not reached
  
   read  (myunit2, iostat=stat1, iomsg=msg1,rec=1 )    b3
   if (( stat1 /= 0 ) .or. ( msg1 /= 'aUNDEFINED' ) )          error stop 11_4  
   
   read  (myunit3, iostat=stat1, iomsg=msg1, pos=2 )   b4
   if (( stat1 /= 0 ) .or. ( msg1 /= 'aASIS' ) )               error stop 12_4      

   backspace 1
   rewind 3
   
   if ( getPositionByUnit(myunit1) /= 'aREWIND' )              error stop 13_4
   if ( getPositionByFile('position001.3') /= 'aREWIND' )      error stop 14_4
   
   if ( b2%c /= 'ibm' ) error stop 15_4
   if ( b3%c /= 'ibm' ) error stop 16_4
   if ( b4%c /= 'bmi' ) error stop 17_4         
   
   ! close the file appropriately
   
   close ( myunit1, status ='delete' )
   close ( myunit2, status ='delete' )
   close ( myunit3, status ='delete' )
   
end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   character(20) :: name1
   inquire (unit, name=name1)
   
   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c
   
   iomsg = getPositionByFile(name1)
        
end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()
    
   if ( iostat /= 0 ) error stop 8_4
    
   FLUSH (unit, iostat=iostat, iomsg=iomsg)
   
   iomsg = getPositionByUnit(unit)
        
end subroutine

character(10) function getPositionByFile(file)
   character(*), intent(in) :: file
   integer :: stat
   character(9) :: position1
   inquire ( file=file, position=position1, iostat=stat )
      
   if ( stat /= 0 ) error stop 10_4
     
   if ( position1 .eq. 'REWIND' ) then
      getPositionByFile = 'aREWIND'
   else if ( position1 .eq. 'APPEND' ) then
      getPositionByFile = 'aAPPEND'
   else if ( position1 .eq. 'ASIS' ) then
      getPositionByFile = 'aASIS'
   else if ( position1 .eq. 'UNDEFINED' ) then
      getPositionByFile = 'aUNDEFINED'
   else
      getPositionByFile = 'error'
   end if      
end function

character(10) function getPositionByUnit(unit)
   integer, intent(in) :: unit
   integer :: stat
   character(9) :: position1
   inquire ( unit, position=position1, iostat=stat )
     
   if ( stat /= 0 ) error stop 10_4
   
   if ( position1 .eq. 'REWIND' ) then
      getPositionByUnit = 'aREWIND'
   else if ( position1 .eq. 'APPEND' ) then
      getPositionByUnit = 'aAPPEND'
   else if ( position1 .eq. 'ASIS' ) then
      getPositionByUnit = 'aASIS'
   else if ( position1 .eq. 'UNDEFINED' ) then
      getPositionByUnit = 'aUNDEFINED'
   else
      getPositionByUnit = 'error'
   end if      
end function

