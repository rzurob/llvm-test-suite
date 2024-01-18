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
! %GROUP: opened001.f
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
!*                               - OPENED= specifier: Try using INQUIRE stmt with OPENED= specifiers in procedures
!*                                                     on unformatted and Standard I/O units
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

   procedure (logical) :: isOpenedByUnit
   procedure (logical) :: isOpenedByFile
   
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


program opened001
   use m1   
   use ISO_FORTRAN_ENV
   
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

   integer, allocatable :: myUnit1, myUnit2, myUnit3

   logical :: opened1, named1
   character(100) :: name1
   
   ! allocation of variables
   
   allocate (b1, b2, b3, b4)
   allocate (myunit1, source=1)
   allocate (myunit2, source=2)
   allocate (myunit3, source=3)
   
   b1%c = 'ibm'

   ! standard units are preconnected to unnamed files
   
   if ( .not. isOpenedByUnit(INPUT_UNIT)  )   error stop 1_4
   if ( .not. isOpenedByUnit(ERROR_UNIT)  )   error stop 2_4
   if ( .not. isOpenedByUnit(OUTPUT_UNIT) )   error stop 3_4
   
   if ( isOpenedByFile('fort.1')  )           error stop 4_4
   if ( isOpenedByUnit(myunit2)  )            error stop 5_4
   if ( isOpenedByUnit(myunit3)  )            error stop 6_4
     
   open (myunit1, file='opened001.1', form='unformatted')
   open (myunit2, access='direct', recl=3)                     !<- by default, this will open file named fort.1
   open (myunit3, file='opened001.3', form='unformatted', access='stream' )

   if ( .not. isOpenedByFile('opened001.1')  )         error stop 7_4
   if ( .not. isOpenedByUnit(myunit2)  )               error stop 8_4
   if ( .not. isOpenedByFile('opened001.3')  )         error stop 9_4
   
   if ( isOpenedByFile('opened001.f')  )               error stop 10_4   !<- file exists but not connected
   
   write (myunit1, iostat=stat1, iomsg=msg1 )        b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) )  error stop 11_4
    
   write (myunit2, iostat=stat1, iomsg=msg1, rec=5 ) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) )  error stop 12_4
   
   write (myunit3, iostat=stat1, iomsg=msg1, pos=2 ) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) )  error stop 13_4

   rewind 1   
   
   read  (myunit1, iostat=stat1, iomsg=msg1 )        b2
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) )   error stop 14_4
   
   read  (myunit2, iostat=stat1, iomsg=msg1, rec=5 ) b3
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) )   error stop 15_4
   
   read  (myunit3, iostat=stat1, iomsg=msg1, pos=2 ) b4
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) )   error stop 16_4
   
   if ( b2%c /= 'ibm' )    error stop 17_4
   if ( b3%c /= 'ibm' )    error stop 18_4
   if ( b4%c /= 'ibm' )    error stop 19_4
 
   ! close the file appropriately
      
   close ( myunit1, status ='delete' )
   close ( myunit2, status ='delete' )
   close ( myunit3, status ='delete' )
   close ( INPUT_UNIT )
   close ( OUTPUT_UNIT )
      
   if ( isOpenedByFile('opened001.1')  )         error stop 20_4
   if ( isOpenedByUnit(myunit2)  )               error stop 21_4
   if ( isOpenedByFile('opened001.3')  )         error stop 22_4 

   if ( isOpenedByUnit(INPUT_UNIT)  )            error stop 23_4
   if ( isOpenedByUnit(OUTPUT_UNIT)  )           error stop 24_4
   
end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( .not. isOpenedByUnit(unit) )   error stop 25_4
        
   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

   iomsg = 'dtio read'
        
end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   character(100) :: file1

   inquire ( unit=unit , name = file1 )
   
   if ( .not. isOpenedByFile(file1) )   error stop 26_4
   
   write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()
   
   iomsg = 'dtio write'
        
end subroutine


logical function isOpenedByUnit( unit ) 
   integer(4) :: unit
   inquire ( unit, opened= isOpenedByUnit )
end function

logical function isOpenedByFile( file ) 
   character(*) :: file
   inquire ( file=file, opened= isOpenedByFile)
end function