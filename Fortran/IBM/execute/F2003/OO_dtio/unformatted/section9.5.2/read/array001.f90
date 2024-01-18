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
! %GROUP: array001.f
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
!*                               - Try input item to be an allocatable array with array section
!*                                 - vector subscripts, elements
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


program array001
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
   class(base), allocatable     :: b1(:)
   class(base), pointer         :: b2(:,:)
   type(base) :: b3(4)
   type(base),  pointer :: b4(:,:)
   integer :: stat
   character(200) :: msg
   
   ! allocation of variables
   allocate ( b1(4), source = (/ base('xxx'), base('xxx'), base('xxx'), base('xxx') /) )
   allocate ( b2(2,2), source = reshape (source = (/ base('XXX'), base('XXX'), base('XXX'), base('XXX')  /), shape=(/2,2/) ) )
   b3 = (/ base('xxx'), base('xxx'), base('xxx'), base('xxx') /)
   allocate ( b4(2,2), source = reshape (source = (/ base('XXX'), base('XXX'), base('XXX'), base('XXX')  /), shape=(/2,2/) ) )
      
   open (unit = 1, file ='array001.data', form='unformatted', access='sequential')
   
   ! unformatted I/O operations
   
   write (1, iostat=stat, iomsg=msg )              'ghidefjklabc'
   write (1, iostat=stat, iomsg=msg )              'ABCDEF'
   write (1, iostat=stat, iomsg=msg )              'stuff'
   write (1, iostat=stat, iomsg=msg )              'MNOSTU'

   rewind 1

   read (1, iostat=stat, iomsg=msg )             b1((/3,2,4,1/))    !<- read "ghidefjklabc" (try array sectino with vector subscript)
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 1_4
      msg = ''  
   read (1, iostat=stat, iomsg=msg )             b2(1:2:1, 1:2:2)   !<- read "ABCDEF" (try array section with subscript triplet)
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 2_4
      msg = ''  
   read (1, iostat=stat, iomsg=msg )             b3(3)              !<- read "stu" (try array element)
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 3_4
      msg = ''  
   read (1, iostat=stat, iomsg=msg )             b4(1, 1:2)         !<- read "MNOSTU" (try array section with subscript triplet)
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 4_4
      msg = ''  
   
   ! check if the values are set correctly
   
   if ( ( b1(1)%c /= 'abc' ) .or. ( b1(2)%c /= 'def' ) .or. ( b1(3)%c /= 'ghi' ) .or. ( b1(4)%c /= 'jkl' ) )            error stop 5_4
   if ( ( b2(1,1)%c /= 'ABC' ) .or. ( b2(2,1)%c /= 'DEF' ) .or. ( b2(1,2)%c /= 'XXX' ) .or. ( b2(2,2)%c /= 'XXX' ) )    error stop 6_4
   if ( ( b3(1)%c /= 'xxx' ) .or. ( b3(2)%c /= 'xxx' ) .or. ( b3(3)%c /= 'stu' ) .or. ( b3(4)%c /= 'xxx' ) )            error stop 7_4
   if ( ( b4(1,1)%c /= 'MNO' ) .or. ( b4(2,1)%c /= 'XXX' ) .or. ( b4(1,2)%c /= 'STU' ) .or. ( b4(2,2)%c /= 'XXX' ) )    error stop 8_4
   
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