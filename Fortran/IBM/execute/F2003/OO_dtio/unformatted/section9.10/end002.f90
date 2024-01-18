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
! %GROUP: end002.f
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
!*  DESCRIPTION                : Testing: Section 9.10: Error, end-of-record, and end-of-file conditions
!*                               - use both end and err specifiers with end of file conditions in a I/O operations,
!*                                 and see if the end branch will be taken correctly and try with io-implied-do
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
      character :: c = ''
   end type  
end module


program end002
   use m1   
   use ISO_FORTRAN_ENV
   
   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface

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
   class(base), allocatable :: b1(:)
   class(base), pointer     :: b2(:)
   type(base),  allocatable :: b3(:)
   integer :: stat
   character(200) :: msg
   
   ! allocation of variables
   
   allocate (b1(2), source = (/ base('abc'), base('def') /) )
   allocate (b2(3), source = (/ base('ABC'), base('DEF'), base('GHI') /) )
   allocate (b3(4), source = (/ base('jkl'), base('mno'), base('pqr'), base('stu') /))

   
   open (unit = 1, file ='end002.1', form='unformatted', access='sequential')
   open (unit = 3, file ='end002.3', form='unformatted', access='stream')   
      
   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg)     ( b1(k), b2(k), b3(k), k = 1,2) 
   write (3, iostat=stat, iomsg=msg)     ( b3(j), b2(j), j = 3,2,-1)   
      
   ! intentionally not rewinding sequential file, so end of file can be reached
    
    rewind 1
    
    read  (1, iostat = stat, iomsg = msg)                           ( b1(k), b2(k), b3(k), k = 1,2) 
    read  (1, iostat = stat, iomsg = msg, err=100, end=200)           b1(1)

100 error stop 1_4

200 if ( stat /= IOSTAT_END )  error stop 2_4
    read  (3,iostat = stat, iomsg = msg, err=700, end=800, pos=1)    ( b3(j), b2(j), j = 3,1,-1)
    error stop 3_4

700 error stop 4_4 
800 if ( stat /= IOSTAT_END ) error stop 5_4
   
   ! close the file appropriately
   
   close ( 1, status ='delete' )
   close ( 3, status ='delete' )   
   
end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
    
   read (unit, iomsg=iomsg, iostat=iostat ) dtv%c
   
end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iomsg=iomsg, iostat=iostat ) dtv%c

end subroutine

