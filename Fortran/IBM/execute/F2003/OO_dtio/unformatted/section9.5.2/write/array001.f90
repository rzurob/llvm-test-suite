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
!*                               - Try output item to be an allocatable array with array section
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
   end type
   
contains
   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c      
   end function   
   
end module


program array001
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
   class(base), allocatable     :: b1(:)
   class(base), allocatable     :: b2(:,:)
   type(base) :: b3(4)
   type(base),  pointer :: b4(:,:)
   integer :: stat
   character(200) :: msg
   character(16) :: c1
   character(8)  :: c2
   character(4)  :: c3
   character(8)  :: c4
   
   ! allocation of variables
   allocate ( b1(4), source = (/ base('abc'), base('def'), base('ghi'), base('jkl') /) )
   allocate ( b2(2,2), source = reshape (source = (/ base('ABC'), base('DEF'), base('GHI'), base('JKL')  /), shape=(/2,2/) ) )
   b3 = (/ base('mno'), base('pqr'), base('stu'), base('vwx') /)
   allocate ( b4(2,2), source = reshape (source = (/ base('MNO'), base('PQR'), base('STU'), base('VWX')  /), shape=(/2,2/) ) )
   
   open (unit = 1, file ='array001.data', form='unformatted', access='sequential')
   
   ! unformatted I/O operations
   
   write (1, iostat=stat, iomsg=msg )             b1((/3,2,4,1/))    !<- writes "ghiZdefZjklZabcZ" (try array sectino with vector subscript)
   write (1, iostat=stat, iomsg=msg )             b2(1:2:1, 1:2:2)   !<- writes "ABCZDEFZ" (try array section with subscript triplet)
   write (1, iostat=stat, iomsg=msg )             b3(3)              !<- writes "stuZ" (try array element)
   write (1, iostat=stat, iomsg=msg )             b4(1, 1:2)         !<- writes "MNOZSTUZ" (try array section withe subscript triplet)
   
   rewind 1
   
   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4
   
   ! check if the values are set correctly
   
   if ( c1 /= 'ghiZdefZjklZabcZ' )        error stop 1_4
   if ( c2 /= 'ABCZDEFZ' )                error stop 2_4
   if ( c3 /= 'stuZ' )                    error stop 3_4
   if ( c4 /= 'MNOZSTUZ' )                error stop 4_4
   
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
