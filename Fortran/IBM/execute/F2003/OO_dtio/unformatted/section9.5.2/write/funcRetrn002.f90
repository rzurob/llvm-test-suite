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
! %GROUP: funcRetrn002.f
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
!*                               - Try output item function return of some transformational intrinsic functions
!*                                 including cshift, eoshift, merge
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
end module

program funcRetrn002
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
   integer :: stat
   character(200) :: msg
   character(16)   :: c1, c2, c3

   class(base), allocatable :: b1(:), b2(:,:)
   class(base), pointer :: b3(:), b4(:,:)   
   logical :: mergemask(2,2) = reshape( source = (/ .true. , .false. , .true. , .false. /), shape = (/2,2/) )
   
   ! allocation of variables
   
   allocate(b1(4), source = (/ base('abc'), base('def'), base('ghi'), base('jkl') /) )
   allocate(b2(2,2), source = reshape( source = (/ base('abc'), base('def'), base('ghi'), base('jkl') /), shape=(/2,2/) ))
   allocate(b3(4), source = (/ base('ABC'), base('DEF'), base('GHI'), base('JKL') /) )
   allocate(b4(2,2), source = reshape( source = (/ base('ABC'), base('DEF'), base('GHI'), base('JKL') /), shape=(/2,2/) ))
   
   open (unit = 1, file ='funcRetrn002.data', form='unformatted', access='sequential')
   
   ! unformatted I/O operations
   
   write (1, iostat=stat, iomsg=msg )             cshift (b1, shift=-1)                        !<- write 'jklabcdefghi' to file (try intrinsic cshift)
   write (1, iostat=stat, iomsg=msg )             eoshift(b3,shift=-1, boundary=base('xxx') )  !<- write 'xxxABCDEFGHI' to file (try intrinsic eoshift)
   write (1, iostat=stat, iomsg=msg )             merge (b2,b4,mergemask)                      !<- write 'abcDEFghiJKL' to file (try intrinsic merge)

   rewind 1
   
   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3

   
   ! check if the values are set correctly

   if ( c1 /= 'jklZabcZdefZghiZ' )                  error stop 1_4
   if ( c2 /= 'xxxZABCZDEFZGHIZ' )                  error stop 2_4
   if ( c3 /= 'abcZDEFZghiZJKLZ' )                  error stop 3_4
   
   ! close the file appropriately
   
   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%c
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"     
end subroutine
