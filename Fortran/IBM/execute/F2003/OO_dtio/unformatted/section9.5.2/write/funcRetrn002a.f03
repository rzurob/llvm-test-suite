! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item function return of some transformational intrinsic functions
!*                                 including reshape, spread, and transpose
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

program funcRetrn002a
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
   character(16)   :: c1, c2
   logical :: mergemask(2,2) = reshape( source = (/ .true. , .false. , .true. , .false. /), shape = (/2,2/) )

   open (unit = 1, file ='funcRetrn002a.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             transpose(reshape( source = (/ base('ABC'), &
      base('DEF'), base('GHI'), base('JKL') /), shape=(/2,2/) ))                               !<- write 'ABCGHIDEFJKL' to file (try intrinsic transpose)
   write (1, iostat=stat, iomsg=msg )             spread ( (/ base('123') , base('456') /), dim=1, ncopies=2 ) !<- writes '123123456456' to file

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2


   ! check if the values are set correctly

   if ( c1 /= 'ABCZGHIZDEFZJKLZ' )                  error stop 4_4
   if ( c2 /= '123Z123Z456Z456Z' )                  error stop 5_4

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%c
    write (unit, iostat=iostat, iomsg=iomsg ) 'Z'   !<- write 'Z' at the end of record so we know DTIO is called

end subroutine