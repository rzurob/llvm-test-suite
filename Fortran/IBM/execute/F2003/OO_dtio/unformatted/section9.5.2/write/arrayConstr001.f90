!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: arrayConstr001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 (Data Transfer input/output list)
!*                               - output item is an array constructor
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
   type base1
      character(3) :: c = ''
   end type

   type base2
      integer(4) :: i
   end type

end module


program arrayConstr001
   use m1

   interface write(unformatted)
      subroutine writeUnformatted1 (dtv, unit, iostat, iomsg)
         import base1
         class(base1), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

      subroutine writeUnformatted2 (dtv, unit, iostat, iomsg)
         import base2
         class(base2), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables

   integer :: stat
   character(200) :: msg
   character(16) :: c1
   integer :: j1(4)

   ! allocation of variables

   open (unit = 1, file ='arrayConstr001.data', form='unformatted', access='sequential')

   ! I/O operations

   write (1, iostat=stat, iomsg=msg )      (/ base1('abc'), base1('def'), base1('ghi'), base1('jkl') /)
   write (1, iostat=stat, iomsg=msg )      (/ (base2(i),i = 4,7) /)

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              j1      !<- should read (5,6,7,8)

   ! check if the values are set correctly

   if ( c1 /= 'abcZdefZghiZjklZ' )                                                           error stop 1_4
   if ( ( j1(1) /= 5 ) .or. ( j1(2) /= 6 ) .or. ( j1(3) /= 7 ) .or. ( j1(4) /= 8 ) )     error stop 2_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted1 (dtv, unit, iostat, iomsg)
use m1
    class(base1), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%c
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine

subroutine writeUnformatted2 (dtv, unit, iostat, iomsg)
use m1
    class(base2), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    ! add 1 so that we know DTIO is used
    write (unit, iostat=iostat, iomsg=iomsg ) dtv%i + 1

end subroutine
