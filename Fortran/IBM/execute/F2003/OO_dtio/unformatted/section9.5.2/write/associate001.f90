!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate001.f
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
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to write associate name
!*                               Sequential Access
!*
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


program associate001
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
   class(base), pointer :: b1, b2
   class(base), allocatable :: b3
   type(base),  allocatable :: b4
   integer :: stat
   character(200) :: msg
   character(4) :: c1, c2, c3, c4

   ! allocation of variables


   allocate ( b1, source = base('abc') )
   allocate ( b2, source = base('def') )
   allocate ( b3, source = base('ghi') )
   allocate ( b4, source = base('jkl') )

   open (unit = 1, file ='associate001.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   associate ( b11 => b1, b12 => b2, b13 => b3, b14 => b4 )
      write (1, iostat=stat, iomsg=msg )   b11         !<- shall write 'abcZ' to file
      write (1, iostat=stat, iomsg=msg )   b12         !<- shall write 'defZ' to file
      write (1, iostat=stat, iomsg=msg )   b13         !<- shall write 'ghiZ' to file
      write (1, iostat=stat, iomsg=msg )   b14         !<- shall write 'jklZ' to file
   end associate

   rewind 1

   read (1, iostat=stat, iomsg=msg )       c1          !<- shall read 'abcZ' from file
   read (1, iostat=stat, iomsg=msg )       c2          !<- shall read 'defZ' from file
   read (1, iostat=stat, iomsg=msg )       c3          !<- shall read 'ghiZ' from file
   read (1, iostat=stat, iomsg=msg )       c4          !<- shall read 'jklZ' from file

   ! check if the values are set correctly

   if ( c1 /= 'abcZ' )        error stop 1_4
   if ( c2 /= 'defZ' )        error stop 2_4
   if ( c3 /= 'ghiZ' )        error stop 3_4
   if ( c4 /= 'jklZ' )        error stop 4_4

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
