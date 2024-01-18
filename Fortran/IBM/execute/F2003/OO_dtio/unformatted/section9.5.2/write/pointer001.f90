!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: pointer001.f
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
!*  DESCRIPTION                : Testing: Section 9.5.2:
!*                               if input is a pointer, data are transferred from file to
!*                               the associated target.  If an output is a pointer, data shall
!*                               transfer from target to file. (WRITE, scalar pointer)
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


program pointer001
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
   class(base), allocatable, target :: b3, b5
   type(base),  allocatable, target :: b4, b6
   integer :: stat
   character(200) :: msg
   character(4) :: c1, c3
   character(8) :: c2, c4

   ! allocation of variables
   allocate ( b3, source = base('ibm') )
   allocate ( b4, source = base('ftn') )
   allocate ( b5, source = base('IBM') )
   allocate ( b6, source = base('FTN') )

   b1 => b3         !<- b1 should be pointing to 'ibm'
   b2 => b4         !<- b2 should be pointing to 'ftn'

   open (unit = 1, file ='pointer001.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             b1         !<- shall write the content of b3('ibmZ') to file
   write (1, iostat=stat, iomsg=msg )             b2, b1     !<- shall write the content of b4 and b3 ('ftnZibmZ') to file

   b1 => b5
   b2 => b6

   write (1, iostat=stat, iomsg=msg )             b1         !<- shall write the content of b5('IBMZ') to file
   write (1, iostat=stat, iomsg=msg )             b2, b1     !<- shall write the content of b6 and b5 ('FTNZIBMZ') to file

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1         !<- shall read 'ibm' from file
   read (1, iostat=stat, iomsg=msg )              c2         !<- shall read 'ftnibm' from file
   read (1, iostat=stat, iomsg=msg )              c3         !<- shall read 'IBM' from file
   read (1, iostat=stat, iomsg=msg )              c4         !<- shall read 'FTNIBM' from file

   ! check if the values are set correctly

   if ( c1 /= 'ibmZ' )        error stop 1_4
   if ( c2 /= 'ftnZibmZ' )     error stop 2_4
   if ( c3 /= 'IBMZ' )        error stop 3_4
   if ( c4 /= 'FTNZIBMZ' )     error stop 4_4

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
