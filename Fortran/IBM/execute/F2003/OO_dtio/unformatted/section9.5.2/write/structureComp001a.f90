!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComp001a.f
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
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be an structure component (without container interface)
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

   type container
      type(base) :: b1
      type(base) :: b2
   end type

contains
   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c
   end function
end module

program structureComp001a
   use m1

   interface write(unformatted)
      subroutine writeUnformattedBase (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   type(container), allocatable  :: b11
   type(container), pointer      :: b12
   type(container)               :: b13
   integer :: stat
   character(200) :: msg
   character(8)  :: c1, c3, c4
   character(4)  :: c2

   ! allocation of variables
   allocate ( b11, source = container( b2=base('def'), b1=base('abc') ) )
   allocate ( b12, source = container( b2=base('DEF'), b1=base('ABC') ) )
   b13 = container( b2=base('JKL'), b1=base('GHI') )

   open (unit = 1, file ='structureComp001a.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             b11        !<- write 'abcZdefZ' to file
   write (1, iostat=stat, iomsg=msg )             b12%b1     !<- write 'ABCZ' to file
   write (1, iostat=stat, iomsg=msg )             b13        !<- write 'GHIZJKLZ' to file
   write (1, iostat=stat, iomsg=msg )             container( b2=base('jkl'), b1=base('ghi') )  !<- write 'ghiZjklZ' to file

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4

   ! check if the values are set correctly

   if ( c1 /= 'abcZdefZ' )           error stop 1_4
   if ( c2 /= 'ABCZ' )               error stop 2_4
   if ( c3 /= 'GHIZJKLZ' )           error stop 3_4
   if ( c4 /= 'ghiZjklZ' )           error stop 4_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformattedBase (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"
end subroutine
