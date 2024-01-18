!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComp003.f
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
!*                               - Try output item to be structure component, try parent component
!*                               Direct Access
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

   type, extends(base) :: child
      character(3) :: cc = ''
   end type

   type container
      type(child) :: b1
      type(child) :: b2
   end type

contains
   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c
   end function
end module


program structureComp003
   use m1

   interface write(unformatted)

      subroutine writeUnformattedContainer (dtv, unit, iostat, iomsg)
         import container
         class(container), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

      subroutine writeUnformattedBase (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

   end interface

   ! declaration of variables
   class(container), allocatable  :: b11
   class(container), pointer      :: b12
   type (container)               :: b13
   integer :: stat
   character(200) :: msg
   character(15)  :: c1, c4
   character(7)   :: c2
   character(4)   :: c3

   ! allocation of variables
   allocate ( b11, source = container( b2=child('def','ghi'), b1=child('abc','def') ) )
   allocate ( b12, source = container( b2=child('DEF','EFG'), b1=child('ABC','DEF') ) )
   b13 = container( b2=child('JKL','MNO'), b1=child('GHI','JKL') )

   open (unit = 1, file ='structureComp003.data', form='unformatted', access='direct', recl=30)

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg, rec=1 )             b11                !<- write 'abcdefZdefghiZY' to file
   write (1, iostat=stat, iomsg=msg, rec=4 )             b12%b1             !<- write 'ABCDEFZ' to file
   write (1, iostat=stat, iomsg=msg, rec=2 )             b13%b2%base        !<- write 'JKLZ'    to file
   write (1, iostat=stat, iomsg=msg, rec=3 )             container( b2=child('jkl','mno'), b1=child('ghi','jkl') )  !<- write 'ghijklZjklmnoZY' to file

   read (1, iostat=stat, iomsg=msg, rec=1 )              c1
   read (1, iostat=stat, iomsg=msg, rec=4 )              c2
   read (1, iostat=stat, iomsg=msg, rec=2 )              c3
   read (1, iostat=stat, iomsg=msg, rec=3 )              c4

   ! check if the values are set correctly

   if ( c1 /= 'abcdefZdefghiZY' )     error stop 1_4
   if ( c2 /= 'ABCDEFZ' )             error stop 2_4
   if ( c3 /= 'JKLZ' )                error stop 3_4
   if ( c4 /= 'ghijklZjklmnoZY' )     error stop 4_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformattedContainer (dtv, unit, iostat, iomsg)
use m1
    class(container), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character, intent(inout) :: iomsg

    interface write(unformatted)
        subroutine writeUnformattedBase (dtv, unit, iostat, iomsg)
            import base
            class(base), intent(in) :: dtv
            integer,  intent(in) :: unit
            integer,  intent(out) :: iostat
            character(*),  intent(inout) :: iomsg
        end subroutine
    end interface

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%b1, dtv%b2
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Y"

end subroutine

subroutine writeUnformattedBase (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()

    select type (dtv)
       type is (child)
          write (unit, iostat=iostat, iomsg=iomsg ) dtv%cc
    end select

    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine
