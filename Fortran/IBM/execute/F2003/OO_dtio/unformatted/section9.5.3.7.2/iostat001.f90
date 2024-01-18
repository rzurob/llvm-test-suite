!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: iostat001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: For iostat equals to non-zero, at runtime, the i/o procedure should stop
!*                                        the execution of the program
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type :: base
       character(3) :: c
    end type
end module

program iostat001
use m

   interface write(unformatted)
      subroutine unformattedWrite (dtv, unit, iostat, iomsg)
      use m
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables

   integer :: stat
   character(30) :: msg

   class(base), allocatable :: b1

   ! allocation of variables

   allocate ( b1 )

   open(1, file='iostat001.data', access='sequential', form='unformatted')

    write (1, err = 100)                b1
    print *, 'error'
100 print *, 'great!'
    write (1, iostat = stat )           b1
    if ( stat /= 1 ) error stop 1_4

   ! close the file appropriately

   ! close ( 1, status ='delete' )

end program


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( unit, iostat= iostat, iomsg=iomsg )    dtv%c
   iostat = 1

end subroutine
