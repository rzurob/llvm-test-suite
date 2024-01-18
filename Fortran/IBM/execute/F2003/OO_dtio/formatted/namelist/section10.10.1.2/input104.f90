!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: input104.f
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
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist Input Values
!*                                        Derived type variable shall be expanded into intrinsic types
!*                                       (no dtio procedure involved, with array components)
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
   type base
      integer(4)   :: i(3) = (/ 9, 9, 9 /)
      real(4)      :: r(3) = (/ 9.0, 9.0, 9.0 /)
      character(3) :: c(3) = (/ 'xxx', 'xxx', 'xxx' /)
   end type
end module

program input104
   use m

   integer :: stat
   character(150) :: msg = ''
   procedure(logical) :: precision_r4
   type(base)               :: b1
   type(base), pointer      :: b2
   type(base), allocatable  :: b3

   namelist /n1/ b1, b2
   namelist /n1/ b3

   allocate(b2,b3)

   open (1, file='input104.1', form='formatted', access='sequential', blank='zero' )

   read (1, n1, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 1_4

   print *, b1
   print *, b2
   print *, b3

end program
