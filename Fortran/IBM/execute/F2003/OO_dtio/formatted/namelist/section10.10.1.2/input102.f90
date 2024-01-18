!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: input102.f
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
!*                                       (no dtio procedure involved)
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
      integer(4)   :: i = -999
      real(4)      :: r = -9.9
      character(3) :: c = 'xyz'
   end type
end module

program input102
   use m

   integer :: stat
   character(150) :: msg = ''
   procedure(logical) :: precision_r4
   type(base)               :: b1
   type(base), pointer      :: b2
   type(base), allocatable  :: b3

   namelist /n1/ b1, b2
   namelist /n1/ b3, b1  !<- namelist contains 4 items

   allocate(b2,b3)

   open (1, file='input102.1', form='formatted', access='sequential', blank='zero' )

   read (1, n1, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 1_4
   if ( ( b1%i /= 12345 ) .or. ( .not. precision_r4(b1%r,8.23456)) .or. ( b1%c /= 'ibm' ) ) error stop 2_4
   if ( ( b2%i /= 5     ) .or. ( .not. precision_r4(b2%r,-0.0023)) .or. ( b2%c /= 'I M' ) ) error stop 3_4
   if ( ( b3%i /= -7825 ) .or. ( .not. precision_r4(b3%r,0.2006 )) .or. ( b3%c /= 'xyz' ) ) error stop 4_4  !<- no change in b3%c

end program
