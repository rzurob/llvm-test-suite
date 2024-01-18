!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: input102a.f
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
!*                                       (no dtio procedure involved, with class hierarchy)
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
   end type

   type, extends(base) :: child
      real(4)      :: r = -9.9
   end type

   type, extends(child) :: gen3
      character(3) :: c = 'xyz'
   end type
end module

program input102a
   use m

   integer :: stat
   character(150) :: msg
   procedure(logical) :: precision_r4
   type(child)               :: b1
   type(child), pointer      :: b2
   type(gen3), allocatable   :: b3
   type(gen3)                :: b4

   namelist /n1/ b1, b2
   namelist /n1/ b3, b4  !<- namelist contains 4 items

   allocate(b2,b3)

   open (1, file='input102a.1', form='formatted', access='sequential', blank='zero' )

   read (1, n1, iostat = stat, iomsg = msg)

   if ( ( b1%i /= 12345 ) .or. ( .not. precision_r4(b1%r,-9.9)) )    error stop 1_4
   if ( ( b2%i /= 5     ) .or. ( .not. precision_r4(b2%r,-0.1234)) ) error stop 2_4
   if ( ( b3%i /= -9876 ) .or. ( .not. precision_r4(b3%r,-9.9 )) .or. ( b3%c /= 'ibm' ) ) error stop 3_4  !<- no change in b3%c
   if ( ( b4%i /= -999 ) .or. ( .not. precision_r4(b4%r,-9.9 )) .or. ( b4%c /= 'IBM' ) ) error stop 4_4  !<- no change in b4%i, b4%r

end program
