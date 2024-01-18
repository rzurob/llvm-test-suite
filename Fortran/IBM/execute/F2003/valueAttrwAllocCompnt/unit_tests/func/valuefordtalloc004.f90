!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: valuefordtalloc004.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan, 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : value attribute for derived types with allocatable components
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of value attribute
!*                               for derived types with allocatable
!*                               components
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  module m
    type A
      character(20) :: w
      complex, allocatable :: x
      integer :: y
      real :: z
    end type
  end module

  use m

  type(A) :: A1,A2,A3
  integer :: i

  interface
    subroutine sub(A4,A5,A6)
      use m
      type(A), value :: A4
      type(A) :: A5,A6
      value :: A5
    end subroutine
  end interface

  allocate(A1%x)
  allocate(A2%x)
  allocate(A3%x)

  A1%w='passbyvalue'
  A1%x=(1.0,1.0)
  A1%y=5
  A1%z=1.0

  A2%w='passbyvalue'
  A2%x=(2.0,2.0)
  A2%y=5
  A2%z=1.0

  A3%w='passbyref'
  A3%x=(3.0,3.0)
  A3%y=5
  A3%z=1.0

  call sub(A1,A2,A3)
  if(A1%w /='passbyvalue' .and.  &
  &  A1%x/=(1.0,1.0) .and. A1%y/=5  &
  &  .and. A1%z/=1.0) error stop 1

  if(A2%w /='passbyvalue' .and.  &
  &  A2%x/=(1.0,1.0) .and. A2%y/=5  &
  &  .and. A2%z/=1.0) error stop 2

  if(A3%w /='ref' .and.  &
  &  A3%x/=(0.0,0.0) .and. A3%y/=0  &
  &  .and. A3%z/=0.0) error stop 3



end program

     subroutine sub(A4, A5, A6)
       use m
       type(A), value :: A4
       type(A) :: A5,A6
       value :: A5

       if(A4%w /='passbyvalue' .and.  &
       &  A4%x/=(1.0,1.0) .and. A4%y/=5  &
       &  .and. A4%z/=1.0) error stop 4

       if(A5%w /='passbyvalue' .and.  &
       &  A5%x/=(1.0,1.0) .and. A5%y/=5  &
       &  .and. A5%z/=1.0) error stop 5


       A4%w='value'
       A4%x=(0.0,0.0)
       A4%y=0
       A4%z=0.0

       A5%w='value'
       A5%x=(0.0,0.0)
       A5%y=0
       A5%z=0.0

       A6%w='ref'
       A6%x=(0.0,0.0)
       A6%y=0
       A6%z=0.0
     end subroutine


