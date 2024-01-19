! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/valueAttrwAllocCompnt/unit_tests/func/valuefordtalloc005.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=self

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan, 20, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : value attribute for derived types with allocatable components
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of value attribute
!*                               for derived types with allocatable
!*                               components (deep copy of derived type)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  module m
    type A(n1,k1,k2)    ! (20,4,4)
      integer, kind :: k1,k2
      integer, len  :: n1
      real(k1)      :: x
      integer(k2)   :: y
    end type

    type B(n2,k3)    ! (20,4)
      integer, kind                 :: k3
      integer, len                  :: n2
      type(A(:,k3,k3)), allocatable :: A1
      integer(k3)                   :: z
    end type

    contains

      function func(B1)
        type(B(20,4)), value :: B1
        type(B(20,4)) :: func

        if(B1%A1%x/=1.0 .or. B1%A1%y/=1 &
       &   .or. B1%z/=2.0) error stop 1

       func=B1

       B1%z=0.0
       B1%A1%x=0.0
       B1%A1%y=0

      end function

  end module

  use m

  type(B(20,4)) :: B2, B3

  B2%z=2.0
  allocate(A(20,4,4) :: B2%A1)
  B2%A1%x=1.0
  B2%A1%y=1

  B3=func(B2)

  if((B2%z .ne. B3%z) .or. (B2%A1%x .ne. B3%A1%x) &
  &   .or. (B2%A1%y .ne. B3%A1%y)) error stop 2

end program
