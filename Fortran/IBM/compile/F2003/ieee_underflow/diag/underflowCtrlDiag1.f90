! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 15 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_support_underflow_control()
!*                             : ieee_support_underflow_control(X)
!*  SECONDARY FUNCTIONS TESTED :
!*  REFERENCE                  : Feature Number 289080
!*
!*  DESCRIPTION                :
!*  test if compiler issues a suitable error memssage when passing an invalid argument to ieee_support_underflow_control(X)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012
      module m
        type Info
            character(20) :: Name
            character(40) :: Address
            character(20) :: PhoneNum
            character(40) :: Email
        end type Info
      end module
      program underCtrlDiag1
        use,intrinsic :: ieee_arithmetic
        use m
        implicit none

        integer   :: i
        integer(1) :: i_1
        integer(2) :: i_2
        integer(4) :: i_4
        integer(8) :: i_8
        integer,parameter :: i_const = 2
        real(4)    :: r_4
        real(8)    :: r_8
        real(16)   :: r_16
        real,parameter    :: r_const = 2.
        character(len=10) :: c = "HELLO WORLD!"
        logical   :: support
        logical   :: l
        logical(1) :: l_1
        logical(2) :: l_2
        logical(4) :: l_4
        logical(8) :: l_8
        logical,parameter :: l_const = .true.
        complex     :: x
        double complex :: dx
        complex(4)  :: x_4
        complex(8)  :: x_8
        complex(16) :: x_16
        byte :: b = "A"
        type(Info)        :: myInfo

!       pass 2 more arguemnts
        support=ieee_support_underflow_control(r_4,r_8)
        support=ieee_support_underflow_control(i_4,r_8,r_16)
        support=ieee_support_underflow_control(i_4,i_2,l_8)

!       pass an integer type argument
        support=ieee_support_underflow_control(i)
        support=ieee_support_underflow_control(i_1)
        support=ieee_support_underflow_control(i_2)
        support=ieee_support_underflow_control(i_4)
        support=ieee_support_underflow_control(i_8)
        support=ieee_support_underflow_control(i_const)
        support=ieee_support_underflow_control(2)
        support=ieee_support_underflow_control(2_8)

!       pass a logical type argument
        support=ieee_support_underflow_control(l)
        support=ieee_support_underflow_control(l_1)
        support=ieee_support_underflow_control(l_2)
        support=ieee_support_underflow_control(l_4)
        support=ieee_support_underflow_control(l_8)
        support=ieee_support_underflow_control(l_const)
        support=ieee_support_underflow_control(.false.)
        support=ieee_support_underflow_control(.false._8)

!       pass a character type argument
        support=ieee_support_underflow_control(c)
        support=ieee_support_underflow_control(c(3:6))
        support=ieee_support_underflow_control('\"XL FORTRAN\"')
        support=ieee_support_underflow_control(1_"ABC\t")

!       pass a complex type argument
        support=ieee_support_underflow_control(x)
        support=ieee_support_underflow_control(dx)
        support=ieee_support_underflow_control(x_4)
        support=ieee_support_underflow_control(x_8)
        support=ieee_support_underflow_control(x_16)
        support=ieee_support_underflow_control((4_2,-3D45))
        support=ieee_support_underflow_control((2+1,1*3_8))

!       pass a byte type argument
        support=ieee_support_underflow_control(b)

!       pass a derived type argument
        support=ieee_support_underflow_control(myInfo)
        support=ieee_support_underflow_control( &
          Info("NAME","ADDRESS","1234567","NAME@CA.IBM.COM"))

       end program
