! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : underflowModeDiag1.f
!*
!*  PROGRAMMER                 : Nancy Wang
!*  DATE                       : Nov. 15 2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_set_underfolow_mode(gradual) 
!*                             : ieee_get_underflow_mode(gradual)
!*  SECONDARY FUNCTIONS TESTED :
!*  REFERENCE                  : Feature Number 289080
!*
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                :    
!*  test if compiler issues a suitable error message when passing an invalid argument to ieee_set_underfolow_mode(gradual) and ieee_get_underflow_mode(gradual) intrinsic subroutines 
!* 
!23456789012345678901234567890123456789012345678901234567890123456789012   
      module m
        type Info
            character(20) :: Name
            character(40) :: Address
            character(20) :: PhoneNum
            character(40) :: Email
        end type Info         
      end module m
      program underflowModeDiag1
        use,intrinsic :: ieee_arithmetic
        use m 
        implicit none
        
        logical   :: l
        logical(1) :: l_1
        logical(2) :: l_2
        logical(4) :: l_4
        logical(8) :: l_8
        logical,parameter :: l_const = .true.
        integer   :: i
        real      :: r
        double precision  :: d
        character(len=10) :: c ="HELLO WORLD!"
        complex   :: x
        double complex :: dx
        byte :: b = 'A'
        type(Info)        :: myInfo 

!       pass 2 more arguements to SET and GET subroutines 
        if(ieee_support_underflow_control(r)) then
           call ieee_set_underflow_mode(.true.,.false.)
           call ieee_set_underflow_mode(l,l_const,l_4,.true.)
           call ieee_get_underflow_mode(.true.,.false.)
           call ieee_get_underflow_mode(l,l_const,l_4,.true.)
        endif

!       pass no arguement to SET and GET subroutines
        if(ieee_support_underflow_control()) then
           call ieee_set_underflow_mode()
           call ieee_get_underflow_mode()
        endif

!       pass a real type arguement to SET and GET subroutines
        if(ieee_support_underflow_control(r)) then
           call ieee_set_underflow_mode(r)
           call ieee_set_underflow_mode(d)
           call ieee_set_underflow_mode(2.5_8)
           call ieee_get_underflow_mode(r)
           call ieee_get_underflow_mode(d)
           call ieee_get_underflow_mode(2.5_8)
        endif

!       pass a non-default logical arguement to SET and GET subroutines
        if(ieee_support_underflow_control()) then
           call ieee_set_underflow_mode(l_1)
           call ieee_set_underflow_mode(l_2)
           call ieee_set_underflow_mode(l_8)
           call ieee_get_underflow_mode(l_1)
           call ieee_get_underflow_mode(l_2)
           call ieee_get_underflow_mode(l_8)
        endif

!       pass a default constant logical arguement to GET subroutine
        if(ieee_support_underflow_control()) then
           call ieee_get_underflow_mode(l_const)
           call ieee_get_underflow_mode(.true.)
           call ieee_get_underflow_mode(.false._4)
        endif

!       pass an integer type arguement to SET and GET subroutines
        if(ieee_support_underflow_control(r)) then
           call ieee_set_underflow_mode(i)
           call ieee_set_underflow_mode(2)
           call ieee_set_underflow_mode(2_8)
           call ieee_get_underflow_mode(i)
           call ieee_get_underflow_mode(2)
           call ieee_get_underflow_mode(2_8)
        endif

!       pass a character type arguement to SET and GET subroutines
        if(ieee_support_underflow_control(r)) then
           call ieee_set_underflow_mode(c)
           call ieee_set_underflow_mode(c(3:6))
           call ieee_set_underflow_mode('\"XL FORTRAN\"')
           call ieee_set_underflow_mode(1_"ABC\t")
           call ieee_get_underflow_mode(c)
           call ieee_get_underflow_mode(c(3:6))
           call ieee_get_underflow_mode('\"XL FORTRAN\"')
           call ieee_get_underflow_mode(1_"ABC\t")
        endif

!       pass a complex type arguement to SET and GET subroutines
        if(ieee_support_underflow_control(r)) then
           call ieee_set_underflow_mode(x)
           call ieee_set_underflow_mode(dx)
           call ieee_set_underflow_mode((4_2,-3D45))
           call ieee_set_underflow_mode((2+1,1*3_8))
           call ieee_get_underflow_mode(x)
           call ieee_get_underflow_mode(dx)
           call ieee_get_underflow_mode((4_2,-3D45))
           call ieee_get_underflow_mode((2+1,1*3_8))
        endif

!       pass a byte type arguement to SET and GET subroutines
        if(ieee_support_underflow_control(r)) then
           call ieee_set_underflow_mode(b)
           call ieee_get_underflow_mode(b)
        endif

!       pass a derived type arguement to SET and GET subroutines
        if(ieee_support_underflow_control(r)) then
           call ieee_set_underflow_mode(myInfo)
           call ieee_set_underflow_mode(   &
              Info("NAME","ADDRESS","1234567","NAME@CA.IBM.COM"))
           call ieee_get_underflow_mode(myInfo)
           call ieee_get_underflow_mode(   &
              Info("NAME","ADDRESS","1234567","NAME@CA.IBM.COM"))
        endif
       end program
