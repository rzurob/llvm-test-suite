!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 26,2007
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289080
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Diag test IEEE Enhancements
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program diaIeeeEnhanc
        use,intrinsic :: ieee_arithmetic
        integer :: x
        logical :: yn,i3=ieee_support_underflow_control(x)
        integer i1,i2
        yn= .true.
        print *,'yn=',yn,'i1=',i1,'i2=',i2,'i3=',i3
        yn = ieee_support_underflow_control(y)
        if(yn .eqv. .true.) then
           call ieee_get_underflow_mode(i2)
           call ieee_set_underflow_mode(i1)
        endif
        print *,'yn=',yn ,'i1=',i1,'i2=',i2,'i3=',i3

end

