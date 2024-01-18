!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxIeeeEnhanc.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Zheming Gu
!*  DATE                       : July 26,2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289080
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Test IEEE Enhancements
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxIeeeEnhanc 
        use,intrinsic :: ieee_arithmetic
        logical :: yn,i1,i2,i3=ieee_support_underflow_control()
        yn= .true.; i1=.true. ; i2=.true.
        print *,'yn=',yn,'i1=',i1,'i2=',i2,'i3=',i3
        yn = ieee_support_underflow_control()
        if(yn .eqv. .true.) then
           call ieee_get_underflow_mode(i2)
           call ieee_set_underflow_mode(i1)
        endif
        print *,'yn=',yn ,'i1=',i1,'i2=',i2,'i3=',i3
end

