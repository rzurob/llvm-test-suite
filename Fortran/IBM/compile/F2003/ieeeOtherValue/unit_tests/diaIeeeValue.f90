!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : diaIeeeValue.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Zheming Gu
!*  DATE                       : July 4,2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Add IEEE_OTHER_VALUE
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 338353
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
!*  Diagnostic test for the feature 338353. Added IEEE_OTHER_VALUE
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main 
   USE,INTRINSIC :: IEEE_ARITHMETIC
   real x,y,m
   type(ieee_class_type), parameter :: k = ieee_other_value
   type(ieee_class_type), parameter :: p = ieee_negative_zero
   real :: z=IEEE_VALUE(1.0,k)
   print *, IEEE_VALUE(1.0,ieee_other_value)
   y=ieee_value(x,k)
   m=ieee_value(x,ieee_other_value)
   print *,ieee_value(x,ieee_negative_zero)
   m = ieee_value(x,p)
end
      
