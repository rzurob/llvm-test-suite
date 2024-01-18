!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : 313098.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Aug 28, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Misc 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  313098 -  ICE by syntax error
!* 
!*  
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM IceByErr_313098 

  type :: dt
    integer :: j
  end type

  type(dt) :: t=dt(j=-1,)

  END

 
