!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : 313098.f
!*
!*  DATE                       : Aug 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Misc
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  313098 -  ICE by syntax error
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM IceByErr_313098

  type :: dt
    integer :: j
  end type

  type(dt) :: t=dt(j=-1,)

  END


