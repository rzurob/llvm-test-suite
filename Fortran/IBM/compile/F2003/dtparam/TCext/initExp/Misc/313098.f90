! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/initExp/Misc/313098.f
! opt variations: -qnol

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

  type :: dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)   :: j
  end type

  type(dt(20,4)) :: t=dt(20,4)(j=-1,)

  END


