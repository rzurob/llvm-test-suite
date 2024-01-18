!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_misc_5.f
!*
!*  DATE                       : May 02, 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Internal procedure as actual argument or procedure target
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : CMVC Feature number 303977
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Test procedure argument asociation/pointer assignment --
!*     C730 (R740) The proc-target shall not be a
!*     nonintrinsic elemental procedure.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM intproc_misc_5
  PROCEDURE(), POINTER :: proc

  proc => sub
  CALL sub1(sub) !<- no restriction, now ok
  CALL sub2(sub)

  CONTAINS

  ELEMENTAL SUBROUTINE sub(arg)
  INTEGER, INTENT(IN) :: Arg
  END SUBROUTINE

  SUBROUTINE sub1(proc)
  PROCEDURE() :: proc
  END SUBROUTINE

  SUBROUTINE sub2(proc)
  PROCEDURE(), POINTER :: proc
  END SUBROUTINE

  END

