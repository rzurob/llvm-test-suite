!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : d321528
!*
!*  DATE                       : 2006-10-06
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Defect 321528: "F2003: ICE: Array constructor with boz literal"
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  : none
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Make sure the compiler can handle a typeless array constructor passed to a
!*  subroutine expecting an unlimited polymorphic argument.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program d321528

  call sub((/z'0'/))

contains

  subroutine sub(arg)
    class (*) :: arg(:)
  end subroutine sub

end program d321528
