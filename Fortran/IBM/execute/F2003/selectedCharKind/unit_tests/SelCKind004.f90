!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SelCKind004.f
!*
!*  DATE                       : August 02, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic function selected_char_kind(name)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 317648
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Test subroutine calling the intrinsic function
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program SelCKind004

       character(5) :: c = "ascii"
       call sub(c)
end

subroutine sub(c)
       character*(*) c
       integer :: h
       h = selected_char_kind(c)
       print *,h
end subroutine
