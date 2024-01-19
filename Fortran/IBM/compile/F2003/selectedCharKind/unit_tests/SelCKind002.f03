!*********************************************************************
!*  ===================================================================
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
!*  Test intrinsic function selected_char_kind(name) languague level
!*  warning message
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program SelCKind002

        integer :: i = selected_char_kind("ascii")
        print *,selected_char_kind("DefAULt")

end


