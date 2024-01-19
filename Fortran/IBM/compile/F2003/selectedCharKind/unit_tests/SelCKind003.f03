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
!*  Test intrinsic function selected_char_kind(name) wrong argument
!*  error message
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program SelCKind003

        print *,selected_char_kind(1)
        print *,selected_char_kind("ascii","default")

end


