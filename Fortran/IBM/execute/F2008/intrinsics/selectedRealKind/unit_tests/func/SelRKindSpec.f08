!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : November 05, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic function
!*                               selected_real_kind(p,r,radix)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 376076
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Test specification expressions for selected_real_kind intrinsic
!*  specified by the argument p,r,radix
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program SelRKindReturnVal

call selected_real_kind_spec(selected_real_kind(2,2,2))
call selected_real_kind_spec(selected_real_kind(11,294,2))
call selected_real_kind_spec(selected_real_kind(16,290,2))
call selected_real_kind_spec(selected_real_kind(12,9,-2))

call selected_real_kind_spec(selected_real_kind(33,290,2))
call selected_real_kind_spec(selected_real_kind(15,330,2))
call selected_real_kind_spec(selected_real_kind(33,310,2))
call selected_real_kind_spec(selected_real_kind(17,300,2))
call selected_real_kind_spec(selected_real_kind(7,30,3))

call selected_real_kind_spec(selected_real_kind(0,0,2))
call selected_real_kind_spec(selected_real_kind(0,37,2))
call selected_real_kind_spec(selected_real_kind(0,291,2))
call selected_real_kind_spec(selected_real_kind(0,307,2))
call selected_real_kind_spec(selected_real_kind(0,308,2))

call selected_real_kind_spec(selected_real_kind(6,0,2))
call selected_real_kind_spec(selected_real_kind(6,37,2))
call selected_real_kind_spec(selected_real_kind(6,291,2))
call selected_real_kind_spec(selected_real_kind(6,307,2))
call selected_real_kind_spec(selected_real_kind(6,308,2))

call selected_real_kind_spec(selected_real_kind(15,0,2))
call selected_real_kind_spec(selected_real_kind(15,37,2))
call selected_real_kind_spec(selected_real_kind(15,291,2))
call selected_real_kind_spec(selected_real_kind(15,307,2))
call selected_real_kind_spec(selected_real_kind(15,308,2))

call selected_real_kind_spec(selected_real_kind(31,0,2))
call selected_real_kind_spec(selected_real_kind(31,37,2))
call selected_real_kind_spec(selected_real_kind(31,291,2))
call selected_real_kind_spec(selected_real_kind(31,307,2))
call selected_real_kind_spec(selected_real_kind(31,308,2))

call selected_real_kind_spec(selected_real_kind(32,0,2))
call selected_real_kind_spec(selected_real_kind(32,37,2))
call selected_real_kind_spec(selected_real_kind(32,291,2))
call selected_real_kind_spec(selected_real_kind(32,307,2))
call selected_real_kind_spec(selected_real_kind(32,308,2))
call selected_real_kind_spec(selected_real_kind(32,308,0))

end

subroutine selected_real_kind_spec(a)
  integer, intent(in) :: a

  print *, a
end