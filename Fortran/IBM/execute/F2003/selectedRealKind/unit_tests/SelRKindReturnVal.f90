!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 05, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic function
!*                               selected_real_kind(p,r)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 324332
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Test return values that equals to the kind type parameter of
!*  the real type specified by the argument p,r
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program SelRKindReturnVal


print *,selected_real_kind(2,2)
print *,selected_real_kind(11,294)
print *,selected_real_kind(16,290)

print *,selected_real_kind(33,290)
print *,selected_real_kind(15,330)
print *,selected_real_kind(33,310)
print *,selected_real_kind(17,300)

print *,selected_real_kind(0,0)
print *,selected_real_kind(0,37)
print *,selected_real_kind(0,291)
print *,selected_real_kind(0,307)
print *,selected_real_kind(0,308)

print *,selected_real_kind(6,0)
print *,selected_real_kind(6,37)
print *,selected_real_kind(6,291)
print *,selected_real_kind(6,307)
print *,selected_real_kind(6,308)

print *,selected_real_kind(15,0)
print *,selected_real_kind(15,37)
print *,selected_real_kind(15,291)
print *,selected_real_kind(15,307)
print *,selected_real_kind(15,308)

print *,selected_real_kind(31,0)
print *,selected_real_kind(31,37)
print *,selected_real_kind(31,291)
print *,selected_real_kind(31,307)
print *,selected_real_kind(31,308)

print *,selected_real_kind(32,0)
print *,selected_real_kind(32,37)
print *,selected_real_kind(32,291)
print *,selected_real_kind(32,307)
print *,selected_real_kind(32,308)


end
