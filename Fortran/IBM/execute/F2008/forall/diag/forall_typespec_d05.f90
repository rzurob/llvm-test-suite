!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-06-25
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED : out-of-bounds value in the forall-triplet-spec
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION
!*
!*    For each integer type, declare an out-of-bounds value in
!*    the forall-triplet-spec list --> Error message.
!*
!*    Example: forall(integer(1)::i=1:16384:1)
!*
!*    Note: Also verifies the error message is the same in a forall loop without
!*    a type specifier.
!*    Note: It is found that there is an existing issue where with integer(1)
!*    and integer(2) types there is no error message with forall in general.
!*    The front end will default the literal to the maximum value allowed for
!*    the type specified, but the loop will be set to iterate 0 times.
!*    Note: This is generally considered to be a user error and an error message
!*    is not issued.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890


implicit none

integer(1)  :: i  , ii=0  , j1
integer(2)  :: i2 , ii2=0 , j2
integer(4)  :: i4 , ii4=0 , j4
integer(8)  :: i8 , ii8=0 , j8
logical(1)  :: l1(65536)
! expect an error message here since 16384 is more than the maximum for integer(1)
forall (i = 1:16:2)
l1(i) = .TRUE.
end forall

! should get same error message as above to be consistent with the behaviour before the extension
forall (integer(1)::j=1:16384:2)
  j1 = j1 + 1
end forall


! expect an error message here since 2^16=65536 is more than the maximum for integer(2)
forall (i2 = 1:65536:2)
  ii2 = ii2 + 1
end forall

! should get same error message as above to be consistent with the behaviour before the extension
forall (integer(2)::j=1:65536:2)
  j2 = j2 + 1
end forall


! expect an error message here since 2^32=4294967296 is more than the maximum for integer(4)
forall (i4 = 1:4294967296:2)
  ii4 = ii4 + 1
end forall

! should get same error message as above to be consistent with the behaviour before the extension
forall (integer(4)::j=1:4294967296:2)
  j4 = j4 + 1
end forall


! expect an error message here since 2^64=18446744073709551616 is more than the maximum for integer(8)
forall (i8 = 1:18446744073709551616:2)
  ii8 = ii8 + 1
end forall

! should get same error message as above to be consistent with the behaviour before the extension
forall (integer(8)::j=1:18446744073709551616:2)
end forall

print *, "ii...ii8 = ",ii, " ", ii2, " ", ii4, " ", ii8
print *, "j1...j8 = ", j1, " ", j2, " ", j4, " ", j8
print *, "l1 = ", l1

end
