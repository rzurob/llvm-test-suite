!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : forall_typespec_f01.f
!*
!*  DATE                       : 2012-06-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED : nesting forall in another block
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION
!*
!*    Reuse an external variable as a FORALL index variable --> FORALL should
!*    not alter its value.
!*    FORALL nested in another block --> FORALL should not alter an index value
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

integer :: i = 10
integer :: a(5) = 0, b(5) = (/ 11, 22, 33, 44, 55 /)

forall(integer :: i = 1:2:1)
end forall

if (i .ne. 10) then
  print *, "forall incrementer modified an external scope variable"
  error stop 11
else

  forall(integer*1 :: i = 1:3:1)
  end forall

  if (i .ne. 10) then
    print *, "forall construct in an else block  modified an external scope variable"
    error stop 22
  end if

  forall(integer*1 :: i = 1:4:1) a(i) = i

  if (i .ne. 10) then
    print *, "forall assignment in an else block modified an external scope variable"
    error stop 33
  end if

end if

! i in the block should have limited scope and should not modify the global i
block
  integer*2::i = -1

  ! forall should not modify this local i
  forall(integer*1 :: i = 1:4:1) a(i) = i

  if (i .ne. -1) then
    print *, "forall assignment in a block modified an external scope variable"
    error stop 44
  end if

end block

! global i should still be 10
select case (i)
  case(10)
    forall(integer*1 :: i = 1:5:1) a(i) = i*10 + i
    if (i .ne. 10) then
      print *, "forall assignment in a case block modified an external scope variable"
      error stop 55
    end if
  case default
    print *, "forall incrementer modified an external scope variable"
    error stop 66
end select

if (any(a /= b )) error stop 77

end
