!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-06-25
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION
!*
!*    Use real*4, real*8, ... , and complex*8, complex*16, ..., in the type
!*    specifier --> Error message
!*    Also expect the same for a derived type, other non-integer types.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

type mytype
  integer*4 :: i4 = 0
  integer*8 :: i8 = 0
end type mytype

! real types
forall(real*4::i=1:10:1)
end forall

forall(real*8::i=1:10:1)
end forall

forall(real*16::i=1:10:1)
end forall

! boolean
forall(logical::i=1:10:1)
end forall

!bogus type name
forall(boolean::i=1:10:1)
end forall

! complex types
forall(complex*8::i=1:10:1)
end forall

forall(complex*16::i=1:10:1)
end forall

forall(complex*32::i=1:10:1)
end forall


! derived type
forall(mytype :: i= 1:10:1)
end forall

forall(type(mytype) :: i= 1:10:1)
end forall


! class(*) and type(*)
forall(class(*) :: i=1:10:1)
end forall

forall(type(*) :: i=1:10:1)
end forall

end


