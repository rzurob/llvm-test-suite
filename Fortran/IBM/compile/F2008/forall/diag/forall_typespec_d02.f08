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
!*    Use multiple type specifiers, forall(integer::i=1:10:1, integer::j=1:10:1)
!*       --> Error message
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

forall(integer::i=1:10:1, integer::j=1:10:1)
end forall
end