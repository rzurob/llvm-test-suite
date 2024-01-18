!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : forall_typespec_d02.f
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 2012-06-25
!*  ORIGIN                     : 
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
