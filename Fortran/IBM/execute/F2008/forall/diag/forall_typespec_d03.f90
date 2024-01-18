!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : forall_typespec_d03.f
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 2012-06-25
!*  ORIGIN                     : 
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED : Language level diagnostic message
!*  ADAPTED FROM               : 
!*
!*  DESCRIPTION
!*
!*    Use type-specifier with -qlanglvl=2003STD --> Language level message
!*    Use type-specifier with -qlanglvl=95STD --> Language level message
!*    Use type-specifier with -qlanglvl=90STD --> Language level message
!*    Use type-specifier with -qlanglvl=77STD --> Language level message
!*    
!*    These are run in forall_typespec_d03_1, forall_typespec_d03_2, ...,
!*    forall_typespec_d03_4 scenarios for each langlvl above.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

forall(integer::i=1:10:1)
end forall
end
