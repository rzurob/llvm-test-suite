!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : impure08f.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Paul Liu
!*  DATE                       : 2012-03-08
!*  ORIGIN                     : Compiler Development, IBM China Development Shanghai Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : impure procedures
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 917300
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION                : Fortran 2008 support for the IMPURE attribute 
!*                               for procedures,which allows for ELEMENTAL procedures
!*                               without the restrictions of PURE. 
!*                               C1287 is not apply to IMPURE procedures
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
      impure elemental subroutine sub(para)
        implicit none
        integer,intent(in):: para
        if(para .ne. 11 ) stop
        if(para .ne. 11 ) error stop
      end
