!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : funDataDTParm.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Zheming Gu
!*  DATE                       : May 11,2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : support for structure constructors containing type parameters to the DATA statement.
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 333315
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Single type structure constructors containing type parameters to the
!*  DATA statement.
!*  
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    TYPE :: DT(K, L)
      INTEGER(8), kind :: K
      INTEGER(8), len  :: L
      integer i1
    END TYPE

    type(dt(4,2)) p1
    integer,parameter :: i = 4
    data p1/dt(i,2)(11)/
    print *,p1%K,p1%L,p1%i1
end


