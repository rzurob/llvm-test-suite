!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : funDataDTParmExtend.f
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
!*  Extended type structure constructors containing type parameters to
!*  the DATA statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    TYPE :: DT(K, L)
      INTEGER(8), kind :: K
      INTEGER(8), len  :: L
      integer i1
    END TYPE

    TYPE, EXTENDS(DT) :: DT1(K1,L1)
      INTEGER(1), kind :: K1
      INTEGER(1), len  :: L1
      integer i2

    END TYPE
    type(dt1(4,4,2,2)) p1
    integer,parameter :: i = 4
    integer j
    data p1,j /dt1(4,4,2,2)(11,22),4/
    print *,p1%K,p1%L,p1%K1,p1%L1
    print *,p1%i1,p1%i2
end


