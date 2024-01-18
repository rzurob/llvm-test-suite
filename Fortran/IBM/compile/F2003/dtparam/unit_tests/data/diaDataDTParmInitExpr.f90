!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : diaDataDTParmInitExpr.f
!*
!*  DATE                       : May 11,2007
!*
!*  PRIMARY FUNCTIONS TESTED   : support for structure constructors containing type parameters to the DATA statement.
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 333315
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  KIND and LEN type parameter is not scalar init-expr, or is assumed
!*  or deferred in
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
    data p1,j /dt1(i,:,:,j)(11,22),4/
end


