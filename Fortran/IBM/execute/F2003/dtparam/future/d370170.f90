!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2009-09-30
!*
!*  DESCRIPTION                : miscellenous (defect 370170)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


  PROGRAM dtParamInitExpr1
  IMPLICIT NONE

  TYPE DT1(K1, K2, K3)
    INTEGER,          KIND :: K1
    INTEGER(KIND=K1), KIND :: K2=MAXVAL((/1, 2*K1/))
    INTEGER(KIND=K2), KIND :: K3=MAXVAL((/4*K1,2*K2/))
  END TYPE

  INTEGER :: I

  TYPE, EXTENDS(DT1) :: DT2(L1, L2, L3)
    INTEGER(KIND=K1), LEN  :: L1=SIZE((/(DT1(K1)(), i=1,K1)/)  )
    INTEGER(KIND=K2), LEN  :: L2=SIZE((/(DT1(K2)(), i=1,K2)/)  )
    INTEGER(KIND=K3), LEN  :: L3=SIZE((/(DT1(K3)(), i=1,K3)/)  )
    TYPE(DT1(K1, K2, K3))  :: Arr1(L1, L2, L3)=DT1(K1, K2, K3)()
  END TYPE


  END


