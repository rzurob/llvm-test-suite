!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpEquiv5 
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jul. 05, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration and specification
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   
!* 
!*  -- The equivalence statement
!* 
!*    Impicit typing rules 
!*  
!*  ()
!*   
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT_R(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    REAL(K)       :: R(L)=K
  END TYPE
 
  TYPE :: DT_C(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    CHARACTER(L)  :: C(L)=CHAR(48+K)
  END TYPE
 
  TYPE :: DT_I(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    INTEGER(K)    :: I(L)=K
  END TYPE
 
  TYPE :: DT_L(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    LOGICAL(K)    :: A(L)=.TRUE.
  END TYPE
 
  TYPE :: DT_Z(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    COMPLEX(K)    :: Z(L)=(K,-K)
  END TYPE
 
  END MODULE

  PROGRAM dtpEquiv5 
  USE M

  IMPLICIT TYPE(DT_R(16,7))(R)
  IMPLICIT TYPE(DT_C(1,5))(C)
  IMPLICIT TYPE(DT_I(2,7))(I)
  IMPLICIT TYPE(DT_L(8,2))(L)
  IMPLICIT TYPE(DT_Z(16,9))(Z)


  REAL(16)          :: R1(7)
  CHARACTER(5)      :: C1(5)
  INTEGER(2)        :: I1(7)
  LOGICAL(8)        :: L1(2)
  COMPLEX(16)       :: Z1(9)

  
  EQUIVALENCE(R, R1)
  EQUIVALENCE(C, C1)
  EQUIVALENCE(I, I1)
  EQUIVALENCE(l, l1)
  EQUIVALENCE(Z, Z1)


  IF ( ANY( R%R .NE.    16         ) ) STOP 11
  R1 = -16
  IF ( ANY( R%R .NE.    -16        ) ) STOP 21

  IF ( ANY( C%C .NE.    CHAR(48+1) ) ) STOP 12
  C1 = CHAR(0)
  IF ( ANY( C%C .NE.    CHAR(0)    ) ) STOP 22

  IF ( ANY( I%I .NE.    2         ) ) STOP 13
  I1 = -2
  IF ( ANY( I%I .NE.     -2         ) ) STOP 23

  IF ( ANY( L%A .NEQV. .true.     ) ) STOP 14
  L1 = .false.
  IF ( ANY( L%A .NEQV. .false.      ) ) STOP 24

  IF ( ANY( Z%Z .NE.    (16,-16)  ) ) STOP 15
  Z1 = -(16,-16)
  IF ( ANY( Z%Z .NE.    -(16,-16)  ) ) STOP 25

  END


