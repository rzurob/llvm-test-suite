!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : variableIsStructComp01 - variable is a
!*                               Subobject
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : November  8, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is a
!*                               Structure Component that is ALLOCATABLE
!*                               Array
!*  SECONDARY FUNCTIONS TESTED : and expr is of the same type as variable
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  7.4.1.1 General form
!*
!*  R734 assignment-stmt  is  variable = expr
!*
!*
!*  7.4.1.3 Interpretation of intrinsic assignments
!*
!*  If variable is an allocated allocatable variable, it is deallocated if
!*  expr is an array of different shape or any of the corresponding length
!*  type parameter values of variable and expr differ. If variable is or
!*  becomes an unallocated allocatable variable, then it is allocated with
!*  each deferred type parameter equal to the corresponding type parameters
!*  of expr, with the shape of expr, and with each lower bound equal to the
!*  corresponding element of LBOUND(expr).
!*
!*  When variable is a subobject, the assignment does not affect the
!*  definition status or value of other parts of the object. For example,
!*  if variable is an array section, the assignment does not affect the
!*  definition status or value of the elements of the array not specified
!*  by the array section.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM variableIsStructComp01

    TYPE :: tType
        CHARACTER(:), ALLOCATABLE :: cA( : )
    END TYPE tType

    INTEGER(4) :: rc = 0_4

    CHARACTER(1) :: chr( 64 ) =&
        [ (ACHAR( i ), i = IACHAR( 'A' ), IACHAR( 'Z' )),&
          (ACHAR( i ), i = IACHAR( 'a' ), IACHAR( 'z' )),'-',&
          (ACHAR( i ), i = IACHAR( '0' ), IACHAR( '9' )),'+' ]

    TYPE(tType), ALLOCATABLE :: tT
    

    ALLOCATE( tT )

    DO i = 1, 31
        IF (MOD(i, 2) == 0) THEN
            m = 32 - i
        ELSE
            m = 32 + i
        END IF

        tT%cA = chr( m: )

        rc = rc + 5_4
        CALL Check(m, rc)
    END DO


    CONTAINS


        SUBROUTINE Check(m, rc)
            INTEGER :: m
            INTEGER(4) :: rc


            IF (.NOT. ALLOCATED( tT%cA )) CALL zzrc( rc )

            PRINT *, rc, m, SIZE( tT%cA ), '(', tT%cA, ')'

            IF (SIZE( tT%cA ) /= (64 - m + 1))  CALL zzrc( (rc + 1_4) )
            IF ( ANY( (tT%cA /= chr( m: )) ) )  CALL zzrc( (rc + 2_4) )

        END SUBROUTINE Check

END PROGRAM variableIsStructComp01
