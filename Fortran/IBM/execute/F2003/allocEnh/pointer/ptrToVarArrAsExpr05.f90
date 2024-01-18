!*  ===================================================================
!*
!*  DATE                       : October 20, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is a an
!*                               Allocated ALLOCATABLE Array of Intrinsic Type
!*                               CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr uses a POINTER that has a TARGET of
!*                               variable and the value of expr has a different
!*                               Shape Result
!*
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
!*  7.4.1.3 Interpretation of intrinsic assignments
!*
!*  If variable is a pointer, the value of expr is assigned to the target
!*  of variable.
!*
!*  If variable is an allocated allocatable variable, it is deallocated if
!*  expr is an array of different shape or any of the corresponding length
!*  type parameter values of variable and expr differ. If variable is or
!*  becomes an unallocated allocatable variable, then it is allocated with
!*  each deferred type parameter equal to the corresponding type parameters
!*  of expr, with the shape of expr, and with each lower bound equal to the
!*  corresponding element of LBOUND(expr).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM ptrToVarArrAsExpr05

    INTEGER(4) :: rc = 10_4

    CHARACTER(2), POINTER :: charArrPtr( : )
    CHARACTER(2), ALLOCATABLE, TARGET :: charArrAlloc( : )


    charArrAlloc = (/ 'ab', 'cd', 'ef', 'gh', 'ij',&
                      'kl', 'mn', 'op', 'qr', 'st' /)
    CALL Dump(10, 2, rc)


    DO i = 1, 4
        charArrPtr => charArrAlloc

        charArrAlloc = [ charArrPtr( (SIZE( charArrPtr ) - 1): ),&
                                    charArrPtr, charArrPtr( :2 ) ]

        rc = rc + 10_4
        CALL Dump((10 + (4 * i)), 2, rc)
    END DO


    PRINT *


    rc = rc + 10_4
    CALL Dump(26, 2, rc)

    DO i = 1, 4
        charArrPtr => charArrAlloc

        charArrAlloc =&
            charArrPtr( 3:(SIZE( charArrPtr ) - 2) )( 2: ) //&
            charArrPtr( 3:(SIZE( charArrPtr ) - 2) )( :2 )

        rc = rc + 10_4
        CALL Dump((26 - (4 * i)), 2, rc)
    END DO


    CONTAINS


        SUBROUTINE Dump(size1, length, failRC)
            INTEGER :: size1
            INTEGER :: length
            INTEGER(4) :: failRC

            CHARACTER(22) :: fmt


            IF (.NOT. ALLOCATED( charArrAlloc )) CALL zzrc( failRC )


            WRITE(fmt, 10) SIZE( charArrAlloc ), LEN( charArrAlloc )
10          FORMAT('("( ",',I2,'(A',I2,'," "),")")')


            PRINT *, SIZE( charArrAlloc ), size1,&
                     LEN( charArrAlloc ), length
            PRINT fmt, charArrAlloc


            IF (SIZE( charArrAlloc ) /= size1) CALL zzrc( (failRC + 1_4) )
            IF (LEN( charArrAlloc ) /= length) CALL zzrc( (failRC + 2_4) )

        END SUBROUTINE Dump

END PROGRAM ptrToVarArrAsExpr05
