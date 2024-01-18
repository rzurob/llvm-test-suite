!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignBoundsSpec03
!*                               and Remapping)
!*
!*  DATE                       : June  1, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Bounds Specification for an array of Derived
!*                               Type (with Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : Where data-target has zero-sized elements,
!*                               or is of zero length
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Bounds Specification for an array of a Container Type where the:
!*  o  Contents is an Array with zero-sized Elements (Storage Sequences),
!*  o  Contents is an Array with zero Elements, and
!*  o  Container Array contains zero Elements.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mType1
    IMPLICIT NONE

    TYPE tType1(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        REAL(k1) :: data( l1 )
    END TYPE tType1

END MODULE mType1


MODULE mType2
    IMPLICIT NONE

    TYPE tType2(l1)
        INTEGER, LEN :: l1

        CHARACTER(l1) :: name
    END TYPE tType2

END MODULE mType2


MODULE mType3
    IMPLICIT NONE

    TYPE tType3(k1,l1,l2)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1
        INTEGER, LEN :: l2

        CHARACTER(l1) :: name
        REAL(k1) :: data( l2 )
    END TYPE tType3

END MODULE mType3


MODULE mContainer
    USE mType1
    USE mType2
    USE mType3

    IMPLICIT NONE

    TYPE :: tContainer(l1)
        INTEGER, LEN :: l1

        CLASS(*), ALLOCATABLE :: contents( : )
    END TYPE tContainer

END MODULE mContainer


PROGRAM dtpPtrAssignBoundsSpec03
    USE mContainer

    IMPLICIT NONE

    INTERFACE
        SUBROUTINE CheckZeroSizedContents(zeroSizedTType1, rc)
            IMPORT tType1
            TYPE(tType1(8,*)) :: zeroSizedTType1( : )
            INTEGER(4) :: rc
        END SUBROUTINE CheckZeroSizedContents

        SUBROUTINE CheckZeroLengthContents(zeroLengthTType2, n, rc)
            IMPORT tType2
            TYPE(tType2(*)), TARGET :: zeroLengthTType2( : )
            INTEGER :: n
            INTEGER(4) :: rc
        END SUBROUTINE CheckZeroLengthContents

        SUBROUTINE CheckZeroLengthContainer(zeroLengthTType3, n,&
                                                dim1, dim2, dim3, rc)
            IMPORT tType3
            TYPE(tType3(16,*,*)), TARGET :: zeroLengthTType3( : )
            INTEGER :: n
            INTEGER :: dim1
            INTEGER :: dim2
            INTEGER :: dim3
            INTEGER(4) :: rc
        END SUBROUTINE CheckZeroLengthContainer
    END INTERFACE

    INTEGER :: i
    INTEGER :: j

    REAL(16) :: real16Array( 27 ) = [ ((1.0_16 / REAL(i, 16)), i = 1, 27) ]

    TYPE(tType1(8,0)) :: zeroSizedTType1( 10 )
    TYPE(tType2(:)), ALLOCATABLE :: zeroLengthTType2( : )
    TYPE(tType3(16,:,:)), POINTER :: zeroLengthTType3( : )


    CALL CheckZeroSizedContents(zeroSizedTType1, 10_4)


    ALLOCATE(zeroLengthTType2( 9 ), SOURCE=[                        &
                ((tType2(2)((CHAR( (48 + i) ) // CHAR( (48 + j) ))),&
                                                i = 1, 3), j = 1, 3) ])
    CALL CheckZeroLengthContents(zeroLengthTType2, 0, 20_4)


    ALLOCATE(zeroLengthTType3( 27 ), SOURCE=[ (tType3(16,3,1)(              &
            '0' // CHAR( (48 + (i / 10)) ) // CHAR( (48 + MOD(i, 10)) ),    &
                                            real16Array( i:i )), i = 1, 27) ])
    CALL CheckZeroLengthContainer(zeroLengthTType3, 1, 3, 0, 3, 100_4)

END PROGRAM dtpPtrAssignBoundsSpec03


SUBROUTINE CheckZeroSizedContents(zeroSizedTType1, rc)
    USE mContainer

    IMPLICIT NONE

    TYPE(tType1(8,*)) :: zeroSizedTType1( : )
    INTEGER(4) :: rc

    INTEGER :: i
    INTEGER :: l
    INTEGER :: m
    INTEGER :: n

    TYPE(tContainer(:)), ALLOCATABLE, TARGET :: zeroSizedContents( : )
    TYPE(tContainer(:)), POINTER :: ptrZeroSizedContents( : )


    n = SIZE( zeroSizedTType1 ) / 5
    ALLOCATE(tContainer(n) :: zeroSizedContents( 5 ))

    DO i = 1, 5
        l = ((i - 1) * n) + 1
        m = l + n - 1
        zeroSizedContents( i ) = tContainer(n)(zeroSizedTType1( l:m ))
    END DO


    ptrZeroSizedContents( -5: ) => zeroSizedContents
    IF (.NOT. ASSOCIATED( ptrZeroSizedContents ) ) THEN
        CALL zzrc( rc )

    ELSE IF (.NOT. ASSOCIATED(ptrZeroSizedContents, zeroSizedContents)) THEN
        CALL zzrc( (rc + 1) )

    ELSE IF (SIZE( ptrZeroSizedContents ) /= 5) THEN
        CALL zzrc( (rc + 2) )

    ELSE IF (LBOUND(ptrZeroSizedContents, 1) /= -5) THEN
        CALL zzrc( (rc + 3) )

    ELSE IF (UBOUND(ptrZeroSizedContents, 1) /= -1) THEN
        CALL zzrc( (rc + 4) )
    END IF

END SUBROUTINE CheckZeroSizedContents


SUBROUTINE CheckZeroLengthContents(zeroLengthTType2, n, rc)
    USE mContainer

    IMPLICIT NONE

    TYPE(tType2(*)), TARGET :: zeroLengthTType2( : )
    INTEGER :: n
    INTEGER(4) :: rc

    INTEGER :: i
    INTEGER :: j
    INTEGER :: k
    INTEGER :: l

    TYPE(tContainer(n)), POINTER :: zeroLengthContents( :,: )
    TYPE(tContainer(:)), POINTER :: ptrZeroLengthContents( :,: )


    ALLOCATE( zeroLengthContents( 3,3 ) )
    DO j = 1, 3
        k = (j - 1) * 3

        DO i = 1, 3
            l = k + j
            zeroLengthContents( i,j ) = tContainer(n)(zeroLengthTType2( l:n ))
        END DO
    END DO


    ptrZeroLengthContents( 5:,-5: ) => zeroLengthContents
    IF (.NOT. ASSOCIATED( ptrZeroLengthContents ) ) THEN
        CALL zzrc( rc )

    ELSE IF (.NOT. ASSOCIATED(ptrZeroLengthContents, zeroLengthContents)) THEN
        CALL zzrc( (rc + 1) )

    ELSE IF (SIZE( ptrZeroLengthContents ) /= 9) THEN
        CALL zzrc( (rc + 2) )

    ELSE IF (LBOUND(ptrZeroLengthContents, 1) /= 5) THEN
        CALL zzrc( (rc + 3) )

    ELSE IF (UBOUND(ptrZeroLengthContents, 1) /= 7) THEN
        CALL zzrc( (rc + 4) )

    ELSE IF (LBOUND(ptrZeroLengthContents, 2) /= -5) THEN
        CALL zzrc( (rc + 5) )

    ELSE IF (UBOUND(ptrZeroLengthContents, 2) /= -3) THEN
        CALL zzrc( (rc + 6) )
    END IF

END SUBROUTINE CheckZeroLengthContents


SUBROUTINE CheckZeroLengthContainer(zeroLengthTType3, n, dim1, dim2, dim3, rc)
    USE mContainer

    IMPLICIT NONE

    TYPE(tType3(16,*,*)), TARGET :: zeroLengthTType3( : )
    INTEGER :: n
    INTEGER :: dim1
    INTEGER :: dim2
    INTEGER :: dim3
    INTEGER(4) :: rc

    INTEGER :: i
    INTEGER :: j
    INTEGER :: k
    INTEGER :: l
    INTEGER :: m
    INTEGER :: p

    INTEGER :: dimSizeArray( 3 )

    TYPE(tContainer(n)), TARGET :: zeroLengthContainer( dim1,dim2,dim3 )
    TYPE(tContainer(:)), POINTER :: ptrZeroLengthContainer( :,:,: )


    DO k = 1, dim3
        l = (k - 1) * (dim2 * dim1)
        DO j = 1, dim2
            m = (j - 1) * dim1
            DO i = 1, dim1
                p = l + m + i
                p = ((p - 1) * n) + 1
                ALLOCATE(zeroLengthContainer( i,j,k )%contents( n ),&
                            SOURCE=zeroLengthTType3( p:(p + n - 1) ))
            END DO
        END DO
    END DO


    ptrZeroLengthContainer( -1:,0:,1: ) => zeroLengthContainer
    IF (.NOT. ASSOCIATED( ptrZeroLengthContainer ) ) THEN
        CALL zzrc( rc )

    !  Array is of zero size (zero elements), thus the POINTER is not
    !  ASSOCIATED with the TARGET Array.
    !  (Ref:  F2003 Standard, Section:  13.7.13, Case(v)
    ELSE IF ( ASSOCIATED(ptrZeroLengthContainer, zeroLengthContainer) ) THEN
        CALL zzrc( (rc + 1) )

    ELSE IF (SIZE( ptrZeroLengthContainer ) /= (dim1 * dim2 * dim3)) THEN
        CALL zzrc( (rc + 2) )
    END IF


    dimSizeArray = [ dim1, dim2, dim3 ]

    DO i = 1, 3
        k = i
        j = i - 2
        IF (dimSizeArray( i ) == 0) THEN
            k = 0
            j = 1
        END IF

        IF (LBOUND(ptrZeroLengthContainer, i) /= j) THEN
            CALL zzrc( (rc + 10_4 + INT(i, 4)) )

        ELSE IF (UBOUND(ptrZeroLengthContainer, i) /= k) THEN
            PRINT *, i, UBOUND(ptrZeroLengthContainer, i)
            CALL zzrc( (rc + 20_4 + INT(i, 4)) )

        ELSE IF (SIZE(ptrZeroLengthContainer, i) /= dimSizeArray( i )) THEN
            CALL zzrc( (rc + 30_4 + INT(i, 4)) )
        END IF
    END DO

END SUBROUTINE CheckZeroLengthContainer
