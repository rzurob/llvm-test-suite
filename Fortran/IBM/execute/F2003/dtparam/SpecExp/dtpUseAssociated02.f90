!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : dtpUseAssociated02
!*  TEST CASE TITLE            : specification-exprs with MODULE Entities
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : June 30, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : USE Association,
!*  SECONDARY FUNCTIONS TESTED : With multiple levels of USE Association
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mSizes
    IMPLICIT NONE

    INTEGER, PARAMETER :: LABEL_SIZE = 5

END MODULE mSizes

MODULE mBase
    USE mSizes

    IMPLICIT NONE

    TYPE tBase(labelSize)
        INTEGER, LEN :: labelSize

        CHARACTER(labelSize) :: label

        CONTAINS

            GENERIC :: OPERATOR(==) => Equal
            PROCEDURE, PASS :: Equal => TBaseEqual

    END TYPE tBase


    TYPE(tBase(LABEL_SIZE)) :: Base


    CONTAINS

        LOGICAL FUNCTION TBaseEqual(this, that)
            CLASS(tBase(*)), INTENT(in) :: this
            CLASS(tBase(*)), INTENT(in) :: that


            TBaseEqual = .TRUE.
            IF ((this%labelSize /= that%labelSize)      .OR.&
                (LEN( this%label ) /= this%labelSize)   .OR.&
                (LEN( that%label ) /= this%labelSize)   .OR.&
                (this%label /= that%label)) THEN
                TBaseEqual = .FALSE.
            END IF

        END FUNCTION TBaseEqual

END MODULE mBase


MODULE mExt
    USE mBase

    IMPLICIT NONE

    TYPE, EXTENDS(tBase) :: tExt(dataSize)
        INTEGER, LEN :: dataSize

        REAL :: data( dataSize )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => TExtNotEqual

    END TYPE tExt


    TYPE(tExt(Base%labelSize,3)) :: Ext


    CONTAINS

        LOGICAL FUNCTION TExtNotEqual(this, that)
            CLASS(tExt(*,*)), INTENT(in) :: this
            CLASS(tExt(*,*)), INTENT(in) :: that


            TExtNotEqual = .TRUE.
            IF ((this%tBase == that%tBase)              .AND.&
                (this%dataSize == that%dataSize)        .AND.&
                (SIZE( this%data ) == that%dataSize)    .AND.&
                (SIZE( that%data ) == that%dataSize)    .AND.&
                ( ALL(this%data == that%data) )) THEN
                TExtNotEqual = .FALSE.
            END IF

        END FUNCTION TExtNotEqual

END MODULE mExt


MODULE mDoubleExt
    USE mExt

    IMPLICIT NONE

    TYPE, EXTENDS(tExt) :: tDoubleExt(length)
        INTEGER, LEN :: length

        INTEGER :: array( length,length )
    END TYPE tDoubleExt


    TYPE(tDoubleExt(Base%labelSize,Ext%dataSize,2)) :: DoubleExt


    CONTAINS

        LOGICAL FUNCTION TDoubleExtNotEqual(this, that)
            CLASS(tDoubleExt(*,*,*)), INTENT(in) :: this
            CLASS(tDoubleExt(*,*,*)), INTENT(in) :: that


            TDoubleExtNotEqual = .TRUE.
            IF ((.NOT. (this%tExt /= that%tExt))        .AND.&
                (this%length == that%length)            .AND.&
                (SIZE(this%array, 1) == this%length)    .AND.&
                (SIZE(this%array, 2) == this%length)    .AND.&
                (SIZE(that%array, 1) == this%length)    .AND.&
                (SIZE(that%array, 2) == this%length)    .AND.&
                ( ALL(this%array == that%array) )) THEN
                TDoubleExtNotEqual = .FALSE.
            END IF

        END FUNCTION TDoubleExtNotEqual

END MODULE mDoubleExt


MODULE mData
    USE mDoubleExt

    IMPLICIT NONE

    INTEGER, PARAMETER :: MAX_ELEMENTS = 3

    CHARACTER(Base%labelSize) :: labelList( MAX_ELEMENTS )

    CONTAINS

        SUBROUTINE LabelListInit(labelList, expectedLen, rc)
            CHARACTER(*) :: labelList( : )
            INTEGER :: expectedLen
            INTEGER(4) :: rc

            CHARACTER(30) :: formatStr
            INTEGER :: i

            IF (LEN( labelList ) /= expectedLen)    CALL zzrc( rc )

            WRITE(formatStr, '("(I",I1,".",I1,")")')&
                    LEN( labelList ), LEN( labelList )

            DO i = 1, SIZE( labelList )
                WRITE(labelList( i ), formatStr) i
            END DO

        END SUBROUTINE LabelListInit

        SUBROUTINE RealArrayInit( array )
            REAL :: array( :,: )

            INTEGER :: i
            INTEGER :: j


            DO j = 1, UBOUND(array, 2)
                DO i = 1, UBOUND(array, 1)
                    array( i,j ) = 1.0 / REAL( i )
                END DO
            END DO

        END SUBROUTINE RealArrayInit

        SUBROUTINE IntArrayInit(intArray, length, rc)
            INTEGER :: intArray( :,:,: )
            INTEGER :: length
            INTEGER(4) :: rc

            INTEGER :: i
            INTEGER :: j
            INTEGER :: k
            INTEGER :: l


            IF (SIZE(intArray, 1) /= length)        CALL zzrc( rc )
            IF (UBOUND(intArray, 2) /= length)      CALL zzrc( (rc + 1_4) )

            l = 0
            DO k = 1, UBOUND(intArray, 3)
                DO j = 1, UBOUND(intArray, 2)
                    DO i = 1, UBOUND(intArray, 1)
                        intArray( i,j,k ) = l
                        l = l + 1
                    END DO
                END DO
            END DO

        END SUBROUTINE IntArrayInit

END MODULE mData


MODULE mTestBase
    USE mData

    IMPLICIT NONE

    CONTAINS

        SUBROUTINE TestBase( rc )
            INTEGER(4) :: rc

            INTEGER :: i

            CHARACTER(5) :: controlLabels( 3 )

            TYPE(tBase(Ext%labelSize)) :: baseTest( MAX_ELEMENTS )
            TYPE(tBase(5)) :: baseControl( 3 )


            CALL LabelListInit(controlLabels, 5, rc)

            DO i = 1, 3
                baseControl( i ) = tBase(5)(controlLabels( i ))
            END DO


            CALL LabelListInit(labelList, 5, (rc + 5_4))

            DO i = 1, 3
                baseTest( i ) = tBase(Base%labelSize)(labelList( i ))
            END DO


            DO i = 1, 3
                IF (.NOT. (baseTest( i ) == baseControl( i )))&
                                CALL zzrc( (rc + 10_4 + INT(i, 4)) )
            END DO

        END SUBROUTINE TestBase

END MODULE mTestBase


PROGRAM dtpUseAssociated02
    USE mTestBase

    IMPLICIT NONE


    INTERFACE
        SUBROUTINE TestDoubleExt(realArray, rc)
            REAL :: realArray( :,: )
            INTEGER(4) :: rc
        END SUBROUTINE TestDoubleExt
    END INTERFACE


    REAL :: realArray( -Ext%dataSize:-1,MAX_ELEMENTS )
    REAL :: controlRealArray( 3,3 )


    CALL TestBase( 10_4 )
    CALL TestExt( 50_4 )
    CALL TestDoubleExt(realArray, 100_4)

    CONTAINS

        SUBROUTINE TestExt( rc )
            INTEGER(4) :: rc

            INTEGER :: i

            TYPE(tExt(Ext%labelSize,Ext%dataSize)) :: extTest( MAX_ELEMENTS )
            TYPE(tExt(5,3)) :: extControl( 3 )


            CALL RealArrayInit( controlRealArray )

            DO i = 1, 3
                extControl( i ) =&
                    tExt(5,3)(labelList( i ),controlRealArray( :,i ))
            END DO


            CALL RealArrayInit( realArray )
            IF (LBOUND(realArray, 1) /= -3)     CALL zzrc( (rc + 1_4) )
            IF (UBOUND(realArray, 1) /= -1)     CALL zzrc( (rc + 2_4) )


            DO i = 1, 3
                extTest( i ) =&
                    tExt(Ext%labelSize,Ext%dataSize)&
                        (labelList( i ),realArray( :,i ))
            END DO


            DO i = 1, 3
                IF (extTest( i ) /= extControl( i ))&
                                CALL zzrc( (rc + 5_4 + INT(i, 4)) )
            END DO

        END SUBROUTINE TestExt

END PROGRAM dtpUseAssociated02


SUBROUTINE TestDoubleExt(realArray, rc)
    USE mData

    IMPLICIT NONE

    REAL :: realArray( :,: )
    INTEGER(4) :: rc

    INTEGER :: i

    INTEGER :: intArray( DoubleExt%length,DoubleExt%length,MAX_ELEMENTS )
    INTEGER :: controlIntArray( 2,2,3 )

    TYPE(tDoubleExt(DoubleExt%labelSize,DoubleExt%dataSize,DoubleExt%length))&
                                        :: doubleExtTest( 0:(MAX_ELEMENTS -1) )
    TYPE(tDoubleExt(5,3,2)) :: doubleExtControl( 3 )


    CALL IntArrayInit(controlIntArray, 2, rc)

    DO i = 1, 3
        doubleExtControl( i ) =&
            tDoubleExt(5,3,2)(labelList( i ),realArray( :,i ),&
                                        controlIntArray( :,:,i ))
    END DO


    CALL IntArrayInit(intArray, 2, (rc + 5_4))

    DO i = 1, 3
        doubleExtTest( (i - 1) ) =&
            tDoubleExt(DoubleExt%labelSize,DoubleExt%dataSize,DoubleExt%length)&
                (labelList( i ),realArray( :,i ),intArray( :,:,i ))
    END DO


    DO i = 1, 3
        IF ( TDoubleExtNotEqual(doubleExtTest( (i - 1) ),&
                doubleExtControl( i )) )    CALL zzrc( (rc + 10_4 + INT(i, 4)) )
    END DO

END SUBROUTINE TestDoubleExt
