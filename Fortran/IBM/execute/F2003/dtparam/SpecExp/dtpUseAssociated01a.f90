!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : July 10, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USE Association with one level of USE
!*                               Association
!*  SECONDARY FUNCTIONS TESTED : and Default Initialization
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mDataEntity
    IMPLICIT NONE

    INTEGER, PARAMETER :: TESTS = 10
    INTEGER, PARAMETER :: DATUM = 100

    INTEGER, PARAMETER :: DATA_SETS = 10

    TYPE tDataEntity
        CHARACTER(10) :: label
        COMPLEX(8) :: data( DATUM,TESTS )
    END TYPE tDataEntity

    TYPE(tDataEntity) :: testData( DATA_SETS )

    CONTAINS

        FUNCTION ReadDataSet( n )
            INTEGER :: n

            COMPLEX(8) :: ReadDataSet( n )

            INTEGER :: i

            DO i = 1, n
                ReadDataSet( i ) =&
                    ((1.0_8 / REAL(i, 8)),(REAL(i, 8) / REAL(n, 8)))
            END DO

        END FUNCTION ReadDataSet

        SUBROUTINE GetTestData( fileList )
            CHARACTER(*) :: fileList( : )

            INTEGER :: i

            DO i = 1, SIZE( fileList )
                testData( i ) =&
                    tDataEntity(fileList( i ),&
                        RESHAPE(ReadDataSet( SIZE( testData( i )%data ) ),&
                                                            [ DATUM,TESTS ]))
            END DO

        END SUBROUTINE GetTestData

END MODULE mDataEntity


MODULE mNewDataEntity
    USE mDataEntity,&
        OLD_TESTS => TESTS,&
        OLD_DATUM => DATUM,&
        OLD_DATA_SETS => DATA_SETS,&
        oldTestData => testData,&
        tOldDataEntity => tDataEntity

    IMPLICIT NONE

    TYPE tDataEntity(labelSize,dataKind,datum,tests)
        INTEGER, LEN :: labelSize = 10
        INTEGER, KIND :: dataKind
        INTEGER, LEN :: datum = 100
        INTEGER, LEN :: tests = 10

        CHARACTER(labelSize) :: label = 'NewDataset'
        COMPLEX(dataKind) :: data( datum,tests )
    END TYPE tDataEntity

    TYPE(tDataEntity(:,8,:,:)), ALLOCATABLE :: testData( : )

    CONTAINS

        SUBROUTINE ConvertOld2New( )

            INTEGER :: i

            ALLOCATE(                                               &
                tDataEntity(MAXVAL( LEN_TRIM( oldTestData%label ) ),&
                                        8,OLD_DATUM,OLD_TESTS)      &
                                        :: testData( SIZE( oldTestData )))

            DO i = 1, SIZE( oldTestData )
                testData( i ) =                                         &
                    tDataEntity(testData%labelSize,testData%dataKind,   &
                                testData%datum,testData%tests)          &
                                    (oldTestData( i )%label,            &
                                        oldTestData( i )%data)
            END DO

        END SUBROUTINE ConvertOld2New

END MODULE mNewDataEntity


MODULE mVerificationRoutines
    USE mNewDataEntity

    IMPLICIT NONE

    CONTAINS

        SUBROUTINE VerifyLabelSize(i, fileList, rc)
            INTEGER :: i
            CHARACTER(*) :: fileList( : )
            INTEGER(4) :: rc

            INTEGER :: maxLabelSize


            maxLabelSize = MAXVAL( LEN_TRIM( fileList ) )

            IF (testData( i )%labelSize <&
                    LEN_TRIM( oldTestData( i )%label )) CALL zzrc( rc )
            IF (testData( i )%labelSize /=&
                    LEN( testData( i )%label ))         CALL zzrc( (rc + 1_4) )
            IF (testData( i )%labelSize /=&
                    maxLabelSize)                       CALL zzrc( (rc + 2_4) )

        END SUBROUTINE VerifyLabelSize


        SUBROUTINE VerifyDataKind(i, rc)
            INTEGER :: i
            INTEGER(4) :: rc


            IF (testData( i )%dataKind /=&
                    KIND( oldTestData( i )%data ))      CALL zzrc( rc )
            IF (testData( i )%dataKind /=&
                    KIND( testData( i )%data ))         CALL zzrc( (rc + 1_4) )
            IF (testData( i )%dataKind /= 8)            CALL zzrc( (rc + 2_4) )

        END SUBROUTINE VerifyDataKind


        SUBROUTINE VerifyDatum(i, rc)
            INTEGER :: i
            INTEGER(4) :: rc


            IF (testData( i )%datum /=&
                    SIZE(oldTestData( i )%data, 1))     CALL zzrc( rc )
            IF (testData( i )%datum /=&
                    SIZE(testData( i )%data, 1))        CALL zzrc( (rc + 1_4) )
            IF (testData( i )%datum /= OLD_DATUM)       CALL zzrc( (rc + 2_4) )

        END SUBROUTINE VerifyDatum


        SUBROUTINE VerifyTests(i, rc)
            INTEGER :: i
            INTEGER(4) :: rc


            IF (testData( i )%tests /=&
                    SIZE(oldTestData( i )%data, 2))     CALL zzrc( rc )
            IF (testData( i )%tests /=&
                    SIZE(testData( i )%data, 2))        CALL zzrc( (rc + 1_4) )
            IF (testData( i )%tests /= OLD_TESTS)       CALL zzrc( (rc + 2_4) )

        END SUBROUTINE VerifyTests

END MODULE mVerificationRoutines


PROGRAM dtpUseAssociated01a
    USE mNewDataEntity
    USE mVerificationRoutines

    IMPLICIT NONE

    CHARACTER(10) :: fileList( OLD_DATA_SETS ) =&
                        [   CHARACTER(10) ::                        &
                            "file1.dat", "file2", "file.dat",       &
                            "data4", "d5", "outliers", "cvt.dat",   &
                            "corrupted", "tst8.dt", "misc"          ]


    CALL GetTestData( fileList )
    CALL ConvertOld2New( )
    CALL VerifyConversion( fileList )

    CONTAINS

        SUBROUTINE VerifyConversion( fileList )
            CHARACTER(*) :: fileList( : )

            INTEGER :: i
            INTEGER :: rcBase


            IF (.NOT. ALLOCATED( testData ))                CALL zzrc( 99_4 )
            IF (SIZE( testData ) /= SIZE( oldTestData ))    CALL zzrc( 199_4 )

            DO i = 1, SIZE( testData )

                rcBase = (i - 1) * 100
                CALL VerifyLabelSize(i, fileList, INT((rcBase +  10), 4))
                CALL VerifyDataKind(i, INT((rcBase + 20), 4))
                CALL VerifyDatum(i, INT((rcBase + 30), 4))
                CALL VerifyTests(i, INT((rcBase + 40), 4))

                IF ( ANY(testData( i )%data /= oldTestData( i )%data) )&
                                        CALL zzrc( INT((rcBase + 50), 4) )
            END DO

        END SUBROUTINE VerifyConversion

END PROGRAM dtpUseAssociated01a
