!*  ===================================================================
!*
!*                               Attribute in Assumed-Shape Array Arguments
!*
!*  DATE                       : April  7, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Assumed-Shape Array
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument implicitly has the ASYNCHRONOUS
!*                               Attribute, and is *NOT* an Assumed-Shape Array
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASYNCHRONOUS Attribute
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  12.4.1.2 Actual arguments associated with dummy data objects
!*
!*  If the actual argument is an array section having a vector subscript,
!*  the dummy argument is not definable and shall not have the INTENT (OUT),
!*  INTENT (INOUT), VOLATILE, or ASYNCHRONOUS attributes.
!*
!*  C1232 (R1221) If an actual argument is an array section or an
!*                assumed-shape array, and the corresponding dummy argument
!*                has either the VOLATILE or ASYNCHRONOUS attribute, that
!*                dummy argument shall be an assumed-shape array.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE ImplicitAsynch
    CONTAINS

        SUBROUTINE AsynchWrite(ioUnit, shouldBeAssumeShape)
            INTEGER :: ioUnit
            INTEGER, DIMENSION( 10 ) :: shouldBeAssumeShape


            WRITE(ioUnit, ASYNCHRONOUS='yes')&
                (shouldBeAssumeShape( i ), i = 1, 10)

        END SUBROUTINE AsynchWrite

END MODULE ImplicitAsynch


PROGRAM C1232assumedShape04d
    USE ImplicitAsynch

    INTEGER, DIMENSION( 10 ) :: explicitShape


    OPEN(1221, ASYNCHRONOUS='yes')

    CALL Prepare2Write(1221, explicitShape)

    CLOSE( 1221 )


    CONTAINS


        SUBROUTINE Prepare2Write(ioUnit, assumeShape)
            INTEGER :: ioUnit
            INTEGER, DIMENSION( : ) :: assumeShape


            CALL AsynchWrite(ioUnit, assumeShape)

        END SUBROUTINE Prepare2Write

END PROGRAM C1232assumedShape04d
