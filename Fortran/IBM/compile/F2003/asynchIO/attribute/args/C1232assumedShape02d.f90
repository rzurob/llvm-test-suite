!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : C1232assumedShape02d - ASYNCHRONOUS
!*                               Attribute in Assumed-Shape Array Arguments
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : April  7, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Assumed-Shape Array
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument explicitly has the ASYNCHRONOUS
!*                               Attribute, and is *NOT* an Assumed-Shape Array
!*
!*  DRIVER STANZA              : xlf2003
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

SUBROUTINE DumpData(toUnit, outData)
    integer :: toUnit
    integer, asynchronous, dimension( 100 ) :: outData


    do i = 0, 9
        write(toUnit, ASYNCHRONOUS='yes')&
            outData( (i * 10 + 1):(i * 10 + 10) )
    end do

END SUBROUTINE DumpData


PROGRAM C1232assumedShape02d

    INTERFACE
        SUBROUTINE MassageAndDump(toUnit, n, outData)
            integer :: toUnit
            integer :: n
            integer, dimension( : ) :: outData
        END SUBROUTINE MassageAndDump
    END INTERFACE

    integer, dimension( 1000 ) :: dataValues


    open(1221, ASYNCHRONOUS='yes', ACTION='write',&
                ACCESS='sequential', FORM='unformatted')


    call MassageAndDump(1221, 1000, dataValues)


    close( 1221 )

END PROGRAM C1232assumedShape02d


SUBROUTINE MassageAndDump(toUnit, n, outData)

    INTERFACE
        SUBROUTINE DumpData(toUnit, outData)
            integer :: toUnit
            integer, asynchronous, dimension( 100 ) :: outData
        END SUBROUTINE DumpData
    END INTERFACE

    integer :: toUnit
    integer :: n
    integer, dimension( : ) :: outData


    outData = outData + 1

    call DumpData(toUnit, outData)

END SUBROUTINE MassageAndDump
