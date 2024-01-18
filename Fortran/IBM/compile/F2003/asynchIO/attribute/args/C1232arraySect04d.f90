!*  ===================================================================
!*
!*                               Attribute in Array Section Arguments
!*
!*  DATE                       : April  6, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Array Section
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument implicitly has the ASYNCHRONOUS
!*                               Attribute and is *NOT* an Assumed-Shape Array
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

module readSomeData

    contains

        subroutine readSumData(ioUnit, someData)
            integer, intent(in) :: ioUnit
            complex, dimension( 10 ) :: someData

            do i = 1, 10
                read(ioUnit, asynchronous='yes') someData( i )
            end do

        end subroutine readSumData
end module readSomeData


program C1232arraySect04d
    use readSomeData

    complex, dimension( 100000 ) ::  lotsOfData


    open(1221, access='stream', action='read',&
            asynchronous='yes', form='unformatted')


    do i = 1, 1000
        call readSumData(1221, lotsOfData( ((i * 100) + 1):((i * 100) + 100) ))
    end do


    close( 1221 )

end program C1232arraySect04d
