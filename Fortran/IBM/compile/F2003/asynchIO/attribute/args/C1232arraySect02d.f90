!*  ===================================================================
!*
!*                               Attribute in Array Section Arguments
!*
!*  DATE                       : April  6, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Array Section
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument explicitly has the ASYNCHRONOUS
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

program C1232arraySect02d

    logical :: done = .FALSE.

    real, dimension( 100 ) :: wholeArray


    open(1221, ASYNCHRONOUS='yes')


    do i = 1, 100
        read(1221, ASYNCHRONOUS='yes') wholeArray( i )
    end do


    close( 1221 )


    i = 0
    do while ((i < 10)  .AND. (.NOT. done))
        done = success( wholeArray( ((i * 10) + 1):((i * 10) + 10) ) )
        i = i + 1
    end do


    contains

        logical function success( arraySection )
            real, asynchronous, dimension( 10 ) :: arraySection

            success = .TRUE.

            do i = 1, 10
                write(*, '(F6.3)') arraySection( i )
                if (arraySecton( i ) > 0) then
                    success = .TRUE.
                end if
            end do

        end function success

end program C1232arraySect02d
