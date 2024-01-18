!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : C1233pointerArray05 - ASYNCHRONOUS
!*                               Attribute with Pointer Array Actual Arguments
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : April  8, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is a Pointer Array
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument implicitly has the ASYNCHRONOUS
!*                               Attribute, and is a Pointer Array
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASYNCHRONOUS Attribute, POINTER Attribute,
!*                               TARGET Attribute
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
!*  C1233 (R1221) If an actual argument is a pointer array, and the
!*        corresponding dummy argument has either the VOLATILE or ASYNCHRONOUS
!*        attribute, that dummy argument shall be an assumed-shape array or
!*        a pointer array.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program C1233pointerArray05

    real :: realValue
    real, pointer, dimension( : ) :: realArrayPtr


    open(1221, ACCESS='direct', FORM='unformatted',&
        RECL=4, ASYNCHRONOUS='yes', ACTION='readwrite')


    allocate( realArrayPtr( 10000 ) )

    realArrayPtr = (/ (((i * 1234.0) / 678.0), i = 1, 10000) /)


    call DataDump(1221, realArrayPtr)


    wait( 1221 )


    do i = 1, 10000
        read(1221, REC=i) realValue

        if (realValue <> realArrayPtr( i )) then
            write(0, *) "realValue = '", realValue,&
                        "', realArrayPtr(", i, ") = '",&
                                    realArrayPtr( i ), "'"
            call zzrc( 21 )
        end if
    end do


    close( 1221 )


    contains

        subroutine DataDump(theUnit, arrayPtr)
            integer :: theUnit
            real, pointer, dimension( : ) :: arrayPtr

            do i = 1, 10000
                write(theUnit, ASYNCHRONOUS='yes', REC=i) arrayPtr( i )
            end do

        end subroutine DataDump

end program C1233pointerArray05
