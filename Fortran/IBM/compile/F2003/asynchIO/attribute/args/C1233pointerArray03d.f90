!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : C1233pointerArray03d - ASYNCHRONOUS
!*                               Attribute with Pointer Array Actual Arguments
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : April  7, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is a Pointer Array
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument explicitly has the ASYNCHRONOUS
!*                               Attribute, and is a Explicit Shape Array
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

PROGRAM C1233pointerArray03d

    INTERFACE
        SUBROUTINE ExplicitShapeArray(ioUnit, arrayShapeExplicit)
            integer :: ioUnit
            complex, asynchronous, dimension( 10 ) :: arrayShapeExplicit
        END SUBROUTINE ExplicitShapeArray
    END INTERFACE

    complex, pointer, dimension( : ) :: ptrComplexArray
    complex, target, dimension( 10 ) :: complexArray


    open(1226, asynchronous='yes', action='read',&
                access='stream', form='unformatted')


    do i = 1, 10
        read(ioUnit, asynchronous='yes') complexArray
    end do


    ptrComplexArray => complexArray
    call ExplicitShapeArray(1226, ptrComplexArray)


    close( 1226 )

END PROGRAM C1233pointerArray03d


SUBROUTINE ExplicitShapeArray(ioUnit, arrayShapeExplicit)
    integer :: ioUnit
    complex, asynchronous, dimension( 10 ) :: arrayShapeExplicit

    wait( ioUnit )

    do i = 0, 9
        write(6, '("(",F8.3,",",F8.3,")")') arrayShapeExplicit( i )
    end do

END SUBROUTINE ExplicitShapeArray
