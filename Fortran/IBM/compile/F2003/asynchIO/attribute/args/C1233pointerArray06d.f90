!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : C1233pointerArray06d - ASYNCHRONOUS
!*                               Attribute with Pointer Array Actual Arguments
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : April  8, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is a Pointer Array
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument implicitly has the ASYNCHRONOUS
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

MODULE mExplicitShapeArray
    contains

        integer function ExplicitShapeArray(ioUnit, arrayExplicitInShape)
            integer, intent(in) :: ioUnit
            character(len = 17), dimension( 61 ) :: arrayExplicitInShape

            do i = 1, 61
                read(ioUnit, asynchronous='yes',&
                    id=ioID, iostat=iStat) arrayExplicitInShape( i )
            end do

            ExplicitShapeArray = iStat

        end function ExplicitShapeArray

END MODULE mExplicitShapeArray

PROGRAM C1233pointerArray06d
    use mExplicitShapeArray

    character(len = 17), dimension( : ), pointer :: charArrayPtr


    open(9, asynchronous='yes', access='stream',&
                form='unformatted', action='read')


    allocate( charArrayPtr( 183 ) )

    iStat = ExplicitShapeArray(9, charArrayPtr)

    if (iStat <> 0) then
        call zzrc( 9 )
    end if


    close( 9 )

END PROGRAM C1233pointerArray06d
