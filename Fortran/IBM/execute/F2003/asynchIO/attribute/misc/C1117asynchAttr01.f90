!*  ===================================================================
!*
!*                               Attribute in Block Data Program Units
!*
!*  DATE                       : April 11, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Block Data Program Unit
!*  SECONDARY FUNCTIONS TESTED : Data Object explicitly has the ASYNCHRONUS
!*                               Attribute
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : BLOCK DATA, ASYNCHRONOUS Attribute
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  11.3 Block data program units
!*
!*  A block data program unit is used to provide initial values for data
!*  objects in named common blocks.
!*
!*  R1116 block-data           is  block-data-stmt
!*                                     [ specification-part ]
!*                                     end-block-data-stmt
!*  R1117 block-data-stmt      is  BLOCK DATA [ block-data-name ]
!*  R1118 end-block-data-stmt  is  END [ BLOCK DATA [ block-data-name ] ]
!*
!*  C1117 (R1116) A block-data specification-part shall contain only
!*        derived-type definitions and ASYNCHRONOUS, BIND, COMMON, DATA,
!*        DIMENSION, EQUIVALENCE, IMPLICIT, INTRINSIC, PARAMETER, POINTER,
!*        SAVE, TARGET, USE, VOLATILE, and type declaration statements.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program C1117asynchAttr01

    REAL :: realDataIn
    REAL :: nonAsynchDataObj

    COMPLEX :: complexDataIn
    COMPLEX :: asynchDataObj

    CHARACTER(len = 256) :: iMsg

    COMMON /commonBlock/ asynchDataObj, nonAsynchDataObj


    OPEN(255, ACTION='readwrite', ACCESS='stream', IOMSG=iMsg,&
            ASYNCHRONOUS='yes', FORM='unformatted', IOSTAT=iStat)
    if (iStat /= 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 1 )
    end if


    WRITE(255, IOMSG=iMsg, IOSTAT=iStat) nonAsynchDataObj
    if (iStat /= 0) then
        write(0, *) "WRITE(ASYNCHRONOUS=no) <", iStat, "> ", iMsg
        call zzrc( 2 )
    end if


    WRITE(255, ASYNCHRONOUS='yes',&
        ID=iID, IOMSG=iMsg, IOSTAT=iStat) asynchDataObj
    if (iStat /= 0) then
        write(0, *) "WRITE(ASYNCHRONOUS=yes) <", iStat, "> ", iMsg
        call zzrc( 3 )
    end if


    WAIT(255, ID=iID, IOMSG=iMsg, IOSTAT=iStat)
    if (iStat /= 0) then
        write(0, *) "WAIT(ID=", iID, ") <", iStat, "> ", iMsg
        call zzrc( 4 )
    end if


    REWIND(255, IOMSG=iMsg, IOSTAT=iStat)
    if (iStat /= 0) then
        write(0, *) "REWIND() <", iStat, "> ", iMsg
        call zzrc( 5 )
    end if


    READ(255, IOMSG=iMsg, IOSTAT=iStat) realDataIn, complexDataIn
    if (iStat /= 0) then
        write(0, *) "READ() <", iStat, "> ", iMsg
        call zzrc( 6 )
    end if


    if (realDataIn /= nonAsynchDataObj) then
        write(0, *) "      realDataIn = '", realDataIn, "'"
        write(0, *) "nonAsynchDataObj = '", nonAsynchDataObj, "'"
        call zzrc( 7 )

    else if (complexDataIn /= asynchDataObj) then
        write(0, *) "complexDataIn = '", complexDataIn, "'"
        write(0, *) "asynchDataObj = '", asynchDataObj, "'"
        call zzrc( 8 )
    end if


    CLOSE(255, IOMSG=iMsg, IOSTAT=iStat)
    if (iStat /= 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 9 )
    end if

end program C1117asynchAttr01


BLOCK DATA bdBlockDataUnit

    REAL :: nonAsynchDataObj
    COMPLEX, ASYNCHRONOUS :: asynchDataObj

    COMMON /commonBlock/ asynchDataObj, nonAsynchDataObj

    DATA nonAsynchDataObj   / 1.0 /
    DATA asynchDataObj      / (2.0,3.0) /

END BLOCK DATA bdBlockDataUnit
