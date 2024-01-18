!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : actArgArraySectVectSub01d - ASYNCHRONOUS
!*                               Attribute in Array Section Arguments
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : April  6, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Array Section (with
!*                               a Vector Subscript)
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument Specification explicitly
!*                               includes the ASYNCHRONOUS Attribute
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
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM actArgArraySectVectSub01d

    interface
        integer function AsynchDump(ioUnit, dataArray)
            integer, intent(in) :: ioUnit
            real, asynchronous, dimension( 5,5 ) :: dataArray
        end function AsynchDump
    end interface

    real, dimension( 10,10 ) :: asynchArray


    open(1224, asynchronous='yes', form='unformatted',&
                    access='sequential', action='write')


    do i = 1, 10
        do j = 1, 10
            asynchArray( j,i ) = (i * 356) + (j * 91)
        end do
    end do


    iStat = AsynchDump(1223,&
                asynchArray( (/ 1, 3, 5, 7, 9 /),(/ 2, 4, 6, 8, 10 /) ))


    close( 1224 )

END PROGRAM actArgArraySectVectSub01d


integer function AsynchDump(ioUnit, dataArray)
    integer, intent(in) :: ioUnit
    real, asynchronous, dimension( 5,5 ) :: dataArray


    i = 0
    do while ((i < 5)  .AND. (iStat == 0))
        i = i + 1

        j = 0
        do while ((j < 5)  .AND. (iStat == 0))
            j = j + 1

            write(ioUnit, iostat=iStat) dataArray( i,j )
        end do
    end do

    AsynchDump = iStat

end function AsynchDump
