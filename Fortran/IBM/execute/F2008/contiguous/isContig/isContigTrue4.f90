! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : isContigTrue4.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 2010-09-29
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : - test intrinsic IS_CONTIGUOUS 
!*                               - actual arg is
!*                                   - array constructor 
!*                                   - intrinsic function call 
!*                                   - array with only 1 element 
!*                               
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod
    byte A1(1)

    type T
        byte, allocatable :: A2(:)
    end type

end module

program main

    use mod

    type(T) :: dT
    integer*4 :: A4(1)


    if ( is_contiguous(A1) .neqv. .true. ) then
         stop 5
    endif

    if ( is_contiguous( int(A4,1) ) .neqv. .true. ) then
        stop 6
    endif

    allocate(dT%A2(1))

    if ( is_contiguous(dT%A2) .neqv. .true. ) then
         stop 8
    endif

    if ( is_contiguous((/1_1,2_1,3_1,4_1/)) .neqv. .true. ) then
        stop 12
    endif

end program

