! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : unpolyPtrSelector.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 2010-09-22
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : - CONTIGUOUS array pointer of type class(*)
!*                               - is slector of select type 
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

    type base
        integer x
    end type
 
end module

program main

    use mod

    integer, allocatable, target :: tar1(:)
    type(base), allocatable, target :: tar2(:)
    class(*), contiguous, pointer :: ptr(:)
    integer i

    allocate(tar1(20), source=(/(i+10,i=1,20)/))
    allocate(tar2(10), source=(/( base(tar1(i)),i=2,20,2)/))

    do i = 1, 2
        if ( i == 1 )  ptr => tar1
        if ( i == 2 )  ptr => tar2

        select type(ptr)
            type is (integer) 
                if ( any(ptr .ne. (/11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30/))) stop 11
            type is (base) 
                if ( any(ptr%x .ne. [12,14,16,18,20,22,24,26,28,30])) stop 21
        end select

    end do

end
