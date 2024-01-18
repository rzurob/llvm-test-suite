!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AssumedType10d
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : June 13, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : type(*) cannot appear in type guard statement 
!*                               of a select type
!* 
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
program AssumedType10d
implicit none
class(*), allocatable :: i 

allocate ( integer(4) :: i) 

select type ( i )
    class is (TYPE(*))
        print*, "really ? How did I end up here? " 

    type is (TYPE(*))
        print*, "even better !?!"

    class default
        ERROR STOP 10
end select

end program AssumedType10d
