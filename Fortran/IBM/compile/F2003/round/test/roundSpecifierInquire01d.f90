!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 24/07/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND specifier= 
!*                             
!*
!*  DESCRIPTION                : diagnostic test for round with inquire 
!*    
!* ===================================================================

  program roundSpecifierInquire01d 
    use ISO_FORTRAN_ENV
    character(*), parameter:: r_mode(2) = (/"down", "zero"/)

    character(20) c(10)

    inquire(OUTPUT_UNIT, round="up")  !<---wrong

    inquire(OUTPUT_UNIT, round=r_mode(1)) !<--wrong

    inquire(10, round=c(5:6))  !<-- wrong

    inquire(20, round=c(1)//c(2)) !<--wrong

    inquire(30, round=getRoundMode(10)) !<--wrong

    contains

       character(:) function getRoundMode(i)
           allocatable :: getRoundMode 

           allocate (character(i) :: getRoundMode)
       end function

       subroutine illegal (x)
           class(*), intent(in) :: x

           select type (x)
               type is (character(*))
                   inquire (1, round=x)  !<-- wrong 

           end select
       end subroutine

  end program roundSpecifierInquire01d 
