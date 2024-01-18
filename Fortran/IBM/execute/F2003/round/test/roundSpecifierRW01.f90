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
!*  PRIMARY FUNCTIONS TESTED   : ROUND specifier in READ/WRITE statement
!*                             
!*
!*  DESCRIPTION                : test character expression in data
!*                               transfer statement is array or 
!*                               character concatenation.
!* ===================================================================

  program roundSpecifierRW01 

    character*17 round(6)
    real r

    round = (/'UP               ', 'DOWN             ',           & 
              'ZERO             ', 'NEAREST          ',           & 
              'COMPATIBLE       ', 'PROCESSOR_DEFINED'/)

    open(11, file="tstIn.dat") 

    read(11, fmt='(f8.5)', round='DO'//'WN   ') r

    do i = 1, 6
      open(12, file="roundSpecifierRW01.out") 
      write(12, fmt='(f8.4)', round=round(i)) r      !<--using array
    end do

  end program roundSpecifierRW01 
