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
!*  DESCRIPTION                : test round with inquire by file and 
!*                               inquire by unit. 
!*    
!* ===================================================================

  program roundSpecifierInquire01 
    use ISO_FORTRAN_ENV
    implicit none
    character(:), allocatable:: r_mode(:)

    allocate(character*18::r_mode(6))

    !inquire by unit

    inquire(OUTPUT_UNIT, round=r_mode(1)) 
    if(r_mode(1) .ne. 'PROCESSOR_DEFINED') error stop 1_4

    open(20, form='formatted', access='direct', recl=20) 

    inquire(20, round=r_mode(2))

    if(r_mode(2) .ne. 'PROCESSOR_DEFINED') error stop 2_4

    ! inquire by file

    inquire(file='roundSpecifierInquire01.dat', round=r_mode(2))
    if(r_mode(2) .ne. 'UNDEFINED') error stop 3_4

    open(30, file='roundSpecifierInquire01.dat',form='formatted', access='stream')

    inquire(file='roundSpecifierInquire01.dat', round=r_mode(3))

    if(r_mode(3) .ne. 'PROCESSOR_DEFINED') error stop 4_4

    close(20)
    close(30)
  
  end program roundSpecifierInquire01 
