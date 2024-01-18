! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : realkind16.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : FROM/TO are of type real*16
!*                               ac_val of array constructor is allocatable 
!*                               function name 
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    integer i

    logical precision_r6

    real*16, allocatable ::  a(:), b(:)

    allocate(a(6), source = (/ ( real(func(i),16) , i= 1, 6)  /) )

    call move_alloc(a, b)

    if ( allocated(a) ) stop 21
    if ( .not. allocated(b) ) stop 23

    do i = 1, 6
      if ( .not. precision_r6(b(i), real(i+1, 16 ) ) ) error stop 10
    end do

    contains
           function func(i)
               integer i, func
               allocatable func
               allocate(func, source = i+1)  
           end function
  end
