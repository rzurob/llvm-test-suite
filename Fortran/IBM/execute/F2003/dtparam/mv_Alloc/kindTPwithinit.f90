! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : kindTPwithinit.f 
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
!*  DESCRIPTION                : The 2nd kind TP is initialized with value 
!*				 of 1st kind
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

	type A (k1,k2)
            integer, kind :: k1
            integer, kind :: k2=k1 
            integer(k1), allocatable :: a1
            integer(k2), allocatable :: a2
        end type


        type(A(2)), allocatable :: b1
        class(A(2,2)), allocatable :: b2

	allocate(b1, source=A(2,2)(25_2, 50_2))

        call move_alloc(b1, b2) 

        call move_alloc(b2%a1, b2%a2)

        if ( allocated(b1) ) stop 21
        if ( allocated(b2%a1) ) stop 31

        if ( b2%a2 /= 25 ) stop 41
end

