! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : ulmt4allAssocname.f 
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
!*  DESCRIPTION                : 1.TO/FROM/pointer are all of type class(*)
!*                               2. TO is a component as a selector associated 
!*                                  with a associated name 
!*                               3. test association in select type block 
!*                               defect 322504      
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type base 
        class(*), allocatable :: a 
    end type

    type(base), target :: b
    class(*), target, allocatable :: c 
    class(*), pointer :: p

    allocate(integer :: b%a)
    allocate(base :: c)

    p => c

    call move_alloc(c, b%a) 

    select type ( x=> b%a )
        type is (base)   
            if ( .not. associated(p, x) ) stop 21
        class default 
            stop 23
    end select
    
    end
